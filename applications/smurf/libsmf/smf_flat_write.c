/*
*+
*  Name:
*     smf_flat_write

*  Purpose:
*     Write flat calibration to NDF

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     void smf_flat_write( const char * flatname, const smfArray * bbhtframes,
*                          const double heater[], const smfData * powref,
*                          const smfData * bolref, const Grp * prvgrp,
*                          int * status );

*  Arguments:
*     flatname = const char * (Given)
*        Name to be used for flatfield file.
*     heatframes = const smfArray* (Given)
*        Collection of heat frames.
*     heater = const double [] (Given)
*        Heater settings. One for each heatframe.
*     powref = const smfData * (Given)
*        Heater power settings in pW. Must be same number of elements
*        as present in "heatframes".
*     bolref = const smfData * (Given)
*        Bolometer calibration values. Dimensioned as number of 
*        number of bolometers times number of heat frames.
*     prvgrp = const Grp * (Given)
*        Group of files contributing provenance to the output flatfield
*        file. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Insert the vales into the flatfield component of the NDF.

*  Notes:
*     - powval and bolval are calculated by smf_flat_standardpow.
*     - uses sc2store to make a pseudo-time series containing the dark
*       subtracted and averaged flatfield data. The flatfield values themselves
*       are stored in the standard flatfield SCUBA2 extension.

*  Authors:
*     BDK: Dennis Kelly (UKATC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-08-23 (BDK):
*        Original version
*     2008-06-27 (BDK):
*        Use setcompflag to disable compression.
*     2008-06-16 (BDK):
*        Add NULL telpar argument to wrtstream call.
*     2008-08-27 (TIMJ):
*        Rewrite for SMURF from sc2flat.c
*     2008-09-02 (TIMJ):
*        Write out variance
*     2010-01-25 (TIMJ):
*        o Write out variance as a _DOUBLE to prevent numerical overflow
*        o Use atlAddWcsAxis to build up output frameset. And use BOLO frame
*          by default.
*     2010-01-28 (TIMJ):
*        Switch to a smfData API
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008,2010 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"
#include "star/atl.h"
#include "star/kaplibs.h"

#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"

#include "star/grp.h"
#include "prm_par.h"
#include "sae_par.h"
#include "par_par.h"

void smf_flat_write( const char * flatname, const smfArray * bbhtframes,
                     const double heater[], const smfData * powref,
                     const smfData * bolref, const Grp * prvgrp,
                     int * status ) {

  size_t colsize;              /* number of columns */
  double *dbuf = NULL;         /* input double buffer for mean data */
  double *dvar = NULL;         /* input double buffer for variance of data */
  char fitsrec[SC2STORE__MAXFITS*80+1]; /* Store for FITS records */
  int *ibuf = NULL;            /* int buffer for mean data */
  int indf = NDF__NOID;        /* NDF identifier for output file */
  smfData * frame = NULL;      /* single frame */
  size_t ncards;               /* number of fits cards */
  size_t numbols;              /* number of bolometers */
  double *outvar = NULL;       /* buffer for variance of data */
  int place = NDF__NOPL;       /* Dummy placeholder for NDF */
  size_t rowsize;              /* number of rows */
  JCMTState *state;            /* State for this flatfield */
  int subnum;                  /* subarray number */

  AstFrameSet *result, *spacefset;
  AstLutMap *heatmap;
  AstFrame *heatfrm;


  int *dksquid;           /* pointer to dummy dark SQUID data */
  size_t i;                  /* loop counter */
  size_t j;                  /* loop counter */
  int jig_vert[1][2];     /* dummy jiggle vertices */
  double jig_path[1][2];  /* dummy jiggle path */
  int *mcehead = NULL;    /* dummy mce header */
  size_t mceheadsz = 0;   /* dummy mce header size */
  int npath = 0;          /* size of jiggle path */
  int nvert = 0;          /* number of jiggle vertices */
  char *xmlfile = NULL;   /* dummy xmlfile name */

  if (*status != SAI__OK) return;

  frame = (bbhtframes->sdata)[0];

  /* note that colsize is the number of rows and rowsize is the number of
     columns */
  colsize = (frame->dims)[SC2STORE__ROW_INDEX];
  rowsize = (frame->dims)[SC2STORE__COL_INDEX];
  numbols = colsize * rowsize;

  /* Make sure we have a FLAT header that reflects this file
     as the flatfield solution */
  smf_fits_updateS( frame->hdr, "FLAT", flatname, "Name of flat-field file",
                    status );

  /* Create a FITS header for DA */
  smf_fits_export2DA( frame->hdr->fitshdr, &ncards, fitsrec, status );

  /* Copy the data as integers so it can be written to data file. To
     prevent overflow in the variance we store that as doubles */

  ibuf = smf_malloc( numbols * bbhtframes->ndat, sizeof(*ibuf), 0, status );
  outvar = smf_malloc( numbols * bbhtframes->ndat, sizeof(*outvar), 0, status );

  for (j = 0; j < bbhtframes->ndat; j++) {
    frame = (bbhtframes->sdata)[j];
    dbuf = (frame->pntr)[0];
    dvar = (frame->pntr)[1];
    for (i = 0; i < numbols; i++ ) {
      size_t index = j*numbols+i;
      /* These started off as integers so the mean value must fit in
         an integer */
      if ( dbuf[i] == VAL__BADD) {
        ibuf[index] = VAL__BADI;
      } else {
        ibuf[index] = (int)dbuf[i];
      }
      /* Same data type so no need to convert bad values */
      if (dvar) {
        outvar[index] = dvar[i];
      } else {
        outvar[index] = VAL__BADD;
      }
    }
  }

  /* Get the representative jcmtstate and sub array number */
  state = smf_malloc( bbhtframes->ndat, sizeof(*state), 0, status );
  for (i = 0; i < bbhtframes->ndat; i++) {
    frame = (bbhtframes->sdata)[i];
    memcpy( &(state[i]), &(frame->hdr->allState)[0], sizeof(*state) );
  }
  smf_find_subarray( frame->hdr, NULL, 0, &subnum, status );

  /* Create dummy components for output file */
  dksquid = smf_malloc ( rowsize* bbhtframes->ndat, sizeof(*dksquid),1,
                         status );
  jig_vert[0][0] = 0;
  jig_vert[0][1] = 0;
  jig_path[0][0] = 0.0;
  jig_path[0][1] = 0.0;

  sc2store_setcompflag ( 0, status );
  sc2store_wrtstream ( flatname, subnum, ncards,
                       fitsrec, colsize, rowsize, bbhtframes->ndat,
                       bbhtframes->ndat, 0, "TABLE", state, NULL,
                       ibuf, dksquid, (bolref->pntr)[0], (powref->pntr)[0],
                       "FLATCAL", mcehead, NULL, mceheadsz, jig_vert,
                       nvert, jig_path, npath, xmlfile, status );

  sc2store_free ( status );

  /* To copy in the variance and modify fix up the WCS we need to reopen
     the file */

  ndfOpen( NULL, flatname, "UPDATE", "OLD", &indf, &place, status );

  /* make sure that history is not written twice */
  ndfHsmod( "SKIP", indf, status );

  if (outvar) {
    void *pntr[3];
    int el;
    ndfStype( "_DOUBLE", indf, "VARIANCE", status );
    ndfMap( indf, "VARIANCE", "_DOUBLE", "WRITE", pntr, &el, status );
    if (*status == SAI__OK) {
      memcpy( pntr[0], outvar, sizeof(*outvar)*el );
    }
  }

  /* For the WCS a time frame is less relevant than heater settings */
  astBegin;

  /* Create frame for focal plane coordinates */
  sc2ast_createwcs( subnum, NULL, NULL, NULL, &spacefset, status );

  /* Copy it to make sure we do not mess with the cache */
  result = astCopy( spacefset );

  /* and switch to BOLO frame which is best for bolometer analysis */
  {
    int frnum = AST__NOFRAME;
    kpg1Asffr( result, "BOLO", &frnum, status );
    if (frnum != AST__NOFRAME) astSetI( result, "CURRENT", frnum );
  }

  /* Create a simple frame for heater settings */
  heatfrm = astFrame( 1, "Domain=HEATER,Label(1)=Heater Setting" );
  heatmap = astLutMap( bbhtframes->ndat, heater, 1.0, 1.0, " " );

  /* Append the heater axis to the spatial frameset */
  atlAddWcsAxis( result, (AstMapping *)heatmap, (AstFrame *) heatfrm,
                 NULL, NULL, status );

  /* write it to the NDF */
  ndfPtwcs( result, indf, status );

  astEnd;

  /* Write provenance information */
  if (prvgrp) {
    size_t size = grpGrpsz( prvgrp, status );
    char prvname[ 2 * PAR__SZNAM + 1];

    smf_get_taskname( NULL, prvname, status );
    for (j=1; j<=size; j++) {
      smf_accumulate_prov( NULL, prvgrp, j, indf, prvname, status );
    }

  }


  ndfAnnul( &indf, status);

  if (ibuf) ibuf = smf_free( ibuf, status );
  if (outvar) outvar = smf_free( outvar, status );
  if (dksquid) dksquid = smf_free( dksquid, status );
  if (state) state = smf_free( state, status );
}
