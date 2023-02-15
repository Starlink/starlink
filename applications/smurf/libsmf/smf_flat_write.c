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
*     void smf_flat_write( ThrWorkForce *wf, smf_flatmeth flatmeth, const char * flatname,
*                          double refres, const smfData * bolval,
*                          const smfData * powref,const smfData * bolref,
*                          const smfData * polyfit, const Grp * prvgrp, int * status );

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     flatmeth = smf_flatmeth (Given)
*        Flatfield method (TABLE or POLYNOMIAL) used for powref and
*        bolref.
*     flatname = const char * (Given)
*        Name to be used for flatfield file.
*     refres = double (Given)
*        Reference resistor value used to calculate flatfield.
*     bolval = const smfData * (Given)
*        Merged flatfield bolometer data as calculated by smf_flat_fastflat
*        or smf_flat_mergedata.
*     powref = const smfData * (Given)
*        Heater power settings in pW. Must be same number of elements
*        as present in "heatframes".
*     bolref = const smfData * (Given)
*        Bolometer calibration values. Dimensioned as number of
*        number of bolometers times number of heat frames.
*     polyfit = const smfData * (Given)
*        Bolometer values derived from the polynomial fit. Can be NULL.
*        Will be written to flatname.MORE.SMURF.FLATFIT
*     prvgrp = const Grp * (Given)
*        Group of files contributing provenance to the output flatfield
*        file. Can be NULL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Write the flatfield measurements into the primary data array and
*     insert the flatfield results into the flatfield component of the NDF.

*  Notes:
*     - powval and bolval are calculated by smf_flat_standardpow. If POLYNOMIAL
*       method is used they are further processed by smf_flat_fitpoly.
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
*     2010-02-03 (TIMJ):
*        Add ability to write the polynomial expansion to an extension.
*     2010-03-08 (TIMJ):
*        Change API to use merged flatfield raw data so that we can share
*        code with smf_flat_fastflat and smf_flat_mergedata.
*     2011-09-08 (TIMJ):
*        Add refres argument.
*     2012-01-20 (TIMJ):
*        Protect pointer dereference
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008,2010-2012 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "smf_typ.h"
#include "smf.h"
#include "smurf_par.h"
#include "star/atl.h"
#include "star/kaplibs.h"
#include "star/one.h"
#include "star/thr.h"
#include "star/grp.h"
#include "prm_par.h"
#include "sae_par.h"
#include "par_par.h"
#include "mers.h"

#include "sc2da/sc2store.h"
#include "sc2da/sc2ast.h"


void smf_flat_write( ThrWorkForce *wf, smf_flatmeth flatmeth, const char * flatname,
                     double refres, const smfData * bolval,
                     const smfData * powref, const smfData * bolref,
                     const smfData * polyfit, const Grp * prvgrp, int * status ) {

  dim_t colsize;              /* number of columns */
  double *dbuf = NULL;         /* input double buffer for mean data */
  double *dvar = NULL;         /* input double buffer for variance of data */
  char fitsrec[SC2STORE__MAXFITS*SZFITSCARD+1]; /* Store for FITS records */
  int *ibuf = NULL;            /* int buffer for mean data */
  int indf = NDF__NOID;        /* NDF identifier for output file */
  dim_t ncards;               /* number of fits cards */
  dim_t numbols;              /* number of bolometers */
  double *outvar = NULL;       /* buffer for variance of data */
  int place = NDF__NOPL;       /* Dummy placeholder for NDF */
  dim_t rowsize;              /* number of rows */
  JCMTState *state = NULL;     /* State for this flatfield */
  sc2ast_subarray_t subnum;    /* subarray number */

  AstFrameSet *result, *spacefset;
  AstLutMap *heatmap;
  AstFrame *heatfrm;


  int *dksquid;           /* pointer to dummy dark SQUID data */
  dim_t j;               /* loop counter */
  int jig_vert[1][2];     /* dummy jiggle vertices */
  double jig_path[1][2];  /* dummy jiggle path */
  dim_t nframes = 0;     /* Number of frames in bolval */
  int npath = 0;          /* size of jiggle path */
  int nvert = 0;          /* number of jiggle vertices */
  char *xmlfile = NULL;   /* dummy xmlfile name */

  if (*status != SAI__OK) return;

  if (!bolval->da) {
    *status = SAI__ERROR;
    errRep( "", "No flatfield solution provided for writing",
            status );
    return;
  }

  if (!bolval->da->heatval) {
    *status = SAI__ERROR;
    errRep( "", "Must provide heater values in DA struct to smf_flat_write"
            " (possible programming error)", status );
    return;
  }

  /* note that colsize is the number of rows and rowsize is the number of
     columns */
  colsize = (bolval->dims)[SC2STORE__ROW_INDEX];
  rowsize = (bolval->dims)[SC2STORE__COL_INDEX];
  numbols = colsize * rowsize;
  nframes = (bolval->dims)[2];

  /* Make sure we have a FLAT header that reflects this file
     as the flatfield solution */
  smf_fits_updateS( bolval->hdr, "FLAT", flatname, "Name of flat-field file",
                    status );

  /* Create a FITS header for DA */
  smf_fits_export2DA( bolval->hdr->fitshdr, &ncards, fitsrec, status );

  /* Copy the data as integers so it can be written to data file. To
     prevent overflow in the variance we store that as doubles */

  ibuf = astMalloc( (numbols * nframes)*sizeof(*ibuf) );
  outvar = astMalloc( (numbols * nframes)*sizeof(*outvar) );

  dbuf = (bolval->pntr)[0];
  dvar = (bolval->pntr)[1];

  if (*status == SAI__OK) {
    for (j = 0; j < (nframes * numbols); j++) {
      /* These started off as integers so the mean value must fit in
         an integer */
      if ( dbuf[j] == VAL__BADD) {
        ibuf[j] = VAL__BADI;
      } else {
        ibuf[j] = (int)dbuf[j];
      }
      /* Same data type so no need to convert bad values */
      if (dvar) {
        outvar[j] = dvar[j];
      } else {
        outvar[j] = VAL__BADD;
      }
    }
  }

  /* get subarray number */
  smf_find_subarray( bolval->hdr, NULL, 0, &subnum, status );

  /* Create dummy components for output file */
  dksquid = astCalloc ( rowsize* nframes, sizeof(*dksquid) );
  jig_vert[0][0] = 0;
  jig_vert[0][1] = 0;
  jig_path[0][0] = 0.0;
  jig_path[0][1] = 0.0;

  sc2store_setcompflag ( SC2STORE__NONE, status );
  sc2store_wrtstream ( flatname, subnum, ncards,
                       fitsrec, colsize, rowsize, nframes,
                       (int) (bolref->dims)[2], refres, 0, smf_flat_methstring( flatmeth, status ),
                       bolval->hdr->allState, NULL,
                       ibuf, dksquid, (bolref->pntr)[0], (powref->pntr)[0],
                       "FLATCAL", NULL, NULL, jig_vert,
                       nvert, jig_path, npath, xmlfile, status );

  sc2store_free ( status );

  /* To copy in the variance and modify fix up the WCS we need to reopen
     the file */

  ndfOpen( NULL, flatname, "UPDATE", "OLD", &indf, &place, status );

  /* make sure that history is not written twice */
  ndfHsmod( "SKIP", indf, status );

  if (outvar) {
    void *pntr[3];
    size_t el;
    ndfStype( "_DOUBLE", indf, "VARIANCE", status );
    ndfMap( indf, "VARIANCE", "_DOUBLE", "WRITE", pntr, &el, status );
    if (*status == SAI__OK) {
      memcpy( pntr[0], outvar, sizeof(*outvar)*el );
    }
  }

  /* For the WCS a time frame is less relevant than heater settings */
  astBegin;

  /* Create frame for focal plane coordinates */
  sc2ast_createwcs( subnum, NULL, NULL, NULL, NO_FTS, &spacefset, status );

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
  heatmap = astLutMap( (int) nframes, bolval->da->heatval, 1.0, 1.0, " " );

  /* Append the heater axis to the spatial frameset */
  atlAddWcsAxis( result, (AstMapping *)heatmap, (AstFrame *) heatfrm,
                 NULL, NULL, status );

  /* write it to the NDF */
  ndfPtwcs( result, indf, status );


  /* Write provenance information */
  if (prvgrp) {
    dim_t size = grpGrpsz( prvgrp, status );
    char prvname[ 2 * PAR__SZNAM + 1];

    smf_get_taskname( NULL, prvname, status );
    for (j=1; j<=size; j++) {
      smf_accumulate_prov( NULL, prvgrp, j, indf, prvname, NULL, status );
    }

  }

  /* Write the polynomial expansion into an extension */
  if (polyfit) {
    char fitfile[GRP__SZNAM+1];
    int fndf = NDF__NOID;
    place = NDF__NOPL;
    one_strlcpy( fitfile, flatname, sizeof(fitfile), status );
    one_strlcat( fitfile, ".MORE.SMURF.FLATFIT", sizeof(fitfile), status );

    /* create the file */
    smf_write_smfData( wf, polyfit, NULL, fitfile, NULL, 0, NDF__NOID,
                       MSG__VERB, 0, NULL, NULL, status );

    /* Same WCS as the main file */
    ndfOpen( NULL, fitfile, "UPDATE", "OLD", &fndf, &place, status );
    ndfPtwcs( result, fndf, status );
    ndfAnnul( &fndf, status );

  }

  astEnd;

  ndfAnnul( &indf, status);

  if (ibuf) ibuf = astFree( ibuf );
  if (outvar) outvar = astFree( outvar );
  if (dksquid) dksquid = astFree( dksquid );
  if (state) state = astFree( state );
}
