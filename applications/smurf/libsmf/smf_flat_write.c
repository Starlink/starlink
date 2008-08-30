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
*                          const double *powref, const double *bolref,
*                          int * status );

*  Arguments:
*     flatname = const char * (Given)
*        Name to be used for flatfield file.
*     heatframes = const smfArray* (Given)
*        Collection of heat frames.
*     powref = double [] (Given)
*        Heater power settings in pW. Must be same number of elements
*        as present in "heatframes".
*     bolref = double [] (Given)
*        Bolometer calibration values. Dimensioned as number of 
*        number of bolometers times number of heat frames.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Insert the vales into the flatfield component of the NDF.

*  Notes:
*     - powval and bolval are calculated by smf_flat_standardpow.
*     - uses sc2store to make a pseudo-time series containing the mean
*       heater settings. The flatfield values are stored in the extensions.

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
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science and Technology Facilities Council.
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

#include "sc2da/sc2store.h"

#include "prm_par.h"
#include "sae_par.h"

void smf_flat_write( const char * flatname, const smfArray * bbhtframes,
                     const double *powref, const double *bolref,
                     int * status ) {

  size_t colsize;              /* number of columns */
  double *dbuf = NULL;         /* input double buffer for mean data */
  double *dvar = NULL;         /* input double buffer for variance of data */
  char fitsrec[SC2STORE__MAXFITS*80+1]; /* Store for FITS records */
  int *ibuf = NULL;            /* int buffer for mean data */
  int *ivar = NULL;            /* int buffer for variance of data */
  smfData * frame = NULL;      /* single frame */
  size_t ncards;               /* number of fits cards */
  size_t numbols;              /* number of bolometers */
  size_t rowsize;              /* number of rows */
  JCMTState *state;            /* State for this flatfield */
  int subnum;                  /* subarray number */

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
  colsize = (frame->dims)[0];
  rowsize = (frame->dims)[1];
  numbols = colsize * rowsize;

  /* Create a FITS header for DA */
  smf_fits_export2DA( frame->hdr->fitshdr, &ncards, fitsrec, status );

  smf_dump_smfData( frame, 0, status );


  /* Copy the data as integers so it can be written to data file */

  ibuf = smf_malloc( numbols * bbhtframes->ndat, sizeof(*dbuf), 0, status );
  ivar = smf_malloc( numbols * bbhtframes->ndat, sizeof(*dbuf), 0, status );

  for (j = 0; j < bbhtframes->ndat; j++) {
    frame = (bbhtframes->sdata)[j];
    dbuf = (frame->pntr)[0];
    dvar = (frame->pntr)[1];
    for (i = 0; i < numbols; i++ ) {
      size_t index = j*numbols+i;
      if ( dbuf[i] == VAL__BADD) {
        ibuf[index] = VAL__BADI;
      } else {
        ibuf[index] = (int)dbuf[i];
      }
      if (dvar) {
        if ( dvar[i] == VAL__BADD || dvar[i] > (int)VAL__MAXI) {
          ivar[index] = VAL__BADI;
        } else {
          ivar[index] = (int)dvar[i];
        }
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
                       bbhtframes->ndat, "TABLE", state, NULL,
                       ibuf, dksquid, bolref, powref, "FLATCAL",
                       mcehead, mceheadsz, jig_vert,
                       nvert, jig_path, npath, xmlfile, status );

  sc2store_free ( status );

  if (ibuf) ibuf = smf_free( ibuf, status );
  if (ivar) ivar = smf_free( ivar, status );
  if (dksquid) dksquid = smf_free( dksquid, status );
  if (state) state = smf_free( state, status );
}
