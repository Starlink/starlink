/*
*+
*  Name:
*     smf_expmodel_dks

*  Purpose:
*     Expand a DKS model into an ICD-ordered 3d data cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     pntr = smf_expmodel_dks( const smfData *indata, smfData **outdata,
*                              int *status)

*  Arguments:
*     indata = smfData* (Given)
*        Pointer to the input smfData
*     outdata = smfData** (Returned)
*        Container for pointer to newly created smfData.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     Pointer to newly created smfData containing the expanded data.

*  Description:
*     Calculate the full time-series representation of a model. A new smfData
*     is created with the first two axes being bolometer coordinates, and the
*     third axis time.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2010-05-19 (EC)
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 University of British Columbia.
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

/* Starlink includes */
#include "mers.h"
#include "ndf.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "sc2da/sc2store.h"

#define FUNC_NAME "smf_expmodel_dks"

void smf_expmodel_dks( const smfData *indata, smfData **outdata,
                           int *status) {

  /* Local Variables */
  AstFrameSet *tswcs=NULL;      /* time series wcs for expanded data cube */
  size_t bolo;                  /* bolo number */
  size_t bstride;               /* bolo stride */
  double *d=NULL;               /* pointer to data array */
  smfData *data=NULL;           /* newly created smfData */
  double *dksquid=NULL;         /* Pointer to current dark squid */
  double *gainbuf=NULL;         /* Array of gains for all bolos in this col */
  size_t i;                     /* Loop counter */
  smfHead *inhdr=NULL;          /* input smfHead pointer */
  size_t j;                     /* Loop counter */
  size_t k;                     /* Loop counter */
  int lbnd_c;
  int lbnd_r;
  int lbnd_t;
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ncol;                   /* Number of columns */
  int ncols;
  dim_t ndata;                  /* Total number of data points */
  smfHead *outhdr=NULL;         /* output smfHead pointer */
  dim_t nrow;                   /* Number of rows */
  int nrows;
  dim_t ntslice;                /* Number of time slices */
  int ntslices;
  double *offsetbuf=NULL;       /* Array of offsets for all bolos in this col */
  size_t tstride;               /* time stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Get the dimensions of the expanded data */
  inhdr = indata->hdr;

  if( !inhdr ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": model container does not have a header.", status );
    return;
  }

  smf_getfitsi( inhdr, "SMFDIMR", &nrows, status );
  smf_getfitsi( inhdr, "SMFDIMC", &ncols, status );
  smf_getfitsi( inhdr, "SMFDIMT", &ntslices, status );
  smf_getfitsi( inhdr, "SMFLBNDR", &lbnd_r, status );
  smf_getfitsi( inhdr, "SMFLBNDC", &lbnd_c, status );
  smf_getfitsi( inhdr, "SMFLBNDT", &lbnd_t, status );

  nrow = (dim_t) nrows;
  ncol = (dim_t) ncols;
  ntslice = (dim_t) ntslices;
  ndata = nrow*ncol*ntslice;

  /* Create a deep copy of input data exluding the main data arrays or associated
     file information. Also replace the tswcs with one appropriate for a full
     time series cube. */

  data = smf_deepcopy_smfData( indata, 0, SMF__NOCREATE_DATA |
                               SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY |
                               SMF__NOCREATE_FILE, status );

  if (!data) return;

  d = smf_malloc( ndata, sizeof(*d), 0, status );
  if( *status == SAI__OK ) {
    for( i=0; i<ndata; i++ ) {
      d[i] = VAL__BADD;
    }
  }

  data->pntr[0] = d;
  data->ndims = 3;
  data->dims[SC2STORE__ROW_INDEX] = nrow;
  data->dims[SC2STORE__COL_INDEX] = ncol;
  data->dims[2] = ntslice;
  data->lbnd[SC2STORE__ROW_INDEX] = lbnd_r;
  data->lbnd[SC2STORE__COL_INDEX] = lbnd_c;
  data->lbnd[2] = lbnd_t;

  outhdr = data->hdr;
  smf_create_tswcs( outhdr, &tswcs, status );

  if( outhdr->tswcs ) {
    outhdr->tswcs = astAnnul( outhdr->tswcs );
  }
  outhdr->tswcs = tswcs;

  /* Figure out remaining dimensions information */
  smf_get_dims( data, NULL, NULL, &nbolo, NULL, NULL, &bstride, &tstride,status);

  /* Loop over column */
  for( i=0; (*status==SAI__OK)&&(i<ncol); i++ ) {

    /* get pointer to the dark squid and gain/offset buffers this column */
    dksquid = indata->pntr[0];
    dksquid += i*(ntslice+nrow*3);
    gainbuf = dksquid + ntslice;
    offsetbuf = gainbuf + nrow;

    /* Loop over rows */
    for( j=0; (*status==SAI__OK)&&(j<nrow); j++ ) {

      /* Calculate the bolo number */
      if( SC2STORE__COL_INDEX ) {
        bolo = i*nrow + j;
      } else {
        bolo = i + j*ncol;
      }

      /* Scale the dark squid into the expanded buffer*/
      if( (gainbuf[j] != VAL__BADD) && (offsetbuf[j] != VAL__BADD) ) {
        for( k=0; k<ntslice; k++ ) {
          if( dksquid[k] != VAL__BADD) {
            d[bolo*bstride+k*tstride] = dksquid[k]*gainbuf[j] + offsetbuf[j];
          }
        }
      }
    }
  }

  /* Once expanded these are no longer model files, so delete relevant FITS
     headers */
  if( (*status==SAI__OK) && (outhdr->fitshdr) ) {
    AstFitsChan *fits = outhdr->fitshdr;

    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFMODEL", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFLBNDR", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFDIMR", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFDIMC", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFDIMT", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFLBNDR", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFLBNDC", NULL, 0) ) astDelFits( fits );
    astClear( fits, "Card" );
    if( astFindFits(fits, "SMFLBNDT", NULL, 0) ) astDelFits( fits );
  }

  /* Return pointer to new data */
  if( outdata ) *outdata = data;
}
