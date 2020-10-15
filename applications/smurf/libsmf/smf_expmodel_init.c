/*
*+
*  Name:
*     smf_expmodel_init

*  Purpose:
*     Create an empty expanded model container

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     pntr = smf_expmodel_init( const smfData *indata, smfData **outdata,
*                               int *status)

*  Arguments:
*     indata = smfData* (Given)
*        Pointer to the input smfData containing a DIMM model
*     outdata = smfData** (Returned)
*        Container for pointer to newly created smfData, initialized to VAL__BADD
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     All model expansion routines require a container with the dimensions of
*     the full bolometer data cube, and the time-series WCS information. This
*     routine creates that container using information in the headers. Models
*     FITS headers contain several entries to help: SMFDIM* and SMFLBND* which
*     given the LBND and lengths along each axis respectively, They also contain
*     a SMFMODEL keyword giving the 3-character model type. These are all
*     deleted in the returned container to avoid confusing the expanded model
*     with a compressed model.

*  Notes:

*  Authors:
*     Edward Chapin (UBC)
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2010-05-27 (EC)
*        Initial version -- factored out of smf_expmodel_dks.c
*     2010-09-21 (COBA):
*        Add SMF__NOCREATE_FTS
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

#define FUNC_NAME "smf_expmodel_init"

void smf_expmodel_init( const smfData *indata, smfData **outdata, int *status) {

  /* Local Variables */
  double *d=NULL;               /* pointer to data array */
  smfData *data=NULL;           /* newly created smfData */
  dim_t i;                     /* Loop counter */
  smfHead *inhdr=NULL;          /* input smfHead pointer */
  int lbnd_c;
  int lbnd_r;
  int lbnd_t;
  dim_t ncol;                   /* Number of columns */
  int ncols;
  dim_t ndata;                  /* Total number of data points */
  smfHead *outhdr=NULL;         /* output smfHead pointer */
  dim_t nrow;                   /* Number of rows */
  int nrows;
  dim_t ntslice;                /* Number of time slices */
  int ntslices;
  AstFrameSet *tswcs=NULL;      /* time series wcs for expanded data cube */


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

  /* Create a deep copy of input data excluding the main data arrays
     or associated file information. Also replace the tswcs with one
     appropriate for a full time series cube. */

  data = smf_deepcopy_smfData( NULL, indata, 0, SMF__NOCREATE_DATA |
                               SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY |
                               SMF__NOCREATE_FILE | SMF__NOCREATE_DA |
                               SMF__NOCREATE_FTS, 0, 0,
                               status );

  if (!data) return;

  d = astMalloc( ndata*sizeof(*d) );
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
  smf_create_tswcs( outhdr, data->isTordered, &tswcs, status );

  if( outhdr->tswcs ) {
    outhdr->tswcs = astAnnul( outhdr->tswcs );
  }
  outhdr->tswcs = tswcs;

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
