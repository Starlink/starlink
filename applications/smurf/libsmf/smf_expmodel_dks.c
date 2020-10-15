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
*     smf_expmodel_dks( const smfData *indata, smfData **outdata, int *status)

*  Arguments:
*     indata = smfData* (Given)
*        Pointer to the input smfData containing a DIMM model.
*     outdata = smfData** (Returned)
*        Container for pointer to newly created smfData.
*     status = int* (Given and Returned)
*        Pointer to global status.

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
*     2010-05-27 (EC)
*        Factored generation of empty container smfData into smf_expmodel_init
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

#define FUNC_NAME "smf_expmodel_dks"

void smf_expmodel_dks( const smfData *indata, smfData **outdata, int *status) {
  /* Local Variables */
  dim_t bolo;                  /* bolo number */
  dim_t bstride;               /* bolo stride */
  double *d=NULL;               /* pointer to data array */
  smfData *data=NULL;           /* newly created smfData */
  double *dksquid=NULL;         /* Pointer to current dark squid */
  double *gainbuf=NULL;         /* Array of gains for all bolos in this col */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
  dim_t k;                     /* Loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ncol;                   /* Number of columns */
  dim_t nrow;                   /* Number of rows */
  dim_t ntslice;                /* Number of time slices */
  double *offsetbuf=NULL;       /* Array of offsets for all bolos in this col */
  dim_t tstride;               /* time stride */

  /* Main routine */
  if (*status != SAI__OK) return;

  /* Create an empty container initialized to VAL__BADD */
  smf_expmodel_init( indata, &data, status );
  if( *status != SAI__OK ) return;

  if( !indata->pntr[0] || !data->pntr[0] ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": NULL DATA pointer encountered", status );
  }

  /* Pointer to output data buffer */
  d = data->pntr[0];

  /* Get array dimensions*/
  smf_get_dims( data, &nrow, &ncol, &nbolo, &ntslice, NULL, &bstride, &tstride,
                status);

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

  /* Return pointer to new data */
  if( outdata ) *outdata = data;
}
