/*
*+
*  Name:
*     smf_expmodel_two

*  Purpose:
*     Expand a TWO model into an ICD-ordered 3d data cube

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     smf_expmodel_two( const smfData *indata, smfData **outdata, int *status)

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
*     2010-06-08 (EC)
*        Initial version based on smf_expmodel_dks
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

#define FUNC_NAME "smf_expmodel_two"

void smf_expmodel_two( const smfData *indata, smfData **outdata, int *status) {
  /* Local Variables */
  dim_t bstride;               /* bolo stride */
  double *d=NULL;               /* pointer to data array */
  smfData *data=NULL;           /* newly created smfData */
  double *comp=NULL;            /* Pointer to current component */
  double *coeff=NULL;           /* Array of coeffs for all bolos in this col */
  dim_t i;                     /* Loop counter */
  dim_t j;                     /* Loop counter */
  dim_t k;                     /* Loop counter */
  dim_t nbolo;                  /* Number of bolometers */
  dim_t ntslice;                /* Number of time slices */
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
  smf_get_dims( data, NULL, NULL, &nbolo, &ntslice, NULL, &bstride, &tstride,
                status);

  /* Loop over component */
  for( i=0; (*status==SAI__OK)&&(i<2); i++ ) {

    /* get pointer to the component and coeff buffers */
    comp = indata->pntr[0];
    comp += i*(ntslice+nbolo);
    coeff = comp + ntslice;

    /* Loop over bolometer */
    for( j=0; (*status==SAI__OK)&&(j<nbolo); j++ ) {

      /* Scale the component into the expanded buffer */
      if( coeff[j] != VAL__BADD ) {
        for( k=0; k<ntslice; k++ ) {
          if( comp[k] != VAL__BADD) {
            if( d[j*bstride+k*tstride] == VAL__BADD ) {
              d[j*bstride+k*tstride] = comp[k]*coeff[j];
            } else {
              d[j*bstride+k*tstride] += comp[k]*coeff[j];
            }
          }
        }
      }
    }
  }

  /* Return pointer to new data */
  if( outdata ) *outdata = data;
}
