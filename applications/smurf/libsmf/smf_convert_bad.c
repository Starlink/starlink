/*
*+
*  Name:
*     smf_convert_bad

*  Purpose:
*     Convert inf/NaN values to VAL__BADD in smfData

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_convert_bad( smfData *data, int *status );

*  Arguments:
*     data = smfData* (Given)
*        Pointer to double precision smfData to be updated
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine checks DATA and VARIANCE components for inf/NaN and changes
*     them to VAL__BADD so that they can be recognized by SMURF

*  Notes:

*  Authors:
*     Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-11-03 (EC):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     University of British Columbia. All Rights Reserved.

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
#include "sae_par.h"
#include "star/ndg.h"
#include "prm_par.h"
#include "par_par.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"

/* Other includes */

#define FUNC_NAME "smf_convert_bad"

void smf_convert_bad( smfData *data, int *status ) {
  dim_t i;                      /* loop counter */
  dim_t j;                      /* loop counter */
  dim_t ndata;                  /* Number of data points */
  double *val=NULL;             /* Pointer to data */

  if ( *status != SAI__OK ) return;

  /* Check for NULL pointer */
  if( !data ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": smfData pointer is NULL", status );
    return;
  }

  /* Check for double-precision data */
  if( data->dtype != SMF__DOUBLE ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Data is not double precision.",
            status );
    return;
  }

  /* Data dimensions */
  ndata = 1;
  for( i=0; i<data->ndims; i++ ) ndata *= data->dims[i];

  /* Convert +/-inf or NaN to VAL__BADD */
  for( i=0; i<2; i++ ) {
    val = data->pntr[i];
    if( val ) for( j=0; j<ndata; j++ ) {
        if( !isfinite(*val) ) *val = VAL__BADD;
        val++;
      }
  }
}
