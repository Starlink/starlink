/*
 *+
 *  Name:
 *     smf_scalar_multiply

 *  Purpose:
 *     Multiply a smfData by a scalar value

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SMURF subroutine

 *  Invocation:
 *     smf_scalar_multiply( smfData * data, double dscalar, int * status );

 *  Arguments:
 *     data = smfData * (Given and Returned)
 *        Data to modify.
 *     dscalar = double (Given)
 *        Multiplication factor. Currently only a double is supported.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Multiply a smfData by a scalar value, taking into account data type
 *     and variance.

 *  Notes:
 *     - Only supports _DOUBLE at present
 *     - could do with a routine for each operator
 *     - should consider multi-threading with a workforce argument
 *     - should consider a union argument along with a dtype to allow
 *       different types of input scalar.
 *     - should consider modifying smfData to include a flag that indicates
 *       whether it is expected to contain bad values.

 *  See Also:
 *     - smf_subtract_dark
 *

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2009-10-05 (TIMJ):
 *        Initial version

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

 *-
 */

#include "sae_par.h"
#include "prm_par.h"

#include "smf.h"


void
smf_scalar_multiply( smfData * data, double dscalar, int * status ) {

  double *ddpntr = NULL;    /* Pointer to double */
  double dvar;              /* Change in variance */
  double *dvpntr = NULL;    /* Pointer to variance */
  dim_t nelem = 0;          /* Number of elements in smfData */
  dim_t i = 0;


  if (*status != SAI__OK) return;
  if (!smf_validate_smfData( data, 0, 0, status)) return;
  if (!smf_dtype_check_fatal( data, NULL, SMF__DOUBLE, status )) return;

  smf_select_pntr( data->pntr, data->dtype, &ddpntr, &dvpntr, NULL, NULL,
                   status);
  smf_get_dims( data, NULL, NULL, NULL, NULL, &nelem, NULL, NULL, status);
  if (*status != SAI__OK) return;

  dvar = dscalar * dscalar;
  for ( i = 0; i < nelem; i++ ) {
    if (ddpntr[i] != VAL__BADD) ddpntr[i] *= dscalar;
    if (dvpntr && dvpntr[i] != VAL__BADD) dvpntr[i] *= dvar;
  }

  return;
}

