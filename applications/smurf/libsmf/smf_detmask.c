/*
*+
*  Name:
*     smf_detmask

*  Purpose:
*     Mask out detectors.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_detmask( type, void *in, int len, int ndim, int *dims_in,
*                  int axis, int *index, int maxis, int *mask, void *out,
*                  int *status );

*  Arguments:
*     type = const char * (Given)
*        The HDS data type. Note, this argument is only included in the
*        interface for the generic "smf_detmask" function.
*     in = const <type> * (Given)
*        Point to the vectorised input array. The elements are assumed to be
*        stored in fortran order (i.e. the first axis varies fastest).
*     len = int (Given)
*        The length of each individual sub-string within the "in" and "out"
*        arrays. Only used for smf_detmaskB. The supplied value is ignored
*        otherwise, and a value of 1 is assumed.
*     ndim = int (Given)
*        Number of array axes in "in" and "out".
*     dims_in = const hdsdim * (Given)
*        Pointer to an array of "ndim" values, each being the length of
*        the corresponding dimension of the "in" array. The dimensions of
*        the "out" array should be the same as those of the "in" array,
*        except that the "maxis" axis should be shorter in the "out" array,
*        by the number of zero values in the mask.
*     maxis = int (Given)
*        The zero-based index of the dimension that is to be masked.
*     mask = const int * (Given)
*        An array with one element for each input pixel on the dimension that
*        is being masked (i.e. it should have "dims_in[maxis]" elements). This
*        array should contain non-zero values for those hyper-rows that
*        are to be copied to "out". Other hyper-rows are not copied. The
*        number of non-zero values in "mask" should equal the length of the
*        "maxis" axis in "out".
*     out = <type> * (Returned)
*        Point to the vectorised output array. The elements are assumed to be
*        stored in fortran order (i.e. the first axis varies fastest).
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function copies the input array to the output array, removing
*     selected hyper-planes from the input array in the process. This
*     results in the output array being smaller than the input array.
*
*     The hyper-planes removed are specified by an axis index and a 1D
*     mask array. This array holds one element for each pixel along the
*     specified axis of the input array. If an element is non-zero, the
*     corresponding hyper-plane is included in the output array.
*     Otherwise it is excluded.

*  Notes:
*     - <x> in the function name should be replaced by one of "r", "i",
*     "d" and "c" for processing arrays of float, int, double and char
*     respectively. <type> above should also be changed to one of the
*     "float", "int", "double" or "char".
*     - The smf_detmaskc function has an extra parameter "len" giving the
*     length of each sub-string within the array of chars gievn by "in"
*     and "out".

*  Authors:
*     David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     8-NOV-2007 (DSB):
*        Initial version.
*     31-MAR-2008 (DSB):
*        Added "mask" and "maxis" arguments.
*     3-APR-2008 (DSB):
*        Added generic smf_detmask function.
*     19-DEC-2008 (DSB):
*        Correct argument and description sections in the prologue.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2007-2008 Science & Technology Facilities Council.
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

#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "smf.h"

void smf_detmask( const char *type, const void *in, int len, int ndim,
                  const hdsdim *dims_in, int maxis, const int *mask,
                  void *out, int *status ){

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Call the correct function for the specified data type. */
   if( !strcmp( type, "_REAL" ) ) {
      smf_detmaskF( (const float *) in, 1, ndim, dims_in, maxis, mask,
                     (float *) out, status );

   } else if( !strcmp( type, "_DOUBLE" ) ) {
      smf_detmaskD( (const double *) in, 1, ndim, dims_in, maxis, mask,
                    (double *) out, status );

   } else if( !strcmp( type, "_INTEGER" ) ) {
      smf_detmaskI( (const int *) in, 1, ndim, dims_in, maxis, mask,
                    (int *) out, status );

   } else if( !strncmp( type, "_CHAR", 5 ) ) {
      smf_detmaskB( (const char *) in, len, ndim, dims_in, maxis, mask,
                    (char *) out, status );

   } else if( *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "TYPE", type );
      errRep( "", "SMF_DETMASK: ^TYPE data type not yet supported.", status );
   }
}

/* Include the generic implementations */

#include "prm_par.h"
#include "cgeneric.h"

#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE
#include "cgeneric_defs.h"
#include "smf_detmaskx.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE
#include "cgeneric_defs.h"
#include "smf_detmaskx.cgen"
#undef CGEN_CODE_TYPE

#define CGEN_CODE_TYPE CGEN_INT_TYPE
#include "cgeneric_defs.h"
#include "smf_detmaskx.cgen"
#undef CGEN_CODE_TYPE

/* Add special flag to be able to use generic code for string copying */
#define CGEN_CODE_TYPE CGEN_BYTE_TYPE
#include "cgeneric_defs.h"
#define IS_BYTE_TYPE
#include "smf_detmaskx.cgen"
#undef CGEN_CODE_TYPE
#undef IS_BYTE_TYPE
