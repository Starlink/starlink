/*
*+
*  Name:
*     smf_mathfunc_str

*  Purpose:
*     Return string representation of math fitting function

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     const char * smf_mathfunc_str( smf_math_function obstype, int * status );

*  Arguments:
*     obstype = smf_math_function (Given)
*        Math function code to convert to string.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Return Value:
*     const char* = Pointer to constant string corresponding to obs type.
*                   Can be NULL pointer if type unknown.

*  Description:
*     This function returns a string representation of the underlying
*     math function. Returns NULL pointer if the data type is not recognized.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-04-11 (TIMJ):
*        Initial version, copied from smf_obstype_str.

*  Notes:
*     - See also smf_mathfunc_fromstring for the reverse.

*  Copyright:
*     Copyright (C) 2008-2012 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
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

/* System includes */
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_mathfunc_str"

const char * smf_mathfunc_str( smf_math_function type, int * status ) {

  /* Set a default value */
  const char * retval = NULL;

  /* Check entry status */
  if (*status != SAI__OK) return retval;

  /* now switch on data type */
  switch( type ) {
  case SMF__MATH_NULL:
    retval = NULL;
    break;
  case SMF__MATH_GAUSS:
    retval = "GAUSSIAN";
    break;
  case SMF__MATH_GAUSSHERMITE1:
    retval = "GAUSSHERMITE1";
    break;
  case SMF__MATH_GAUSSHERMITE2:
    retval = "GAUSSHERMITE2";
    break;
  case SMF__MATH_VOIGT:
    retval = "VOIGT";
    break;
  case SMF__MATH_POLYNOMIAL:
    retval = "POLYNOMIAL";
    break;
  case SMF__MATH_HISTOGRAM:
    retval = "HISTOGRAM";
    break;
  }

  return retval;
}
