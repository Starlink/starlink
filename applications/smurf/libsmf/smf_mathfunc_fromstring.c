/*
*+
*  Name:
*     smf_mathfunc_fromstring

*  Purpose:
*     Return integer constant corresponding to string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     type = smf_mathfunc_fromstring( const char * type, int * status );

*  Arguments:
*     data = const char * type (Given)
*        String representation of the math function. Supported values:
*           - GAUSS
*           - GAUSSHERMITE1
*           - GAUSSHERMITE2
*           - VOIGT
*           - POLYNOMIAL
*           - HISTOGRAM
*     status = int* (Given and Returned)
*        Pointer to global status. Status is set to SMF__BDTYP if the
*        type is not recognized.

*  Return Value:
*     itype = smf_math_function
*        Internal numeric constant corresponding to the supplied function.
*        Returns SMF__NULL if type is unknown.

*  Description:
*     This function returns the internal integer constant corresponding
*     to the given string. The GAUSSHERMITE functions are checked first
*     and for remaining tests only the first four characters are tested.
*     (e.g. "Gaussian" will return SMF__MATH_GAUSS).

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-04-11 (TIMJ):
*        Initial version based on smf_dtype_fromstring

*  Notes:
*     - See also smf_mathfunc_str for the reverse.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
#include "ast.h"
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_mathfunc_fromstring"

smf_math_function
smf_mathfunc_fromstring( const char * mathfunc, int * status ) {
  smf_math_function itype = SMF__MATH_NULL;
  if (*status != SAI__OK) return itype;
  if (!mathfunc) return itype;

  /* GAUSSHERMITE1 and 2 need full test */
  if ( strcasecmp(mathfunc, "GAUSSHERMITE1" ) == 0 ) {
    itype = SMF__MATH_GAUSSHERMITE1;
  } else if ( strcasecmp(mathfunc, "GAUSSHERMITE2" ) == 0 ) {
    itype = SMF__MATH_GAUSSHERMITE2;
  } else if ( strncasecmp(mathfunc, "GAUS", 4 ) == 0 ) {
    itype = SMF__MATH_GAUSS;
  } else if ( strncasecmp(mathfunc, "VOIG", 4 ) == 0 ) {
    itype = SMF__MATH_VOIGT;
  } else if ( strncasecmp(mathfunc, "POLY", 4 ) == 0 ) {
    itype = SMF__MATH_POLYNOMIAL;
  } else if ( strncasecmp(mathfunc, "HIST", 4 ) == 0 ) {
    itype = SMF__MATH_HISTOGRAM;
  } else {
    if ( *status == SAI__OK) {
      *status = SMF__BDTYP;
      msgSetc( "F", mathfunc);
      errRep( FUNC_NAME, "Math function '^F', is not supported",status);
    }
  }

  return itype;
}
