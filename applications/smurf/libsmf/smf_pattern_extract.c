/*
*+
*  Name:
*     smf_pattern_extract

*  Purpose:
*     Find pattern in string

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_pattern_extract ( const char * sourcestr, const char * pattern,
*                  double *dresult, char * sresult, dim_t szstr, int * status );

*  Arguments:
*     sourcestr = const char * (Given)
*        String to be searched.
*     pattern = const char * (Given)
*        Regular expression to use. Should be understood by astChrSplitRE and
*        include one set of capture parentheses.
*     dresult = double * (Returned)
*        If non-null the result from the pattern match will be converted to
*        a double and returned.
*     sresult = char * (Returned)
*        If non-null the result from the pattern match will be copied to
*        this buffer.
*     szstr = dim_t (Given)
*        Allocated size of sresult buffer.
*     status = int * (Given & Returned)
*        Pointer to global status

*  Returned Value:
*     Returns true if we found something of false if there was no match.

*  Description:
*     Wrapper around astChrSplitRE to copy the result of a pattern match into
*     a double or into a string buffer.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)

*  Notes:
*     Pattern should only match one result.

*  History:
*     2009-11-27 (TIMJ):
*        Split from smf_fix_metadata
*     2011-05-12 (TIMJ):
*        Use one_strtod

*  Copyright:
*     Copyright (C) 2009,2011 Science & Technology Facilities Council.
*     Copyright (C) 2009 University of British Columbia.
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

#include "sae_par.h"
#include "ast.h"
#include "star/one.h"
#include "prm_par.h"
#include "mers.h"

#include "smf.h"

#include <string.h>
#include <stdlib.h>

int smf_pattern_extract ( const char * sourcestr, const char * pattern,
                          double *dresult, char * sresult, dim_t szstr, int * status ) {

  int i;
  int retval = 0;
  char ** result = NULL;
  int n;

  /* initialise */
  if (dresult) *dresult = VAL__BADD;
  if (sresult) sresult[0] = '\0';

  if (*status != SAI__OK) return retval;

  result = astChrSplitRE( sourcestr, pattern, &n, NULL );
  if (n == 1) {
    retval = 1;
    /* we have a match */

    /* Now need to convert it to a float if required. We trap for bad conversion. */
    if ( dresult ) {
      *dresult = one_strtod( result[0], status );
    }
    /* Copy to results buffer if required */
    if ( sresult ) {
      one_strlcpy( sresult, result[0], szstr, status );
    }
  }
  for (i = 0; i < n; i++) {
    (void)astFree( result[i] );
  }
  if (result) astFree( result );
  return retval;
}
