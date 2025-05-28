/*
*+
*  Name:
*     one_strtod

*  Purpose:
*     Starlink compliant wrapper around the standard strtod function.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Description:
*     The strtod() function converts a string to a double precision
*     number. This function is the same except that there is not endptr
*     argument and it will set status to bad on failure and use inherited
*     status.

*  Invocation:
*     dval = one_strtod( const char * instr, int * status );

*  Arguments:
*     instr = const char * (Given)
*        Input string to parse. Must be nul-terminated standard
*        C string suitable for the strtod() library call.
*     status = int * (Given and Returned)
*        Inherited status. Will be set to ONE__CNVERR if the string
*        did not contain a number.

*  Returned Value:
*     double dval
*        Converted double precision value. VAL__BADD on error.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-05-12 (TIMJ):
*        Initial version.

*  Notes:
*     - This is for use from C only.
*     - Fortran D format is supported so 5.2D5 is converted to 5.2E5
*       before involving strtod.

*  See Also:
*     - CHR_CTOD subroutine.

*  Copyright:
*     Copyright (C) 2011 Science and Technology Facilities Council.
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

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include "ems.h"
#include "one.h"
#include "one_err.h"
#include "sae_par.h"
#include "prm_par.h"
#include "star/mem.h"

#include <stdlib.h>
#include <string.h>

double
one_strtod( const char * instr, int * status ) {
  size_t i = 0;
  char *endptr = NULL;
  size_t lenstr = 0;
  double retval = VAL__BADD;
  char *tempbuf = NULL;

  if (*status != SAI__OK) return retval;

  /* Trap null pointers - since strtod won't */
  if (!instr) {
    *status = SAI__ERROR;
    emsRep( " ", "one_strtod: Input string is a NULL pointer "
            "(possible programming error)", status);
  }

  /* Need to handle Fortran D notation so copy the string */
  lenstr = strlen(instr);
  tempbuf = starMallocAtomic( sizeof(*tempbuf) * (lenstr + 1) );
  if (!tempbuf) {
    *status = ONE__MALLOCERR;
    emsRep("", "Failed to allocate memory for copy of string in one_strtod",
           status );
    return retval;
  }

  one_strlcpy( tempbuf, instr, lenstr+1, status );
  if (*status == SAI__OK) {
    for (i = 0; i<lenstr; i++) {
      if (tempbuf[i] == 'd' || tempbuf[i] == 'D') {
        tempbuf[i] = 'E';
      }
    }
  }

  /* Now call the function */
  if (*status == SAI__OK) {
    retval = strtod( tempbuf, &endptr );
    if (retval == 0.0 && endptr == tempbuf) {
      retval = VAL__BADD;
      *status = ONE__CNVERR;
      emsRepf( " ", "Error converting '%s' to double", status, instr);
    }
  }

  if (tempbuf) starFree(tempbuf);

  return retval;
}
