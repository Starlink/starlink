/*
*+
*  Name:
*     emsSetv

*  Purpose:
*    Assign a formatted value to a message token (formatted).

*  Language:
*     Starlink ANSI C

*  Invocation:
*     emsSetv( const char * token, const char * format, va_list args );

*  Description:
*     A given value is encoded using the supplied format field and the
*     result assigned to the named message token. If the token is already
*     defined, the result is appended to the existing token value. If this
*     subroutine fails the token remains unmodified. This will be apparent in
*     any messages which refer to this token.
*
*     If the token contents exceeds EMS__SZTOK characters after formatting
*     the token will be truncated.

*  Arguments:
*     token = const char * (Given)
*        The message token name.
*     format = const char * (Given)
*        The ANSI C sprintf format specifier used to encode the supplied value.
*        This format string must have the same number of specifiers as there
*        are variables supplied afterwards.
*     args = va_list (Given)
*        The variable(s) to be formatted.

*  Algorithm:
*     -  Convert the value into a string.
*     -  Use EMS_SETC to set the message token.

*  See Also:
*     emsSet() can be called with variable argument list, whereas this
*     routine requires a va_list.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989, 1990 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     13-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-DEC-1989 (PCTR):
*        EMS_ version adapted from MSG_FMTD.
*     9-APR-1990 (PCTR):
*        Converted to Standard Fortran 77 CHARACTER concatenation.
*     15-FEB-2001 (AJC):
*        Renamed to EMS_FMTx to MSG_FMTx
*     22-DEC-2008 (TIMJ):
*        va_list form, copied from msgFmt.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems.h"
#include "ems_par.h"

#include <stdarg.h>
#include <stdio.h>

void
emsSetv( const char * token, const char *format, va_list args ) {

  char str[EMS__SZTOK+1];  /* somewhere to store the expanded string */

  /* format the va_list into a string buffer */
  vsnprintf( str, sizeof(str), format, args );

  /* set the token */
  emsSetc( token, str );
}
