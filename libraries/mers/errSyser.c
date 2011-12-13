/*
*+
*  Name:
*     errSyser

*  Purpose:
*     Assign an operating system error message to a token.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     errSyser( const char * token, int systat);

*  Description:
*     The text of the error message associated with the operating system
*     status value, "systat", is assigned to the named message token. This
*     token may then be included in an error message.

*  Arguments:
*     token = const char * (Given)
*        The message token name.
*     systat= int (Given)
*        The operating system status value.

*  System-specific:
*     The messages generated using this facility will depend on the
*     computer system upon which the library is implemented.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-OCT-1989 (PCTR):
*        Original version.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_SYSER.
*     19-JUL-2008 (TIMJ):
*        Rewrite in C to call emsSyser.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "ems.h"

void errSyser( const char * token, int systat) {
  emsSyser( token, systat );
}
