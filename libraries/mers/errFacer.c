/*
*+
*  Name:
*     errFacer

*  Purpose:
*     Assign the message associated with a Starlink STATUS value to a token.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     errFacer( const char * token, int * status )

*  Description:
*     The text of the message associated with the Starlink facility STATUS
*     value is assigned to the named message token. This token may then be
*     included in an error message.

*  Arguments:
*     token = const char * (Given)
*        The message token name.
*     status = int * (Given)
*        The facility status value. Note that this does not take a pointer
*        argument to status. See emsFacer.

*  System-specific:
*     The messages generated using this facility will depend on the
*     computer system upon which the library is implemented.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1994 Science & Engineering Research Council.
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
*     BKM: B.K. McIlwrath (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1994 (BKM):
*        Original version (Converted from ERR_SYSER)
*     23-JUL-2008 (TIMJ):
*        Now written in C to call emsSyser
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems.h"
#include "merswrap.h"

void errFacer (const char * token, int status ) {
  emsFacer( token, status );
}
