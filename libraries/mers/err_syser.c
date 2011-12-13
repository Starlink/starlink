/*
*+
*  Name:
*     ERR_SYSER

*  Purpose:
*     Assign an operating system error message to a token.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_SYSER( TOKEN, SYSTAT )

*  Description:
*     The text of the error message associated with the operating system
*     status value, SYSTAT, is assigned to the named message token. This
*     token may then be included in an error message.

*  Arguments:
*     TOKEN = CHARACTER * ( * ) (Given)
*        The message token name.
*     SYSTAT = INTEGER (Given)
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
*        Rewrite in C to call errSyser.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_syser)( CHARACTER(TOKEN), INTEGER(SYSTAT) TRAIL(TOKEN)) {
  int systat;
  char *token;

  GENPTR_CHARACTER(TOKEN);

  F77_IMPORT_INTEGER( *SYSTAT, systat );
  token = starMallocAtomic( TOKEN_length + 1 );
  F77_IMPORT_CHARACTER( TOKEN, TOKEN_length, token );

  errSyser( token, systat );
  starFree( token );
}
