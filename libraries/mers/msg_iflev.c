/*
*+
*  Name:
*     MSG_IFLEV

*  Purpose:
*     Return the current filter level for conditional message output.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_IFLEV( FILTER, STRING, STATUS )

*  Description:
*     The value of the current filtering level set for conditional
*     message output is returned as both an integer and a string.

*  Arguments:
*     FILTER = INTEGER (Returned)
*        The current message filtering level.
*     STRING = CHAR (Returned)
*        String representation of the current message filtering
*        level. Should be at least MSG__SZLEV characters long. If it
*        is a single character ' ' it is assumed that the string
*        value is not required. If the buffer is smaller than MSG__SZLEV
*        then the return string will be truncated.
*     STATUS = INTEGER (Given & Returned)
*        Inherited Status.

*  Notes:
*     This subroutien can be called with a null Fortran string if the
*     string version of the message level is not required
*
*        CALL MSG_IFLEV( FILTER, ' ', STATUS )

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     10-JUN-1991 (PCTR):
*        Original version.
*     24-JUL-2008 (TIMJ):
*        Use Common block accessor
*     12-SEP-2008 (TIMJ):
*        Now in C. Calls msgIflev.
*     23-JUL-2009 (TIMJ):
*        Use new API.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_iflev)( INTEGER(FILTER), CHARACTER(STRING),
                           INTEGER(STATUS) TRAIL(STRING) ) {
  msglev_t filter;
  int status;
  char *string = NULL;

  F77_IMPORT_INTEGER( *STATUS, status );

  /* Decide whether we need to allocate a string buffer.
     Do not do so if the Fortran string is a single character. */
  if ( STRING_length > 1 ) {
    string = starMallocAtomic( MSG__SZLEV );
    string[0] = '\0';
  }

  filter = msgIflev( string, &status );

  if (string) {
    F77_EXPORT_CHARACTER( string, STRING, STRING_length );
    starFree( string );
  }
  F77_EXPORT_INTEGER( filter, *FILTER );
  F77_EXPORT_INTEGER( status, *STATUS );
}
