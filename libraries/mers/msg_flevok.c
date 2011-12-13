/*
*+
*  Name:
*     MSG_FLEVOK

*  Purpose:
*     Return true if a message at the supplied level would be output

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     LOGICAL MSG_FLEVOK( FILTER, STATUS )

*  Description:
*     Return true if the supplied message level would result in output
*     from MSG_OUTIF or MSG_BLANKIF. This allows user-supplied functions to
*     be written that can execute depending on message filtering level.

*  Arguments:
*     FILTER = INTEGER (Given)
*        The message filtering level to compare with the internal value.
*     STATUS = INTEGER (Given & Returned)
*        Inherited global status.

*  Returned Value:
*     MSG_FLEVOK = LOGICAL
*        Returns .TRUE. if the message would be output, .FALSE. otherwise.

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-JUL-2009 (TIMJ):
*        Fortran Wrapper for msgFlevok
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_LOGICAL_FUNCTION(msg_flevok)( INTEGER(FILTER), INTEGER(STATUS) ) {
  msglev_t filter;
  int status;
  int retval;
  DECLARE_LOGICAL(RETVAL);

  F77_IMPORT_INTEGER( *FILTER, filter );
  F77_IMPORT_INTEGER( *STATUS, status );
  retval = msgFlevok( filter, &status );
  F77_EXPORT_INTEGER( status, *STATUS );
  F77_EXPORT_LOGICAL( retval, RETVAL );
  return RETVAL;
}
