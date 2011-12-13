/*
*+
*  Name:
*     MSG_BELL

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     CALL MSG_BELL( STATUS )

*  Description:
*     A bell character and a new line is delivered to the user. If the
*     user interface in use supports the ASCII BEL character, this routine
*     will ring a bell and print a new line on the terminal.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     1-OCT-1993 (PCTR):
*        Original version.
*     23-JUL-2008 (TIMJ):
*        Now written in C to call msgBell
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_bell)( INTEGER(STATUS) ) {
  int status;
  F77_IMPORT_INTEGER( *STATUS, status );
  msgBell( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
