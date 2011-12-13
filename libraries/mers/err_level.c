/*
*+
*  Name:
*     ERR_LEVEL

*  Purpose:
*     Inquire the current error context level.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_LEVEL( LEVEL )

*  Description:
*     Return the number of context markers set in the error message table.

*  Arguments:
*     LEVEL = INTEGER (Returned)
*        The error context level: all values greater than one indicate
*        the deferral of reported error messages.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     9-OCT-1990 (PCTR):
*        Original version.
*     23-JUL-2008 (TIMJ):
*        Now a C wrapper.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_level)( INTEGER(LEVEL) ) {
  int level;
  errLevel( &level );
  F77_EXPORT_INTEGER( level, *LEVEL );
}
