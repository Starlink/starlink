/*
*+
*  Name:
*     ERR_END

*  Purpose:
*     End the current error reporting environment.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_END( STATUS )

*  Description:
*     Check if any error messages are pending output in the previous
*     error reporting context. If there are, the current context is
*     annulled and then released; if not, the current context is just
*     released. The last reported status value is returned on exit.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     26-SEP-1990 (PCTR):
*        Original version.
*     15-JAN-1990 (PCTR):
*        Changed to call EMS_END.
*     19-JUL-2008 (TIMJ):
*        Now in C and calls errEnd.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_end)( INTEGER(STATUS) ) {
  int status;
  errEnd( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
