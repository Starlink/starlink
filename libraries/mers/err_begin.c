/*
*+
*  Name:
*     ERR_BEGIN

*  Purpose:
*     Create a new error reporting environment.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_BEGIN( STATUS )

*  Description:
*     Begin a new error reporting environment by marking a new error
*     reporting context and then resetting the status argument to SAI__OK.
*     If ERR_BEGIN is called with the status argument set to an error
*     value, a check is made to determine if there are any messages
*     pending output in the current context: if there are none, an
*     error report to this effect is made on behalf of the calling
*     application.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     15-JAN-1991 (PCTR):
*        Implemented status check.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being anulled
*        on error.
*     14-AUG-1991 (PCTR):
*        Changed to call EMS_BEGIN.
*     19-JUL-2008 (TIMJ):
*        Now in C and calls errBegin.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_begin)( INTEGER(STATUS) ) {
  int status;
  F77_IMPORT_INTEGER( *STATUS, status );
  errBegin( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
