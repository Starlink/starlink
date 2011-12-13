/*
*+
*  Name:
*     MSG_SYNC

*  Purpose:
*     Synchronise message output via the user interface.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL MSG_SYNC( STATUS )

*  Description:
*     This performs a synchronisation handshake with the user interface.
*     This is required if the current task has been outputting messages
*     via the user interface and now wants to use a graphics cursor on the
*     command device. If a synchronisation error occurs, then an error
*     report is made and the status value is returned set to MSG__SYNER.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status: it is returned set to MSG__SYNER on error.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of MSG_SYNC.
*     -  This subroutine makes calls to SUBPAR_SYNC.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1985, 1989-1991 Science & Engineering Research Council.
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
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original version.
*     20-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Included error reporting.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     28-JUL-2008 (TIMJ):
*        Now a C wrapper to msgSync
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"
#include "merswrap.h"

F77_SUBROUTINE(msg_sync)( INTEGER(STATUS ) ) {
  int status;
  F77_IMPORT_INTEGER( *STATUS, status );
  msgSync( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
