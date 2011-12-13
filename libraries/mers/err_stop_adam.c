/*
*+
*  Name:
*     ERR_STOP

*  Purpose:
*     Close the Error Reporting System.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_STOP( STATUS )

*  Description:
*     Flush any messages pending output and return the Error Reporting
*     System to its initial state.

*  Arguments:
*     STATUS = INTEGER (Given)
*        The global status.

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  Algorithm:
*     -  If there are pending messages, then flush them.
*     -  If there are no pending messages, but STATUS is set, then
*     report a warning message and then flush it.
*     -  Clear the message token table.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1987, 1989-1991 Science & Engineering
*     Research Council. Copyright (C) 2001 Central Laboratory of the
*     Research Councils. All Rights Reserved.

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
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1983 (SLW):
*        Original version.
*     10-JUN-1987 (BDK):
*        Ensure return to startup state.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     12-JAN-1990 (PCTR):
*        Converted to use EMS_ calls.
*     31-JAN-1991 (PCTR):
*        Removed restart of EMS.
*     20-FEB-2001 (AJC):
*        Remove report on status set but no report (can't happen)
*         and avoids use of EMS internal EMS1_IEPND.
*     29-JUL-2008 (TIMJ):
*        Now C wrapper around errStop.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_stop)( INTEGER(STATUS) ) {
  int status;
  F77_IMPORT_INTEGER( *STATUS, status );
  errStop( &status );
}
