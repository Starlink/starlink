/*
*+
*  Name:
*     ERR_RLSE

*  Purpose:
*     Release (end) the current error context.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_RLSE

*  Description:
*     Release a "mark" in the error message table, returning the Error
*     Reporting System to the previous error context. Any error messages
*     pending output will be passed to this previous context, not
*     annulled.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 1983, 1988-1991 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     AJC: Alan Chipperfield (STARLINK)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-APR-1983 (SLW):
*        Original version.
*     2-DEC-1988 (AJC):
*        Retain unflushed messages from above the mark.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     2-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     15-DEC-1989 (PCTR):
*        Converted to call EMS_RLSE.
*     16-MAR-1990 (PCTR):
*        Added trap to stop ADAM version returning to the lowest context.
*     26-SEP-1990 (PCTR):
*        Changed call from EMS_$IELEV to EMS_LEVEL.
*     22-JAN-1991 (PCTR):
*        Changed to call only EMS_RLSE.
*     19-JUL-2008 (TIMJ):
*        Rewrite in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_rlse)( void ) {
  errRlse();
}
