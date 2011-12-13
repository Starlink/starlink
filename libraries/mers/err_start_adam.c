/*
*+
*  Name:
*     ERR_START

*  Purpose:
*     Initialise the Error Reporting System.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL ERR_START

*  Description:
*     Initialises the global variables used by the Error Reporting System.

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989-1991 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK,RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     23-FEB-1983 (SLW):
*        Original version.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     12-JAN-1990 (PCTR):
*        Converted to use EMS_ calls.
*     4-JUN-1991 (PCTR):
*        Added call to EMS1_MSTRT.
*     16-FEB-2001 (AJC):
*        Avoid use of EMS internals
*     29-JUL-2008 (TIMJ):
*        Call errStart.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "f77.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_start)( void ) {
  errStart();
}
