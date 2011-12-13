/*
*+
*  Name:
*     errStop

*  Purpose:
*     Close the Error Reporting System.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     errStop( int * status );

*  Description:
*     Flush any messages pending output and return the Error Reporting
*     System to its initial state.

*  Arguments:
*     status = int * (Given)
*        The global status. Not used.

*  Implementation Notes:
*     This subroutine is for use only with the ADAM implementation of
*     the Error Reporting System.

*  Algorithm:
*     -  call errClear

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
*        Rewrite in C. Remove old commented out code.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "ems.h"
#include "sae_par.h"

void errStop( int * status __attribute__((unused)) ) {
  int istat = SAI__OK;    /* Local status */

  /*     Call ERR_CLEAR to clear the error message table. */
  errClear( &istat );
}
