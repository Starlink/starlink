/*
*+
*  Name:
*     ERR_FLUSH

*  Purpose:
*     Flush the current error context.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL ERR_FLUSH( STATUS )

*  Description:
*     Ensure that all pending error messages in the current error
*     context have been output to the user. On successful completion, the
*     error context is annulled and the status argument reset to SAI__OK;
*     if an error occurs during output of the error messages, the
*     error context is not anulled and the status argument is returned
*     set to ERR__OPTER.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status: it is set to SAI__OK on return if the
*        error message output is successful; if not, it is set to
*        ERR__OPTER.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1984, 1989-1991, 1994 Science & Engineering
*     Research Council. Copyright (C) 1997, 1999, 2001 Central Laboratory
*     of the Research Councils. All Rights Reserved.

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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry(JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-ARP-1983 (SLW):
*        Added MARK and RELEASE mods.
*     14-NOV-1984 (BDK):
*        Change name of ERR_PRINT.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     11-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     1-MAR-1990 (PCTR):
*        Converted to use EMS_ calls where possible, and changed the
*        behaviour of STATUS.
*     9-APR-1990 (PCTR):
*        Removed unreferenced declarations and replaced DO WHILE construct
*        with ANSI Fortran 77 equivalent.
*     6-JUN-1991 (PCTR):
*        Attempt to print all the pending messages regardless of
*        output errors.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*      3-AUG-1994 (AJC):
*        Flush ERR_FLUSH error message also
*     15-AUG-1997 (AJC):
*        Use NEQV to compare ERRBEL
*      7-SEP-1999 (AJC):
*        Avoid repetition of messages in 'reveal' mode
*     20-FEB-2001 (AJC):
*        EMS1_TUNE renamed EMS_TUNE
*        Use EMS_ELOAD not EMS1_ECOPY
*          (means have to add !'s here)
*        Allow for !'s in LINE length
*        Check for NOMSG at base level is not an error
*     28-JUL-2008 (TIMJ):
*        Call ERR1_FLUSH with NOBEL.
*     3-AUG-2008 (TIMJ):
*        Call errFlush. Now in C.
*     1-SEP-2008 (DSB):
*        Export *STATUS correctly.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(err_flush)( INTEGER(STATUS) ) {
  int status;
  errFlush( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
