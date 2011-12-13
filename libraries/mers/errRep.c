/*
*+
*  Name:
*     errRep

*  Purpose:
*     Report an error message.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     errRep( const char * param, const char * text, int * status );

*  Description:
*     Report an error message. According to the error context, the
*     error message is either sent to the user or retained in the
*     error table. The latter case allows the application to take
*     further action before deciding if the user should receive the
*     message. On exit the values associated with any existing message
*     tokens are left undefined. On successful completion, the global
*     status is returned unchanged; if the status argument is set to
*     SAI__OK on entry, an error report to this effect is made on behalf
*     of the application and the status argument is returned set to
*     ERR__BADOK; the given message is still reported with an associated
*     status of ERR__UNSET.
*     If an output error occurs, the status argument is
*     returned set to ERR__OPTER. The status argument may also be returned
*     set to an EMS_ fault error value, indicating an error occuring
*     within the error reporting software.

*  Arguments:
*     param = const char * (Given)
*        The error message name.
*     text = const char * (Given)
*        The error message text.
*     status = int * (Given & Returned)
*        The global status: it is left unchanged on successful completion,
*        or is set an appropriate error value if an internal error has
*        occurred.

*  Algorithm:
*     -  Call err1Rep with formatting disabled.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989-1991, 1994 Science & Engineering
*     Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     17-APR-1983 (SLW):
*        Added MARK and RELEASE mods.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     13-DEC-1989 (PCTR):
*        Converted to use EMS_ calls.
*     19-MAR-1990 (PCTR):
*        Changed handling of status returned from ERR_FLUSH.
*     25-SEP-1990 (PCTR):
*        Changed call from EMS1_IELEV to EMS_LEVEL.
*     22-JAN-1991 (PCTR):
*        Removed default level behaviour (it now exists in EMS1_ESTOR).
*     10-JUN-1994 (AJC):
*        Associate ERR__BADOK with warning message and ERR__UNSET with
*        the given message if STATUS is given as SAI__OK.
*     15-SEP-1999 (AJC):
*        Add CLEAN argument to call MSG1_FORM
*     21-FEB-2001 (AJC):
*        Use EMS_REP not EMS1_ESTOR
*     31-JUL-2008 (TIMJ):
*        Use common accessor rather than COMMON directly.
*     09-SEP-2008 (TIMJ):
*        Rewrite in C
*     23-DEC-2008 (TIMJ):
*        Now call err1Rep.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "err_err.h"
#include "err_par.h"
#include "ems_err.h"
#include "star/util.h"

#include "ems.h"
#include "mers.h"
#include "mers1.h"

#include <stdarg.h>

void errRep( const char * param, const char * text, int * status ) {
  va_list args;
  err1Rep( param, text, 0, args, status);
}
