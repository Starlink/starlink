/*
*+
*  Name:
*     ERR_REP

*  Purpose:
*     Report an error message.

*  Language:
*     Starlink ANSI C (callable from Fortran)

*  Invocation:
*     CALL ERR_REP( PARAM, TEXT, STATUS )

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
*     PARAM = CHARACTER * ( * ) (Given)
*        The error message name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The error message text.
*     STATUS = INTEGER (Given)
*        The global status: it is left unchanged on successful completion,
*        or is set an appropriate error value if an internal error has
*        occurred.

*  Algorithm:
*     -  Use MSG1_FORM to create the complete message text.
*     -  Use EMS1_ESTOR to store the message in the error table.

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
*     2010-09-23 (TIMJ):
*        cnfImpn requires the size without the trailing nul.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"
#include "err_par.h"

F77_SUBROUTINE(err_rep)( CHARACTER(PARAM), CHARACTER(TEXT),
                         INTEGER(STATUS) TRAIL(PARAM) TRAIL(TEXT) ) {
  char param[ERR__SZPAR+1];
  char text[ERR__SZMSG+1];
  int status;

  GENPTR_CHARACTER(PARAM);
  GENPTR_CHARACTER(TEXT);

  cnfImpn( PARAM, PARAM_length, ERR__SZPAR, param );
  cnfImpn( TEXT, TEXT_length, ERR__SZMSG, text );
  F77_IMPORT_INTEGER( *STATUS, status );

  errRep( param, text, &status );

}
