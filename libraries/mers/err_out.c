/*
*+
*  Name:
*     ERR_OUT

*  Purpose:
*     Report an error message and deliver it to the user.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL ERR_OUT( PARAM, TEXT, STATUS )

*  Description:
*     The message is added to the error table at the current error context.
*     The contents of the error table at this context are then flushed, i.e.
*     output to the user. After output, the error table context is annulled,
*     and the values associated with any existing message tokens left
*     undefined.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Expression containing the message parameter name.
*     TEXT = CHARACTER * ( * ) (Given)
*        Expression containing the error message text.
*     STATUS = INTEGER (Given and Returned)
*        The global status. On normal completion Status is returned
*        set to SAI__OK.

*  Implementation Notes:
*     -  It exists purely to provide compatibility with the existing ERR_
*     library.
*     -  This subroutine is not documented and should not be used in
*     any new code.

*  Algorithm:
*     -  Use ERR_REP and then ERR_FLUSH.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1989, 1991, 1993 Science & Engineering
*     Research Council.  Copyright (C) 1996 Central Laboratory of the
*     Research Councils.  All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
*        Changed ERR_FLUSH call.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     31-MAY-1991 (PCTR):
*        Return SAI__OK regardless of the success of ERR_FLUSH.
*     14-MAY-1993 (PCTR):
*        Removed STATUS = SAI__OK assignment on exit so that errors on
*        message delivery can be detected by the application.
*     25-JAN-1996 (AJC):
*        Change argument ERROR to PARAM in code
*         and MSG to PARAM in prologue.
*     11-SEP-2008 (TIMJ):
*        Call errOut. Rewrite in C.
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

F77_SUBROUTINE(err_out)( CHARACTER(PARAM), CHARACTER(TEXT),
                         INTEGER(STATUS) TRAIL(PARAM) TRAIL(TEXT) ) {
  char param[ERR__SZPAR+1];
  char text[ERR__SZMSG+1];
  int status;

  GENPTR_CHARACTER(PARAM);
  GENPTR_CHARACTER(TEXT);

  cnfImpn( PARAM, PARAM_length, ERR__SZPAR, param );
  cnfImpn( TEXT, TEXT_length, ERR__SZMSG, text );
  F77_IMPORT_INTEGER( *STATUS, status );

  errOut( param, text, &status );

  F77_EXPORT_INTEGER( status, *STATUS );

}
