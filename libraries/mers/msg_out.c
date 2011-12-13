/*
*+
*  Name:
*     MSG_OUT

*  Purpose:
*     Output a message.

*  Language:
*    Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_OUT( PARAM, TEXT, STATUS )

*  Description:
*     Any tokens in supplied message are expanded and the result is
*     output to the user. If the status argument is not set to SAI__OK
*     on entry, no action is taken except that the values of any
*     existing message tokens are always left undefined after a call to
*     MSG_OUT. If an output error occurs, an error is reported and the
*     status argument is returned set to MSG__OPTER.
*
*     A call to MSG_OUT is equivalent to a call to MSG_OUTIF with the
*     message output priority set to MSG__NORM.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The message name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The message text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Just kill tokens if STATUS is bad; otherwise call MSG_OUTIF
*     with priority normal.

*  Copyright:
*     Copyright (C) Science and Technology Facilities Council.
*     Copyright (C) 1983, 1984, 1988, 1991, 1992 Science & Engineering
*     Research Council.  Copyright (C) 1999, 2001 Central Laboratory
*     of the Research Councils.  All Rights Reserved.

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
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     12-NOV-1984 (BDK):
*        Remove call to error system and change name of output routine.
*     2-NOV-1988 (AJC):
*        Remove INCLUDE 'MSG_ERR'.
*     3-JUN-1991 (PCTR):
*        Changed to annul the token table regardless of given
*        status value.
*     26-AUG-1992 (PCTR):
*        Changed to call MSG_OUTIF with PRIOR set to MSG__NORM.
*     15-SEP-1999 (AJC):
*        Correct Algorithm above
*     22-FEB-2001 (AJC):
*        Replace EMS1_KTOK with MSG1_KTOK
*     10-SEP-2008 (TIMJ):
*        Call msgOut.
*     2010-09-23 (TIMJ):
*        cnfImpn requires the size without the trailing nul.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"
#include "merswrap.h"
#include "msg_par.h"
#include "err_par.h"

F77_SUBROUTINE(msg_out)( CHARACTER(PARAM),
                           CHARACTER(TEXT),
                           INTEGER(STATUS)
                           TRAIL(PARAM)
                           TRAIL(TEXT) ) {
  char param[ERR__SZPAR+1];
  char text[MSG__SZMSG+1];
  int status;

  cnfImpn( PARAM, PARAM_length, ERR__SZPAR, param );
  cnfImpn( TEXT, TEXT_length, MSG__SZMSG, text );

  F77_IMPORT_INTEGER( *STATUS, status );

  msgOut( param, text, &status );

  F77_EXPORT_INTEGER( status, *STATUS );

}
