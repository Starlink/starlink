/*
*+
*  Name:
*     MSG_LOAD

*  Purpose:
*     Expand and return a message.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG_LOAD( PARAM, TEXT, OPSTR, OPLEN, STATUS )

*  Description:
*     Any tokens in the supplied message are expanded and the result is
*     returned in the character variable supplied. If the status
*     argument is not set to SAI__OK on entry, no action is taken
*     except that the values of any existing message tokens are always
*     left undefined after a call to MSG_LOAD. If the expanded message
*     is longer than the length of the supplied character variable,
*     the message is terminated with an ellipsis.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The message name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The raw message text.
*     OPSTR = CHARACTER * ( * ) (Returned)
*        The expanded message text.
*     OPLEN = INTEGER (Returned)
*        The length of the expanded message.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Use MSG1_FORM to construct the output message text.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1983, 1984, 1988, 1989, 1991 Science & Engineering
*     Research Council. Copyright (C) 1995, 1998, 1999, 2001 Central
*     Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
*     28-NOV-1989 (PCTR):
*        MSG_MLOAD adapted from MSG_OUT.
*     15-DEC-1989 (PCTR):
*        Changed name to MSG_LOAD, and converted to use EMS_ calls.
*     3-JUN-1991 (PCTR):
*        Changed to annul the token table regardless of given
*        status value.
*     9-NOV-1995 (AJC):
*        Remove use of local buffer and hence length restiction.
*        Rely on lower levels for ellipsis.
*     12-NOV-1998 (AJC):
*        Remove unused variables
*     15-SEP-1999 (AJC):
*        Add CLEAN argument to call MSG1_FORM
*     22-FEB-2001 (AJC):
*        Use MSG1_KTOK not EMS1_KTOK
*     24-JUL-2008 (TIMJ):
*        Use common block getter
*     10-SEP-2008 (TIMJ):
*        Call msgLoad
*     4-MAR-2009 (TIMJ):
*        Do not import Fortran strings if input status is bad. Stopping this
*        prevents possible warnings from valgrind in cnfImpn.
*     2010-09-23 (TIMJ):
*        cnfImpn requires the size without the trailing nul.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "sae_par.h"
#include "merswrap.h"
#include "mers_f77.h"
#include "err_par.h"
#include "msg_par.h"


F77_SUBROUTINE(msg_load)( CHARACTER(PARAM),
                          CHARACTER(TEXT),
                          CHARACTER(OPSTR),
                          INTEGER(OPLEN),
                          INTEGER(STATUS)
                          TRAIL(PARAM)
                          TRAIL(TEXT)
                          TRAIL(OPSTR) ) {

  char param[ERR__SZPAR+1];
  char text[MSG__SZMSG+1];
  char * opstr;
  int oplen = 0;
  int status;

  F77_IMPORT_INTEGER( *STATUS, status );
  opstr = starMallocAtomic( OPSTR_length + 1 );
  if (opstr) {
    opstr[0] = '\0';
  } else {
    OPSTR_length = 0;
  }

  if (status == SAI__OK) {
    cnfImpn( PARAM, PARAM_length, ERR__SZPAR, param );
    cnfImpn( TEXT, TEXT_length, MSG__SZMSG, text );
  } else {
    /* blank strings if status is bad */
    param[0] = '\0';
    text[0] = '\0';
  }

  /* Still call msgLoad so that tokens can be killed */
  msgLoad( param, text, opstr, OPSTR_length, &oplen, &status );

  F77_EXPORT_INTEGER( status, *STATUS );
  F77_EXPORT_INTEGER( oplen, *OPLEN );
  F77_EXPORT_CHARACTER( opstr, OPSTR, OPSTR_length );
  starFree( opstr );

}
