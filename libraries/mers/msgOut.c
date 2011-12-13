/*
*+
*  Name:
*     msgOut

*  Purpose:
*     Output a message.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     msgOut( const char * param, const char * text, int * status );

*  Description:
*     Any tokens in supplied message are expanded and the result is
*     output to the user. If the status argument is not set to SAI__OK
*     on entry, no action is taken except that the values of any
*     existing message tokens are always left undefined after a call to
*     msgOut. If an output error occurs, an error is reported and the
*     status argument is returned set to MSG__OPTER.
*
*     A call to msgOut is equivalent to a call to msgOutif with the
*     message output priority set to MSG__NORM.

*  Arguments:
*     param = const char * (Given)
*        The message name.
*     text = const char * (Given)
*        The message text.
*     status = int * (Given and Returned)
*        The global status.

*  Algorithm:
*     Just kill tokens if STATUS is bad; otherwise call msgOutif
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
*        Rewrite in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "mers1.h"
#include "msg_par.h"
#include "sae_par.h"

void
msgOut( const char * param, const char * text, int * status ) {

  /*  Check the inherited global status. */
  if (*status != SAI__OK) {

    /*     Status is not SAI__OK, so just annul the token table. */
    msg1Ktok();

  } else {

    /*     Call MSG_OUTIF with the conditional output priority set to
     *     MSG__NORM. */
    msgOutif( MSG__NORM, param, text, status );
  }

}
