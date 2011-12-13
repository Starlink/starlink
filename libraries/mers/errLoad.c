/*
*+
*  Name:
*     errLoad

*  Purpose:
*     Return error messages from the current error context.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     errLoad( char *param, int param_length, int *parlen,
*              char *opstr, int opstr_length, int *oplen,
*              int *status );

*  Description:
*     On the first call of this routine, the error table for the current
*     context is copied into a holding area, the current error context
*     is annulled and the first message in the holding area is returned.
*     Thereafter, each time the routine is called, the next message from
*     the holding area is returned.
*
*     The status associated with the returned message is returned in STATUS
*     until there are no more messages to return -- then STATUS is set to
*     SAI__OK, PARAM and OPSTR are set to blanks and PARLEN and OPLEN to 1
*     If there are no messages pending on the first call, a warning message
*     is returned with STATUS set to EMS__NOMSG.
*
*     After STATUS has been returned SAI__OK, the whole process is repeated
*     for subsequent calls.

*  Arguments:
*     param = char * (Returned)
*        The error message name. Should be of size ERR__SZPAR.
*     param_length = int (Given)
*        Actual size of "param"
*     parlen = int * (Returned)
*        The length of the error message name.
*     opstr = char * (Returned)
*        The error message. Should be of size ERR__SZMSG.
*     opstr_length = int (Given)
*        Actual size of "opstr"
*     oplen = int * (Returned)
*        The length of the error message.
*     STATUS = INTEGER (Given and Returned)
*        The status associated with the returned error message:
*        it is set to SAI__OK when there are no more messages

*  Notes:
*     The interface differs from emsEload in that the size of the return
*     buffers are allowed to be smaller than EMS__SZPAR and EMS__SZMSG. Ellipsis
*     will be used to indicate truncation.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1989, 1990, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J. Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-NOV-1989 (PCTR):
*        Original version adapted from ERR_FLUSH and MSG_MLOAD.
*     15-DEC-1989 (PCTR):
*        Changed name to ERR_LOAD, and converted to call EMS_ELOAD.
*     26-SEP-1990 (PCTR):
*        Changed argument list to include the message name.
*     16-APR-1994 (AJC):
*        Revised comments for revised behaviour
*     11-SEP-2008 (TIMJ):
*        Rewrite in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems.h"
#include "merswrap.h"
#include "star/util.h"
#include "err_par.h"

#include <string.h>

void errLoad( char *param, int param_length, int *parlen,
              char *opstr, int opstr_length, int *oplen,
              int *status ) {

  char partemp[ERR__SZPAR+1];
  char optemp[ERR__SZMSG+1];
  int partlen;
  int optlen;

  /* The ERR interface allows the buffer size to be specified
     so we call emsEload with our buffers and then truncate
     as required */

  /*  Load the pending error messages at the current context. */
  emsEload( partemp, &partlen, optemp, &optlen, status );

  /* Copy and truncat as needed */
  star_strellcpy( param, partemp, param_length );
  star_strellcpy( opstr, optemp, opstr_length );

  *oplen = strlen(opstr);
  *parlen = strlen(param);

}
