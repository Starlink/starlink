/*
*+
*  Name:
*     ERR_LOAD

*  Purpose:
*     Return error messages from the current error context.

*  Language:
*    Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     ERR_LOAD( PARAM, PARLEN, OPSTR, OPLEN, STATUS )

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
*     PARAM = CHARACTER * ( * ) (Returned)
*        The error message name.
*     PARLEN = INTEGER (Returned)
*        The length of the error message name.
*     OPSTR = CHARACTER * ( * ) (Returned)
*        The error message.
*     OPLEN = INTEGER (Returned)
*        The length of the error message.
*     STATUS = INTEGER (Given and Returned)
*        The status associated with the returned error message:
*        it is set to SAI__OK when there are no more messages

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
*        Revised comments for revised behaviouur
*     12-SEP-2008 (TIMJ):
*        Now in C. Calls errLoad.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"
#include "merswrap.h"

F77_SUBROUTINE(err_load)( CHARACTER(PARAM),
                          INTEGER(PARLEN),
                          CHARACTER(OPSTR),
                          INTEGER(OPLEN),
                          INTEGER(STATUS)
                          TRAIL(PARAM)
                          TRAIL(OPSTR) ) {

  char * opstr;
  char * param;
  int oplen;
  int parlen;
  int status;

  opstr = starMallocAtomic( OPSTR_length + 1 );
  param = starMallocAtomic( PARAM_length + 1 );
  F77_IMPORT_INTEGER( *STATUS, status );

  errLoad( param, PARAM_length+1, &parlen,
           opstr, OPSTR_length+1, &oplen, &status );

  F77_EXPORT_INTEGER( status, *STATUS );
  F77_EXPORT_INTEGER( oplen, *OPLEN );
  F77_EXPORT_INTEGER( parlen, *PARLEN );

  F77_EXPORT_CHARACTER( opstr, OPSTR, OPSTR_length );
  F77_EXPORT_CHARACTER( param, PARAM, PARAM_length );
  starFree( opstr );
  starFree( param );

}
