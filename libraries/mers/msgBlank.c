/*
*+
*  Name:
*     msgBlank

*  Purpose:
*     Output a blank line.

*  Language:
*    Starlink ANSI C

*  Invocation:
*     msgBlank( int * status );

*  Description:
*     A blank line is output to the user. If the status argument is not
*     set to SAI__OK on entry, no action is taken. If an output error
*     occurs, an error report is made and the status argument returned
*     set to MSG__OPTER.

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Use msgOut in private error context to prevent tokens from
*        being cleared.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1990, 1991, 1992 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1990 (PCTR):
*        Original version.
*     24-JAN-1991 (PCTR):
*        Changed to use MSG1_PRINT (i.e. environment independent).
*     26-AUG-1992 (PCTR):
*        Output the blank line conditionally, assuming MSG__NORM to be
*        the priority.
*     10-SEP-2008 (TIMJ):
*        Rewrite in C. Use msgOut.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "sae_par.h"
#include "ems.h"

void msgBlank( int * status ) {

  /*  Check the inherited global status. */
  if (*status != SAI__OK) return;

  /*  Mark a new error reporting context to ensure that no existing message 
   *  tokens are annulled. */
  emsMark();

  /* Deliver the message with normal priority */
  msgOut( "MSG_BLANK", " ", status );

  /*  Release the current error reporting context. */
  emsRlse();
}
