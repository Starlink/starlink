/*
*+
*  Name:
*     msgBell

*  Purpose:
*     Deliver an ASCII BEL character.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgBell( int * status );

*  Description:
*     A bell character and a new line is delivered to the user. If the
*     user interface in use supports the ASCII BEL character, this routine
*     will ring a bell and print a new line on the terminal.

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1993 Science & Engineering Research Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     23-JUL-2008 (TIMJ):
*        Now written in C
*     29-JUL-2008 (TIMJ):
*        Now the we have msg1Prtln there is no need for separate
*        ADAM and standalone implementations.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "mers1.h"
#include "msg_err.h"
#include "sae_par.h"
#include "ems.h"

#include <errno.h>
#include <stdio.h>

void msgBell( int * status ) {

  if (*status != SAI__OK) return;

  /* print the message using the ADAM or standalone output device
     and call msgSync to ensure the output buffer is delivered. */
  msg1Prtln( "\a", status );
  msgSync( status );

  /* Check the returned status and report an error message if necessary. */
  if (*status != SAI__OK) {
    *status = MSG__OPTER;
    emsMark();
    emsRep( "MSG_BELL_OPTER",
	    "Error encountered during BELL output.", status );
    emsRlse();
  }

}
