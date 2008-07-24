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

*  Implementation Notes:
*     -  This subroutine is the stand-alone version of MSG_BELL.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (PCTR):
*        Original version.
*     23-JUL-2008 (TIMJ):
*        Now written in C
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "msg_err.h"
#include "sae_par.h"
#include "ems.h"

#include <errno.h>
#include <stdio.h>

void msgBell( int * status ) {
  int retval = 0;

  if (*status != SAI__OK) return;

  retval = printf( "\a\n" );

  /*  Check the returned I/O status and report an error 
      message if necessary. */

  if (retval != 2) {
    *status = MSG__OPTER;
    emsMark;
    if (retval < 0) {
      emsSyser( "ERR", errno );
    } else {
      emsSetc( "ERR", "Unknown reason");
    }
    emsRep( "MSG_BELL_OPTER",
	    "Error encountered during BELL output: ^ERR", status );
    emsRlse;
  }

}
