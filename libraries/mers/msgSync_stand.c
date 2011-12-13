/*
*+
*  Name:
*     msgSync

*  Purpose:
*     Synchronise message output via the user interface.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgSync( int * status );

*  Description:
*     This performs a synchronisation handshake with the user interface.
*     This is required it the current task has been outputting messages
*     via the user interface and now wants to use a graphics cursor on
*     the command device.

*  Arguments:
*     status = int * (Given and Returned)
*        The global status: it is returned set to MSG__SYNER on error.

*  Implementation Notes:
*     -  This subroutine is the STANDALONE version of MSG_SYNC.
*     -  The STANDALONE version does nothing.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1985, 1989 Science & Engineering Research Council.
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
*     BDK: Dennis Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     11-NOV-1985 (BDK):
*        Original version.
*     20-SEP-1989 (PCTR):
*        Converted to new prologue and layout.
*     15-DEC-1989 (PCTR):
*        Standalone version adapted from MSG_SYNC.
*     23-JUL-2008 (TIMJ):
*        Do nothing in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "sae_par.h"

void msgSync( int * status ) {
  /*  Standalone version does nothing. */
  if (*status != SAI__OK) return;
  return;
}
