/*
*+
*  Name:
*     msgIflev

*  Purpose:
*     Return the current filter level for conditional message output.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgIflev( msglev_t * filter );

*  Description:
*     The value of the current filtering level set for conditional
*     message output is returned.

*  Arguments:
*     filter = msglev_t * (Returned)
*        The current message filtering level.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 1991 Science & Engineering Research Council.
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
*     10-JUN-1991 (PCTR):
*        Original version.
*     24-JUL-2008 (TIMJ):
*        Use Common block accessor
*     12-SEP-2008 (TIMJ):
*        Rewrite in C
*     23-DEC-2008 (TIMJ):
*        Use msglev_t rather than simple integer.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "merswrap.h"

void msgIflev( msglev_t * filter ) {
  /*  Return the current value of the message output filter level. */
  *filter = msg1Gtinf();
}
