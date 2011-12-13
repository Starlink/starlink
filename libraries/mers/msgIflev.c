/*
*+
*  Name:
*     msgIflev

*  Purpose:
*     Return the current filter level for conditional message output.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msglev_t msgIflev( char * filstr, int * status );

*  Description:
*     The value of the current filtering level set for conditional
*     message output is returned as both a string and as a msglev_t
*     integer.

*  Arguments:
*     filstr = char * (Given & Returned)
*        Buffer to receive the string version of the current message
*        reporting level. Can be a NULL pointer. If present should be
*        at least MSG__SZLEV characters (including terminator).
*     status = int * (Given & Returned)
*        Inherited global status. Buffer is not touched if status is bad.

*  Returned Value:
*     msglev_t msgIflev
*        The current message filtering level. Will be returned even if
*        status is bad.

*  Copyright:
*     Copyright (C) 2008-2009 Science and Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
*     23-JUL-2009 (TIMJ):
*        Rearrange API to include a string representation.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "mers1.h"
#include "merswrap.h"
#include "msg_err.h"

#include <string.h>

msglev_t msgIflev( char * filstr, int * status ) {
  /*  Return the current value of the message output filter level. */
  msglev_t filter;              /* Current filter level */
  const char * match = NULL;    /* Matching string */

  /* This will work even if status is bad */
  filter = msg1Gtinf();

  if (*status != SAI__OK) return filter;

  /* see if we have to fill in the buffer */
  if ( filstr ) {
    match = msg1Levstr( filter );
    if (match) {
      /* hope it is the right size */
      strcpy( filstr, match );
    } else {
      /* should not be possible */
      strcpy( filstr, "ERROR" );
      *status = MSG__FATAL;
      errRepf( "", "Internal error trying to convert a messaging level of %d to a string",
               status, filter );
    }
  }

  return filter;
}
