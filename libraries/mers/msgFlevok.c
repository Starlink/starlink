/*
*+
*  Name:
*     msgFlevok

*  Purpose:
*     Return true if a message at the supplied level would be output

*  Language:
*     Starlink ANSI C

*  Invocation:
*     willoutput = msgFlevok( msglev_t filter, int *status );

*  Description:
*     Return true if the supplied message level would result in output
*     from msgOutif or msgBlankif. This allows user-supplied functions to
*     be written that can execute depending on message filtering level.

*  Arguments:
*     filter = msglev_t  (Given)
*        The message filtering level to be compared with the current value.
*     status = int * (Given & Returned)
*        Inherited status.

*  Returned Value:
*     int
*        True if message would be displayed. False otherwise.

*  Related Functions:
*     msgIflev - query the current message filtering level

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     14-JAN-2009 (TIMJ):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "merswrap.h"
#include "msg_par.h"
#include "msg_err.h"
#include "sae_par.h"
#include "ems.h"

int msgFlevok( msglev_t  filter, int *status ) {

  if (*status != SAI__OK) return 0;

  if (filter < MSG__QUIET || filter >= MSG__ALL) {

      /*        The given message filtering level is out of range: set the
       *        returned status and report an error. */
      *status = MSG__INVIF;
      emsSeti( "PRIOR", filter );
      emsRep( "MSG_FLEVOK_INVIF",
              "MSG_FLEVOK: Invalid message filtering value:  ^PRIOR",
              status );
      return 0;
  }

  if ( filter <= msg1Gtinf() ) {
    return 1;
  } else {
    return 0;
  }
}
