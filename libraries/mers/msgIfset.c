/*
*+
*  Name:
*     msgIfset

*  Purpose:
*     Set the filter level for conditional message output.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgIfset( msglev_t filter, int * status );

*  Description:
*     The value of the message filtering level is set using the given
*     filtering value. If no such level exists, then an error is
*     reported and the status returned set to MSG__IFINV: the current
*     filtering level remains unchanged.

*  Arguments:
*     filter = msglev_t (Given)
*        The filtering level. Any value from MSG__NONE to MSG__ALL is allowed.
*     status = int * (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
*     02-MAY-2008 (TIMJ):
*        Added MSG__DEBUG
*     24-JUL-2008 (TIMJ):
*        Use common block accessor
*     12-SEP-2008 (TIMJ):
*        Rewrite in C
*     23-DEC-2008 (TIMJ):
*        Use msglev_t rather than simple integer.
*     09-JAN-2009 (TIMJ):
*        Extend range of allowed values.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "mers1.h"
#include "merswrap.h"
#include "ems.h"
#include "sae_par.h"

#include "msg_err.h"
#include "msg_par.h"

void msgIfset( msglev_t filter, int * status ) {

  /*  Check inherited global status. */
  if (*status != SAI__OK) return;

  /*  Check that the given filter value is acceptable.
      The odd test of filter == and > MSG__NONE rather than using
      >= is because the compiler is clever enough to notice that msglev_t
      can be an unsigned type so will always be > 0. To hide the warning
      and allow msglev_t to be signed we expand the test.
   */
  if ( filter == MSG__NONE ||
       ( filter > MSG__NONE && filter <= MSG__ALL ) ) {

    /*     Assign the message filtering level. */
    msg1Ptinf( filter );

  } else {
    /*     The given value for message filtering is outside the allowed
     *     range: set status and report an error message. */
    emsMark();
    *status = MSG__INVIF;
    emsSeti( "FILTER", filter );
    emsSeti( "LOW", MSG__NONE );
    emsSeti( "HIGH", MSG__ALL );
    emsRep( "MSG_IFSET_INVIF",
            "MSG_IFSET: Invalid message filtering value: "
            "^LOW <= ^FILTER <= ^HIGH",
            status );
    emsRlse();
  }
}
