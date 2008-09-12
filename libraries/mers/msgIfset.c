/*
*+
*  Name:
*     msgIfset

*  Purpose:
*     Set the filter level for conditional message output.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgIfset( int filter, int * status );

*  Description:
*     The value of the message filtering level is set using the given
*     filtering value. If no such level exists, then an error is 
*     reported and the status returned set to MSG__IFINV: the current 
*     filtering level remains unchanged.

*  Arguments:
*     filter = int (Given)
*        The filtering level.
*     status = int * (Given and Returned)
*        The global status.

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
*     02-MAY-2008 (TIMJ):
*        Added MSG__DEBUG
*     24-JUL-2008 (TIMJ):
*        Use common block accessor
*     12-SEP-2008 (TIMJ):
*        Rewrite in C
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

void msgIfset( int filter, int * status ) {

  /*  Check inherited global status. */
  if (*status != SAI__OK) return;

  /*  Check that the given filter value is acceptable. */
  if ( filter < MSG__QUIET || filter > MSG__DEBUG ) {

    /*     The given value for message filtering is outside the allowed
     *     range: set status and report an error message. */
    emsMark();
    *status = MSG__INVIF;
    emsSeti( "FILTER", filter );
    emsRep( "MSG_IFSET_INVIF",
            "MSG_IFSET: Invalid message filtering value: ^FILTER",
            status );
    emsRlse();
  } else {

    /*     Assign the message filtering level. */
    msg1Ptinf( filter );
  }
}
