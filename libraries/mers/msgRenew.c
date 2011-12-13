/*
*+
*  Name:
*     msgRenew

*  Purpose:
*     Renew any annulled message tokens in the current context.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgRenew();

*  Description:
*     Any message tokens which have been annulled by a call to msgOut,
*     msgOutif, msgLoad, errRep, errAnnul or errLoad are renewed.
*     If any new token value has been defined (using the msgSetx and
*     msgFmt routines) since the previous tokens were annulled, no
*     action is taken. The intended use of msgRenew is to renew all message
*     tokens immediately after a call msgOut, msgOutif, msgLoad,
*     errRep, errAnnul or errLoad for re-use in a subsequent message.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-JUN-1991 (PCTR):
*        Original version.
*     23-JUL-2008 (TIMJ):
*        Now written in C to call emsRenew
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "ems.h"
#include "merswrap.h"

void msgRenew( void ) {
  emsRenew();
}
