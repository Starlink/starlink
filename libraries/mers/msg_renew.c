/*
*+
*  Name:
*     MSG_RENEW

*  Purpose:
*     Renew any annulled message tokens in the current context.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     CALL MSG_RENEW

*  Description:
*     Any message tokens which have been annulled by a call to MSG_OUT,
*     MSG_OUTIF, MSG_LOAD, ERR_REP, ERR_ANNUL or ERR_LOAD are renewed.
*     If any new token value has been defined (using the MSG_SETx and
*     MSG_FMTx routines) since the previous tokens were annulled, no
*     action is taken. The intended use of MSG_RENEW is to renew all message
*     tokens immediately after a call MSG_OUT, MSG_OUTIF, MSG_LOAD,
*     ERR_REP, ERR_ANNUL or ERR_LOAD for re-use in a subsequent message.

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
*        Now written in C.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "mers_f77.h"
#include "merswrap.h"

F77_SUBROUTINE(msg_renew)( void ) {
  msgRenew();
}
