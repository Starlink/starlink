/*
*+
*  Name:
*     errMark

*  Purpose:
*     Mark (start) a new error context.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     errMark();

*  Description:
*     Begin a new error reporting context so that delivery of subsequently
*     reported error messages is deferred and the messages held in the
*     error table. Calls to ERR_ANNUL, ERR_FLUSH and ERR_LOAD will only
*     flush or annul the contents of the error table within this new
*     context.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 1983, 1989 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-APR-1983 (SLW):
*        Original version.
*     7-AUG-1989 (RFWS):
*        Converted to new prologue layout and added comments.
*     12-SEP-1989 (PCTR):
*        Completed code tidy-up.
*     15-DEC-1989 (PCTR):
*        Converted to use EMS_ calls.
*     19-JUL-2008 (TIMJ):
*        Rewrite in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "ems.h"

void errMark( void ) {
  emsMark();
}
