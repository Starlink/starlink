/*
*+
*  Name:
*     errStat

*  Purpose:
*     Inquire the last reported error status.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     errStat( int * status );

*  Description:
*     The current error context is checked for any error messages pending
*     output. If none exist, the status argument is returned set to
*     SAI__OK. If any messages have been reported, the status argument is
*     returned set to the last reported value.

*  Arguments:
*     status = int * (Returned)
*        The global status: it returned set to the last reported
*        error status within the current error context; if none exist,
*        it is returned set to SAI__OK.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     25-SEP-1990 (PCTR):
*        Original version.
*     19-JUL-2008 (TIMJ):
*        Rewrite in C.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "merswrap.h"
#include "ems.h"

void errStat( int * status ) {
  emsStat(status);
}
