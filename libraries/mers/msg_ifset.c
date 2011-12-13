/*
*+
*  Name:
*     MSG_IFSET

*  Purpose:
*     Set the filter level for conditional message output.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_IFSET( FILTER, STATUS )

*  Description:
*     The value of the message filtering level is set using the given
*     filtering value. If no such level exists, then an error is
*     reported and the status returned set to MSG__IFINV: the current
*     filtering level remains unchanged.

*  Arguments:
*     FILTER = INTEGER (Given)
*        The filtering level.
*     STATUS = INTEGER (Given and Returned)
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"

F77_SUBROUTINE(msg_ifset)( INTEGER(FILTER),
                           INTEGER(STATUS) ) {
  msglev_t filter;
  int status;

  F77_IMPORT_INTEGER( *STATUS, status );
  F77_IMPORT_INTEGER( *FILTER, filter );
  msgIfset( filter, &status );
  F77_EXPORT_INTEGER( status, *STATUS );

}
