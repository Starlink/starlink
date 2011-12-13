/*
*+
*  Name:
*     MSG_IFGETENV

*  Purpose:
*     Get the filter level for conditional message output from an
*     environment variable.

*  Language:
*     Starlink ANSI C (Callable from Fortran)

*  Invocation:
*     CALL MSG_IFGETENV( STATUS )

*  Description:
*     Translate the MSG_FILTER environment variable into a value for the filter
*     level for conditional message output. The translation accepts
*     abbreviations. This value is then used to set the informational
*     filtering level. The name is chosen internally to match that used
*     for msgTune when using the "ENVIRONMENT" to tune. The acceptable
*     strings for MSG_FILTER are
*
*        -  NONE  -- representing MSG__NONE;
*        -  QUIET -- representing MSG__QUIET;
*        -  NORMAL -- representing MSG__NORM;
*        -  VERBOSE -- representing MSG__VERB;
*        -  DEBUG -- representing MSG__DEBUG;
*        -  DEBUG1 to DEBUG20 -- representing MSG__DEBUGnn;
*        -  ALL -- representing MSG__ALL
*
*     MSG_IFGET accepts abbreviations of these strings; any other value
*     will result in an error report and the status value being
*     returned set to MSG__INVIF. If an error occurs getting the
*     environment value, the status value is returned and an additional
*     error report is made.
*
*     If the environment variable is not set the reporting level
*     will remain unchanged (defaulting to NORMAL).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine provides a more controlled interface to reading
*     the MSG_FILTER environment variable rather than msgTune that
*     will attempt to read all MSG_ related variables.

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
*     22-JUL-2009 (TIMJ):
*        Wrapper for msgIfgetenv
*     27-JUL-2009 (TIMJ):
*        Remove environment name from API to promote a single
*        usage.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "f77.h"
#include "merswrap.h"
#include "mers_f77.h"
#include "star/mem.h"

F77_SUBROUTINE(msg_ifgetenv)( INTEGER(STATUS) ) {
  int status;
  F77_IMPORT_INTEGER( *STATUS, status );
  msgIfgetenv( &status );
  F77_EXPORT_INTEGER( status, *STATUS );
}
