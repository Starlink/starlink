/*
*+
*  Name:
*     msgIfgetenv

*  Purpose:
*     Get the filter level for conditional message output from an
*     environment variable.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgIfgetenv( int * status );

*  Description:
*     Translate the MSG_FILTER environment variable name into a value for the
*     filter level for conditional message output. The translation accepts
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
*
*     msgIfgetenv accepts abbreviations of these strings; any other value
*     will result in an error report and the status value being
*     returned set to MSG__INVIF. If an error occurs getting the
*     environment value, the status value is returned and an additional
*     error report is made.
*
*     If the environment variable is not set the reporting level
*     will remain unchanged (defaulting to NORMAL).

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Notes:
*     This routine provides a more controlled interface to reading
*     the MSG_FILTER environment variable rather than msgTune that
*     will attempt to read all MSG_ related variables.

*  Related Functions:
*     msgIfget, msgTune

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
*        New routine based on msgIfget_adam
*     27-JUL-2009 (TIMJ):
*        Remove environment name from API to promote a single
*        usage.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "msg_err.h"
#include "msg_par.h"
#include "mers1.h"

#include "merswrap.h"
#include "ems.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

void msgIfgetenv( int * status ) {

  const char ename[] = "MSG_FILTER";
  char *fname = NULL;   /* Name of message filtering level */

  /*  Check inherited global status. */
  if (*status != SAI__OK) return;

  /*  Mark a new error reporting context. */
  emsMark();

  /*  Get the message filtering level from the environment. */
  fname = getenv( ename );

  /* if we have a non-null value try to parse it */
  if ( fname && strlen(fname) > 1 ) {

    /* Translate this string to a message level and set it */
    msg1Ifget( fname, status );

    /*  Check the returned status. */
    if (*status != SAI__OK) {

      /*     The environment variable was read and had a value
	     but it was not understood */
      emsRepf( "MSG_GETIF_NOPAR",
	       "msgIfgetenv: Unable to get the informational filtering "
	       "level from the '%s' environment variable.", status, ename );

    }

  }

  /*  Release the current error reporting context. */
  emsRlse();
}

