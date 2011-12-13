/*
*+
*  Name:
*     msgIfget

*  Purpose:
*     Get the filter level for conditional message output from the ADAM
*     parameter system.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     msgIfget( int * status );

*  Description:
*     Controls the messaging filter level using a variety of
*     techniques. The messaging filter level can be read from the
*     QUIET parameter, the MSG_FILTER parameter or MSG_FILTER
*     environment variable. The QUIET parameter can be used to quickly
*     turn messaging off with minimum of typing whereas the MSG_FILTER
*     parameter and environment variable provide more detailed control
*     over the filtering level. The MSG_FILTER parameter is read first
*     and if a value is available it will be used (see below for
*     definitions). The QUICK parameter will be read next and compared
*     with MSG_FILTER for consistency, generating an error if they are
*     inconsistent. If the MSG_FILTER parameter has not been read, or
*     returns a NULL value the QUICK parameter will be used to control
*     the filter level. An explicit true value will set the filter
*     level to QUIET and an explicit false will set it to NORM.
*
*     If neither parameter returned a value the MSG_FILTER environment
*     variable will be read and parsed in a similar way to the
*     MSG_FILTER parameter.
*
*     The acceptable strings for MSG_FILTER parameter and environment
*     variables are:
*
*        -  NONE  -- representing MSG__NONE;
*        -  QUIET -- representing MSG__QUIET;
*        -  NORMAL -- representing MSG__NORM;
*        -  VERBOSE -- representing MSG__VERB;
*        -  DEBUG -- representing MSG__DEBUG;
*        -  DEBUG1 to DEBUG20 -- representing MSG__DEBUGnn;
*        -  ALL -- representing MSG__ALL.
*
*     msgIfget accepts abbreviations of these strings; any other value
*     will result in an error report and the status value being
*     returned set to MSG__INVIF.
*
*     Any use of abort (!!) when reading the parameters will be
*     recognized and the routine will return without reading the
*     environment variable.

*  Arguments:
*     status = int * (Given and Returned)
*        The global status.

*  Related Functions:
*     msgIfgetenv, msgTune

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
*     Copyright (C) 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2004 Central Laboratory of the Research Councils.
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
*     AJC: A. J. Chipperfield (STARLINK)
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1991 (PCTR):
*        Original version.
*     26-JUN-1991 (PCTR):
*        Added mark and release to prevent message tokens being annulled
*        on error.
*     27-AUG-1992 (PCTR):
*        Changed PAR call to a SUBPAR call and enabled abbreviations in
*        the accepted parameter values.
*     25-JAN-1996 (AJC):
*        re-format CHARACTER declarations
*     17-SEP-1999 (AJC):
*        Avoid calling MSG_IFSET - linking problem
*     1-JUL-2004 (DSB):
*        Use MSG1_GT... functions to get the values from the MSG_CMN
*        common blocks rather than directly accessing the common blocks
*        (which are initialised in a different shared library).
*     02-MAY-2008 (TIMJ):
*        Add MSG__DEBUG
*     12-SEP-2008 (TIMJ):
*        Rewrite in C.
*     09-JAN-2009 (TIMJ):
*        Add new message levels. Recognize an integer as a valid level.
*     22-JUL-2009 (TIMJ):
*        Now calls msg1Ifget for the bulk of the work.
*     29-JUL-2009 (TIMJ):
*        Fall back to reading the environment variable on error.
*     31-JUL-2009 (TIMJ):
*        - Remove parameter name from argument list
*        - Read MSG_FILTER and QUIET parameters
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include "sae_par.h"
#include "msg_par.h"
#include "msg_err.h"
#include "msg_par.h"
#include "mers1.h"

#include "merswrap.h"
#include "star/subpar.h"
#include "star/par_err.h"
#include "ems.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

void msgIfget( int * status ) {

  size_t namcod;        /* SUBPAR pointer to parameter */
  char fname[8];        /* Name of message filtering level */
  int hasq = 0;         /* Valid QUIET param? */
  int hasm = 0;         /* Valid MSG_FILTER param? */
  int quiet;            /*  boolean for QUIET */

  /*  Check inherited global status. */
  if (*status != SAI__OK) return;

  /*  Mark a new error reporting context. */
  emsMark();

  /*  Get the message filtering level from the parameter system. */
  subParFindpar( "MSG_FILTER", &namcod, status );
  subParGet0c( namcod, fname, sizeof(fname), status );
  if (*status == PAR__ABORT) {
    return;
  } else if (*status == SAI__OK) {
    hasm = 1;
  } else {
    emsAnnul( status );
    hasm = 0;
  }

  /* Also get the QUIET parameter */
  subParFindpar( "QUIET", &namcod, status );
  subParGet0l( namcod, &quiet, status );
  if (*status == PAR__ABORT) {
    return;
  } else if (*status == SAI__OK) {
    hasq = 1;
  } else {
    emsAnnul( status );
    hasq = 0;
  }

  /* Force to NORM before we enter the routine */
  msgIfset( MSG__NORM, status );

  /* Use environment if we read neither parameter */
  if (!hasm && !hasq) {
      msgIfgetenv( status );
  } else {

    if (hasm) {
      /* Translate the parameter string to a message level and set it */
      msg1Ifget( fname, status );

      /* Report that we had a problem with the value originating
         from the parameter */
      if (*status != SAI__OK) {
        emsRep( "MSG_GETIF_NOPAR",
                "msgIfget: Unable to get the informational filtering "
                "level from the parameter system.", status );
      }

      /* compare with QUIET setting */
      if (hasq && *status == SAI__OK) {
        msglev_t curlev;
        char curfil[MSG__SZLEV];
        curlev = msgIflev( curfil, status );

        if ( quiet && curlev > MSG__QUIET) {
          *status = MSG__INVIF;
          emsRepf( "MSG_GETIF_QINC",
                  "Inconsistent usage of MSG_FILTER and QUIET parameters: "
                  "QUIET is true but MSG_FILTER was '%s'",
                  status, curfil );
        } else if ( !quiet && msgIflev( NULL, status ) < MSG__NORM) {
          *status = MSG__INVIF;
          emsRepf( "MSG_GETIF_QINC",
                  "Inconsistent usage of MSG_FILTER and QUIET parameters: "
                  "QUIET was false but MSG_FILTER was '%s'",
                   status, curfil );
        }
      }

    } else if (hasq) {
      /* Just have quiet. Already set to NORM */
      if (quiet) {
        msgIfset( MSG__QUIET, status );
      }

    } else {
      if (*status == SAI__OK) {
        *status = SAI__ERROR;
        emsRep( "MSG_GETIF_ARGH",
                "msgIfget: Complete logic breadkdown. Should not be here",
                status );
      }
    }

  }
  /*  Release the current error reporting context. */
  emsRlse();
}

