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
*     msgIfget( const char * pname, int * status );

*  Description:
*     Translate the given parameter name into a value for the filter
*     level for conditional message output. The translation accepts
*     abbreviations. This value is then used to set the informational
*     filtering level. It is recommended that one parameter name is
*     used universally for this purpose, namely MSG_FILTER, in order to
*     clarify the interface file entries.  The acceptable strings
*     for MSG_FILTER are
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
*     msgIfget accepts abbreviations of these strings; any other value
*     will result in an error report and the status value being
*     returned set to MSG__INVIF. If an error occurs getting the
*     parameter value, the status value is returned and an additional
*     error report is made.

*  Arguments:
*     pname = const char * (Given)
*        The filtering level parameter name.
*     status = int * (Given and Returned)
*        The global status.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
#include "ems.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>

void msgIfget( const char * pname, int * status ) {

  /* we will assume that the index into this array corresponds to
     the actual constant msglev_t value */
  const char * slevels[] = {
    "NONE", "QUIET", "NORMAL", "VERBOSE", "DEBUG",
    "DEBUG1", "DEBUG2", "DEBUG3", "DEBUG4", "DEBUG5",
    "DEBUG6", "DEBUG7", "DEBUG8", "DEBUG9", "DEBUG10",
    "DEBUG11", "DEBUG12", "DEBUG13", "DEBUG14", "DEBUG15",
    "DEBUG16", "DEBUG17", "DEBUG18", "DEBUG19", "DEBUG20",
    "ALL", NULL
  };

  unsigned long strint; /* string as integer */
  size_t i;             /* Loop counter */
  msglev_t filter;      /* Message filtering level */
  size_t namcod;        /* SUBPAR pointer to parameter */
  char fname[8];        /* Name of message filtering level */
  size_t flen;          /* length of supplied string */
  const msglev_t badlev = -1; /* indicate that we did not match a level */
  char *endptr = NULL;  /* position in strong after finding number */

  /*  Check inherited global status. */
  if (*status != SAI__OK) return;

  /*  Mark a new error reporting context. */
  emsMark();

  /*  Get the message filtering level from the parameter system. */
  subParFindpar( pname, &namcod, status );
  subParGet0c( namcod, fname, sizeof(fname), status );

  /*  Check the returned status. */
  if (*status != SAI__OK) {

    /*     A parameter system error has occured: set the returned status,
     *     report the error and abort. */
    emsRep( "MSG_GETIF_NOPAR",
            "msgIfget: Unable to get the informational filtering "
            "level from the parameter system.", status );

  } else {

    filter = badlev;  /* initialise so that we can see if we set it */

    /* See if we have an integer rather than a string */
    errno = 0;
    strint = strtoul( fname, &endptr, 10 );

    /* Trapping failure seems to be non-portable to we ask for endptr so
       that we can compare it with fname as well as looking at errno. */
    if ( ( strint == 0 && errno != 0) || ( endptr == fname ) ) {
      /* was not an integer so treat as string */

      i = 0;
      flen = strlen( fname );

      while ( slevels[i] != NULL ) {
        /* compare case insensitive. Assume that subParGet0c
           returns terminated string */
        if (strncasecmp( slevels[i], fname, flen ) == 0 ) {

          /* we have a match */
          filter = i;
          break;
        }
        i++;
      }
    } else {
      /* was a valid match */
      filter = strint;
    }

    /*     Set the message filtering level. */
    if (filter != badlev) {
      msgIfset( filter, status );
    } else {

      /*        An invalid filter name has been used, so report an error. */
      *status = MSG__INVIF;
      emsSetc( "FILTER", fname );
      emsRep( "MSG_IFGET_INVIF",
              "MSG_IFGET: Invalid message filtering level: ^FILTER",
              status );
    }
  }

  /*  Release the current error reporting context. */
  emsRlse();
}

