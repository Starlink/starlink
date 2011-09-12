
/*
*+
*  Name:
*     smf_get_nthread

*  Purpose:
*     Determine the number of threads to use.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_get_nthread( int *status );

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     The number of threads to use. A value of 1 is returned if an error
*     occurs.

*  Description:
*     This function returns the number of worker threads to use when
*     dividing a task up between multiple threads. Note, a value of "1"
*     means one worker thread in addition to the required manager thread
*     that co-ordinates the workers (i.e. the main thread in which the
*     application is started). The default value is the number
*     of CPU cores available, but this can be over-ridden by setting the
*     environment variable SMURF_THREADS to some other value.

*  Notes:
*     - This function is not in thr.c because it does not make
*     any pthread calls.

*  Authors:
*     David S Berry (JAC, UCLan)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     9-JUN-2008 (DSB):
*        Initial version.
*     29-MAY-2009 (TIMJ):
*        Use sysconf to get number of processors rather than non-portable
*        parsing of /proc. Use strtol instead of sscanf.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smf.h"

int smf_get_nthread( int *status ){

/* Local Variables */
   const char *env_text = NULL;
   int result = 1;    /* Number of threads selected */

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* If the SMURF_THREADS environment variable has been set, use its value. */
   env_text = getenv( "SMURF_THREADS" );
   if( env_text ) {
      result = strtol( env_text, NULL, 10 );
      if( result < 1 ) {
         *status = SAI__ERROR;
         msgSetc( "S", env_text );
         errRep( "", "Illegal value for environment variable "
                 "SMURF_THREADS: '^S'.", status );
      }
      msgOutiff( MSG__VERB, "", "Using %d threads obtained from environment variable",
                 status, result );

   } else {
#ifdef _SC_NPROCESSORS_ONLN
/* Otherwise, use sysconf. This is fairly portable (Tru64, Linux, OSX, Solaris)
   and supposedly POSIX compliant. */
      result = sysconf(_SC_NPROCESSORS_ONLN);
      msgOutiff( MSG__VERB, "", "Using %d threads derived from number of CPU cores",
                 status, result );
#else
/* Otherwise, default the number of threads to 1. */
      result = 1;
      msgOutiff( MSG__VERB, "", "Could not determine number of thread to use; using one thread",
                 status, result );
#endif
   }

/* Ensure we have at least one thread. */
   if( result < 1 ) result = 1;

/* If an error has occurred, use 1 thread. */
   if( *status != SAI__OK ) result = 1;

/* Return the result */
   return result;
}
