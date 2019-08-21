#if HAVE_CONFIG_H
#include <config.h>
#endif
#include <errno.h>

#define _POSIX_SOURCE 1

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#include "sae_par.h"
#include "ems.h"
#include "ndf1.h"
#include "ndf_err.h"

void ndf1Time( int ymdhm[ 5 ], float *sec, int *status ){
/*
*+
*  Name:
*     ndf1Time

*  Purpose:
*     Return the current UTC date and time.

*  Synopsis:
*     void ndf1Time( int ymdhm[ 5 ], float *sec, int *status );

*  Description:
*     This function returns the current UTC date and time stored as year,
*     month, day, hour, minute and second values.

*  Parameters:
*     ymdhm[5]
*        The full year, month, day, hour and minute values (in that
*        order), stored as integers.
*     *sec
*        The seconds value.
*     *status
*        The global status.

*  Notes:
*     - The year value is given in full (i.e. 1993, not simply 93).
*     - The month value starts at 1 for January.
*     - The day value is the day of the month, starting at 1.

*  Machine Dependencies:
*     Depending on the machine in use, this routine may or may not
*     return the date and time with sub-second accuracy, and may or may
*     not include the effect of recent leap seconds.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Authors:
*     DSB: David S. Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     3-APR-2019 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
      struct tm *local;   /* Pointer to local time */
      time_t timer;       /* Calendar time */
#if HAVE_GETTIMEOFDAY
      struct timeval tv;  /* Calendar time and microseconds */
      suseconds_t usec = 0; /* Microseconds */
#else
      long usec = 0;      /* Microseconds */
#endif
      int systat = 0;     /* System status */

/* Check inherited status. */
      if ( *status != SAI__OK ) return;

/* Obtain the calendar time, checking that it is available. Report an error */
/* if it is not. */
#if HAVE_GETTIMEOFDAY
      systat = gettimeofday( &tv, NULL );
      if (systat == 0) {
         timer = tv.tv_sec;
         usec = tv.tv_usec;
      }
#else
      timer = time( NULL );
      if (timer == (time_t)-1) {
         systat = -1;
      }else {
         systat = 0;
      }
#endif

      if ( systat == -1 ) {
          *status = NDF__FATIN;
          emsSyser( "ERRNO", errno );
          emsRep( " ", "Error determining current date/time: ^ERRNO", status );

/* Convert calendar time into UTC (roughly). */
      } else {
         local = gmtime( (const time_t *) &timer );

/* Return the other integer fields, making the month number start at one.*/
         ymdhm[ 0 ] = local->tm_year + 1900;
         ymdhm[ 1 ] = local->tm_mon + 1;
         ymdhm[ 2 ] = local->tm_mday;
         ymdhm[ 3 ] = local->tm_hour;
         ymdhm[ 4 ] = local->tm_min;

/* Return the seconds value. */
         *sec = (float)local->tm_sec + ((float)usec/1.0E6);
      }

/* If necessary, call the error tracing function. */
      if ( *status != SAI__OK ) ndf1Trace( "ndf1Time", status );

   }
