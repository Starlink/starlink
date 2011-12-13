#if HAVE_CONFIG_H
#include <config.h>
#endif

#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#include <time.h>		 /* Date and time definitions		    */
#if HAVE_SYS_TIME_H
#  include <sys/time.h>          /* gettimeofday                            */
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* EMS_ error reporting routines	    */
#include "f77.h"		 /* Fortran <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_time)( INTEGER_ARRAY(YMDHM),
                               REAL(SEC),
			       INTEGER(STATUS) )
   {
/*
*+
*  Name:
*     NDF1_TIME

*  Purpose:
*     Return the current UTC date and time.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_TIME( YMDHM, SEC, STATUS )

*  Description:
*     The routine returns the current UTC date and time stored as year,
*     month, day, hour, minute and second values.

*  Arguments:
*     YMDHM( 5 ) = INTEGER (Returned)
*        The full year, month, day, hour and minute values (in that
*        order), stored as integers.
*     SEC = REAL (Returned)
*        The seconds value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The year value is given in full (i.e. 1993, not simply 93).
*     -  The month value starts at 1 for January.
*     -  The day value is the day of the month, starting at 1.

*  Machine Dependencies:
*     Depending on the machine in use, this routine may or may not
*     return the date and time with sub-second accuracy, and may or may
*     not include the effect of recent leap seconds.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     DSB: David S Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     23-JAN-2009 (DSB):
*        Original version (based on ndf1_gtime.c).
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Returned:							    */
      GENPTR_INTEGER_ARRAY(YMDHM)
      GENPTR_REAL(SEC)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* Local Variables:							    */
      struct tm *local;		 /* Pointer to local time		    */
      time_t timer;		 /* Calendar time			    */
#if HAVE_GETTIMEOFDAY
      struct timeval tv;         /* Calendar time and microseconds          */
      suseconds_t usec = 0;      /* Microseconds                            */
#else
      long usec = 0;             /* Microseconds                            */
#endif
      int systat = 0;            /* System status                           */

/*.									    */

/* Check inherited status.						    */
      if ( *STATUS != SAI__OK ) return;

/* Obtain the calendar time, checking that it is available. Report an error */
/* if it is not.							    */
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
      }	else {
	systat = 0;
      }
#endif

      if ( systat == -1 )
      {
	 *STATUS = NDF__FATIN;
	 emsSyser( "ERRNO", errno );
	 emsRep( "NDF1_TIME_NONE",
		 "Error determining current date/time: ^ERRNO", STATUS );
      }

/* Convert calendar time into UTC (roughly).                               */
      else
      {
         local = gmtime( (const time_t *) &timer );

/* Return the other integer fields, making the month number start at one.   */
	 YMDHM[ 0 ] = (F77_INTEGER_TYPE) ( local->tm_year + 1900 );
         YMDHM[ 1 ] = (F77_INTEGER_TYPE) ( local->tm_mon + 1 );
         YMDHM[ 2 ] = (F77_INTEGER_TYPE) ( local->tm_mday );
	 YMDHM[ 3 ] = (F77_INTEGER_TYPE) ( local->tm_hour );
	 YMDHM[ 4 ] = (F77_INTEGER_TYPE) ( local->tm_min );

/* Return the seconds value.						    */
         *SEC = (F77_REAL_TYPE) ((float)local->tm_sec + ((float)usec/1.0E6));
      }

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_time", STATUS );

/* Exit the routine.							    */
      return;
   }
