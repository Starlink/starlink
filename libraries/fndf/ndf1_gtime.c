#if HAVE_CONFIG_H
#include <config.h>
#endif

#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#if !defined( vms )		 /* Portable version include files:	    */
#include <time.h>		 /* Date and time definitions		    */
#  if HAVE_SYS_TIME_H
#    include <sys/time.h>        /* gettimeofday                            */
#  endif
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* EMS_ error reporting routines	    */
#include "f77.h"		 /* Fortran <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_gtime)( INTEGER_ARRAY(YMDHM),
                               REAL(SEC),
			       INTEGER(STATUS) )
   {
/*
*+
*  Name:
*     NDF1_GTIME

*  Purpose:
*     Return the current date and time.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_GTIME( YMDHM, SEC, STATUS )

*  Description:
*     The routine returns the current date and time stored as year,
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
*     return the date and time with sub-second accuracy.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-SEP-1993 (RFWS):
*        Original version.
*     15-FEB-2006 (TIMJ):
*        Use correct tm_year logic.
*     07-DEC-2006 (TIMJ):
*        Add sub-second accuracy for unix (gettimeofday) (was in VMS)
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

/* External References:							    */
#if defined( vms )		 /* VMS system calls:			    */
      unsigned int SYS$NUMTIM	 /* Return date/time as integer fields	    */
         ( unsigned short int *timbuf,
	   int *timadr );
#endif

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      unsigned short int timbuf[ 7 ]; /* Buffer for date/time values	    */

#else				 /* Portable version local variables:	    */
      struct tm *local;		 /* Pointer to local time		    */
      time_t timer;		 /* Calendar time			    */
#  if HAVE_GETTIMEOFDAY
      struct timeval tv;         /* Calendar time and microseconds          */
      suseconds_t usec = 0;      /* Microseconds                            */
#  else
      long usec = 0;             /* Microseconds                            */
#  endif
      int systat = 0;            /* System status                           */
#endif

/*.									    */

/* Check inherited status.						    */
      if ( *STATUS != SAI__OK ) return;

/* VMS Version:								    */
/* ===========								    */
/* (Note this version allows sub-second accuracy.)			    */
#if defined( vms )

/* Obtain the current system time as integer fields.			    */
      (void) SYS$NUMTIM( &timbuf, (int *) 0 );

/* Return the required integer date/time values.			    */
      YMDHM[ 0 ] = (F77_INTEGER_TYPE) timbuf[ 0 ];
      YMDHM[ 1 ] = (F77_INTEGER_TYPE) timbuf[ 1 ];
      YMDHM[ 2 ] = (F77_INTEGER_TYPE) timbuf[ 2 ];
      YMDHM[ 3 ] = (F77_INTEGER_TYPE) timbuf[ 3 ];
      YMDHM[ 4 ] = (F77_INTEGER_TYPE) timbuf[ 4 ];

/* Return the seconds and fractions of seconds values.			    */
      *SEC = (F77_REAL_TYPE) ( (float) timbuf[ 5 ] +
                               (float) 0.01 * (float) timbuf[ 6 ] );

/* Portable Version:							    */
/* ================							    */
#else

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
	 emsRep( "NDF1_GTIME_NONE",
		 "Error determining current date/time: ^ERRNO", STATUS );
      }

/* Convert calendar time into local time.				    */
      else
      {
         local = localtime( (const time_t *) &timer );

/* Return the other integer fields, making the month number start at one.   */
	 YMDHM[ 0 ] = (F77_INTEGER_TYPE) ( local->tm_year + 1900 );
         YMDHM[ 1 ] = (F77_INTEGER_TYPE) ( local->tm_mon + 1 );
         YMDHM[ 2 ] = (F77_INTEGER_TYPE) ( local->tm_mday );
	 YMDHM[ 3 ] = (F77_INTEGER_TYPE) ( local->tm_hour );
	 YMDHM[ 4 ] = (F77_INTEGER_TYPE) ( local->tm_min );

/* Return the seconds value.						    */
         *SEC = (F77_REAL_TYPE) ((float)local->tm_sec + ((float)usec/1.0E6));
      }
#endif

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_gtime", STATUS );

/* Exit the routine.							    */
      return;
   }
