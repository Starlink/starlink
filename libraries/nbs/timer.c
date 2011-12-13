/*
*+
*  Name:
*     timer.c

*  Purpose:
*     UNIX implementations of the routines timer_start and cpu_used

*  Language:
*     {routine_language}

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     KS: Keith Shortridge (AAO)
*     WFL: William Lupton (RGO)
*     {enter_new_authors_here}

*  History:
*     XX-XXX-198X (KS):
*        Original.
*     31-Mar-1988 (WFL):
*        Make portable between VMS and UNIX and return all
*        elapsed times as integer microseconds.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

#ifdef vms
typedef	long time_t;
struct tms
{
   time_t	tms_utime;		/* user time */
   time_t	tms_stime;		/* system time */
   time_t	tms_cutime;		/* user time, children */
   time_t	tms_cstime;		/* system time, children */
};
#define TIMEUNIT	100.0		/* time unit is 1/100 second */
#endif


#if HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif

#if HAVE_TIME_H
#  include <time.h>
#  define TIMEUNIT	60.0		/* time unit is 1/60 second */
#endif

#if HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

long tstart;
struct tms start;

void
timer_start()
{
   time (&tstart);
   times (&start);
}

int timer_time ()
{
   long buffer;

   time (&buffer);
   return 1000000 * (buffer-tstart);
}

int timer_utime ()
{
   struct tms buffer;

   times (&buffer);
   return (int) (1000000.0 / TIMEUNIT) * (buffer.tms_utime-start.tms_utime);
}

int timer_stime ()
{
   struct tms buffer;

   times (&buffer);
   return (int) (1000000.0 / TIMEUNIT) * (buffer.tms_stime-start.tms_stime);
}

int timer_cutime ()
{
   struct tms buffer;

   times (&buffer);
   return (int) (1000000.0 / TIMEUNIT) * (buffer.tms_cutime-start.tms_cutime);
}

int timer_cstime ()
{
   struct tms buffer;

   times (&buffer);
   return (int) (1000000.0 / TIMEUNIT) * (buffer.tms_cstime-start.tms_cstime);
}
