/*
 *+
 *  Name:
 *     timer.c
 *
 *  Purpose:
 *     UNIX implementations of the routines timer_start and cpu_used
 *
 *  Authors:
 *     KS: Keith Shortridge (AAO)
 *     WFL: William Lupton (RGO)
 
 *  History:
 *     XX-XXX-198X (KS):
 *        Original.
 *     31-Mar-88 (WFL):
 *        Make portable between VMS and UNIX and return all
 *        elapsed times as integer microseconds.
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
