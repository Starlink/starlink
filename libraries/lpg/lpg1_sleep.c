#include <sys/time.h>
#include <unistd.h>
#include <signal.h>
#include "f77.h"
#include "sae_par.h"

static void myhand( int );

F77_SUBROUTINE(lpg1_sleep)( REAL(DELAY), INTEGER(STATUS) ){
/*
*  Name:
*     lpg1_sleep

*  Purpose:
*     Suspend the process for a given number of seconds.

*  Description:
*     This function puts the calling process to sleep for a given
*     number of seconds.

*  Parameters:
*     DELAY = REAL (Given)
*        The number of seconds.
*     STATUS = INTEGER (Given and Returned)
*        The inherited global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1999 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/

   GENPTR_REAL(DELAY)
   GENPTR_INTEGER(STATUS)

   typedef void (*sighandler_t)(int);
   sighandler_t oldhand;
   struct itimerval *val, *oldval;
   int sec;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Allocate memory to store an interval timer structure. */
   val = (struct itimerval *) malloc( sizeof( struct itimerval ) );
   if ( val ){

/* Allocate memory to store the old interval timer structure. */
      oldval = (struct itimerval *) malloc( sizeof( struct itimerval ) );
      if( oldval ) {

/* The setitimer call raises a SIGALRM signal when the timer goes off.
   The default action on SIGALRM is to terminate the process. To avoid
   this, save the old SIGALRM handler, and use the myhand handler which
   does nothing. */
         oldhand = signal( SIGALRM, myhand );

/* Set the interval timer to zero to avoid repeated alarms being raised. */
         val->it_interval.tv_sec = 0;
         val->it_interval.tv_usec = 0;

/* Set the current timer to the required number of seconds and
   microseconds. Save the old timer in oldval. */
         sec = (int) *DELAY;
         val->it_value.tv_sec = sec;
         val->it_value.tv_usec = (int) ( ( *DELAY - (float) sec )*1000000 );
         if( setitimer( ITIMER_REAL, val, oldval ) == 0 ) {

/* Suspend the process until the alarm goes off. */
            pause();

/* Re-instate the original interval timer. */
            setitimer( ITIMER_REAL, oldval, NULL );
         }

/* The alarm has gone off. Re-instate the original SIGALRM handler. */
         (void) signal( SIGALRM, oldhand );

/* Free the memory. */
         free( oldval );
      }
      free( val );

   }

}

static void myhand( int sig ){ };
