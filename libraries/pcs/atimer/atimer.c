
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <errno.h>

#include "sae_par.h"

#include "atimer_err.h"
#include "atimer_par.h"
#include "atimer.h"
#include "atimer_static.h"

#define true 1
#define false 0

/* int setitimer ( int which, const struct itimerval *value, struct
  itimerval *ovalue ); */

static struct timer_q *timer_queue = NULL;   /* Queue list-head */



/*+  ATIMER_CANTIM - remove an event from the timer queue */

void atimer_cantim
(
int timerid,    /* timeout identifier (given) */
int *status     /* global status (given and returned) */
)

/*   Method :
      Given a timer-id number cancel the appropriate queued event.
     Authors :
      Brian McIlwrath (RAL)
      B.D.Kelly (ROE)
     History :
      09 July 1993: original (bkm)
      10 Mar 1994: atimer version (BDK)
*/

{
   struct timer_q *cur_entry;             /* used in queue processing */
   struct timer_q *prev_entry=NULL;       /* used in queue processing */
   struct itimerval oldtime;              /* outstanding time */
   struct itimerval timer;                /* new time */
   int istat;                             /* local status */


   if ( *status != SAI__OK ) return;

   if ( timer_queue != NULL)
   {

/*   cancel any current timer while list is modified */

      timerclear(&timer.it_value);
      timerclear(&timer.it_interval);
      setitimer ( ITIMER_REAL, &timer, &oldtime );
      timer_queue->delta_t = oldtime.it_value;

/*   Search for the entry */

      cur_entry = timer_queue;

      do 
      {
         if ( cur_entry->timerid == timerid )
         {
            break;
         }
         prev_entry = cur_entry;
         cur_entry  = cur_entry->next;
      }
      while ( cur_entry != NULL );

      if (cur_entry != NULL) 
      {
         if ( cur_entry == timer_queue )
         {
            timer_queue = timer_queue->next;
            if ( timer_queue != NULL) 
            {
               timer_queue->delta_t.tv_sec += cur_entry->delta_t.tv_sec;
               if( (timer_queue->delta_t.tv_usec += cur_entry->delta_t.tv_usec)
                 >= 1000000 ) 
               {
                  timer_queue->delta_t.tv_usec -= 1000000;
                  ++(timer_queue->delta_t.tv_sec);
               }
            }
         }
         else 
         {
            if( (prev_entry->next = cur_entry->next) != NULL ) 
            {
               cur_entry->next->delta_t.tv_sec += cur_entry->delta_t.tv_sec;
               if ( ( cur_entry->next->delta_t.tv_usec += 
                 cur_entry->delta_t.tv_usec ) >= 1000000 ) 
               {
                  cur_entry->next->delta_t.tv_usec -= 1000000;
                  ++(cur_entry->next->delta_t.tv_sec);
               }
            }
         }
         free((char *) cur_entry);
      } 
      else
      {
         *status = ATIMER__NOTFOUND;
      }

      if ( timer_queue != NULL )
      {

/*   Restart the timer */

         timer.it_value = timer_queue->delta_t;
         timerclear(&timer.it_interval);
         istat = setitimer ( ITIMER_REAL, &timer, (struct itimerval *)0 );
         if ( istat == -1 )
         {
            perror ( "atimer_cantim - setitimer call failed" );
         }
      }
   }
   else
   {
      *status = ATIMER__EMPTY;
   }
}

/*=  ATIMER_HANDLER - signal handler for timer system */

static void atimer_handler
(
int signo                   /* signal number (given) */
)

/*   Method :
      This is called as a result of SIGALRM.
      Remove the entry from the front of the timer queue and invoke the
      associated function. If there are still entries on the queue, restart 
      the timer.
     Authors :
      B.D.Kelly (ROE)
     History :
      10 Mar 1994: original (BDK)
      12 Apr 1994: make the function static (BDK)
*/
{

   struct timer_q *cur_entry;    /* for queue processing */
   struct itimerval timer;       /* new time */
   int istat;                    /* local status */


   if ( timer_queue != 0 )
   {

/*   invoke the function at the head of the queue */

      (*timer_queue->func) ( timer_queue->timerid );

/*   Remove processed event from queue */

      cur_entry = timer_queue;
      timer_queue = timer_queue->next;
      free ( (char*) cur_entry );
      if ( timer_queue != 0 )
      {

/*   Restart the timer */

         timer.it_value = timer_queue->delta_t;
         timerclear(&timer.it_interval);
         istat = setitimer ( ITIMER_REAL, &timer, (struct itimerval *)0 );
         if ( istat == -1) 
         {
            perror ( "atimer_handler - setitimer call failed" );
         }
      }
   }
   signal ( SIGALRM, atimer_handler ); 

}


/*=  ATIMER_INSERT - insert an entry in the event list */

static void atimer_insert 
(
struct timer_q *new_entry,  /* pointer to item to be added (given) */
int *status                 /* global status (given and returned) */
)

/*   Method :
      Add a time value to the timer queue.
      The interval timer queue is implemented as an ordered in increasing 
      time order linked list of (struct timer_q *) entries with a list_head 
      pointed to by 'timer_queue'. Each entry is constructed to contain the 
      time interval (in setitimer format) required from completion of the
      previous entry.
      Note - The macros timercmp, timerisset and timerclear are included from
      <sys/time.h.>. The macro timer_sub is defined in "atimer_par.h"
     Authors :
      Brian McIlwrath (RAL)
      B.D.Kelly (ROE)
     History :   
      07Jul 1993: original (BKM)
      11Mar 1994: atimer version (BDK)
      12 Apr 1994: make the function static (BDK)
       5 Feb 1998: rename timersub to timer_sub as Linux has timersub
*/

{
   struct timer_q *cur_entry;
   struct timer_q *old_entry;
   struct timeval value;

   if ( *status != SAI__OK ) return;

/*   Check if the new entry has to go at the front of the queue */

   value = new_entry->delta_t;
   cur_entry = timer_queue;

   if ( timercmp ( &value, &cur_entry->delta_t, < ) )
   {
      timer_queue = new_entry;
      timer_queue->next = cur_entry;
      timer_sub ( &cur_entry->delta_t, &value );
   }
   else
   {

/*   Go past all accumulated intervals no larger than value */

      while ( timercmp ( &value, &cur_entry->delta_t, > ) &&
        ( cur_entry->next != 0 ) )
      {
         timer_sub ( &value, &cur_entry->delta_t );
         old_entry = cur_entry;
         cur_entry = cur_entry->next;
      }

      if ( cur_entry->next != 0 )
      {

/*   Insert the new entry at this point */

         new_entry->delta_t = value;
         new_entry->next = cur_entry;
         old_entry->next = new_entry;
         timer_sub ( &cur_entry->delta_t, &value );
      }
      else
      {

/*   New item goes at end of queue */

         new_entry->next = 0;
         cur_entry->next = new_entry;
         timer_sub ( &value, &cur_entry->delta_t );
         new_entry->delta_t = value;
      }
   }
}


/*+  ATIMER_SETTIMR - add an event to the timer queue */

void atimer_settimr
(
int delay,             /* time in millisecs (given) */
int timerid,           /* timer request identifier (given) */
void (*func)(),        /* address of associated routine (given) */
int *status            /* global status (given and returned) */
)

/*   Method :
     Authors :
      Brian McIlwrath (RAL)
      B.D.Kelly (ROE)
     History :   
      30 June 1993: original (bkm)
      10 Mar 1994: atimer version (BDK)
*/

{
   struct timeval value;        /* new time requested */
   struct itimerval oldtime;    /* unexpired part of current timer */
   struct timer_q *new_entry;   /* new queue entry */
   static int setup = 0;        /* first-time flag */
   struct itimerval timer;      /* new time */
   int istat;                   /* local status */


   if ( *status != SAI__OK ) return;

   if ( setup == 0 )
   {

/*   First time in, initialise signal handler */

      timer_queue = 0;
      signal ( SIGALRM, atimer_handler );
      setup = 1;
   }


/*   Convert milliseconds argument to seconds and microseconds */

   value.tv_sec = ( (long)delay ) / 1000;
   value.tv_usec = ( ( (long)delay ) % 1000 ) * 1000;

/*   Create the new queue entry */

   new_entry = (struct timer_q *)malloc(sizeof(struct timer_q));
   if ( new_entry != 0 )
   {
      new_entry->timerid = timerid;
      new_entry->func = func;
      new_entry->delta_t = value;

      if ( timer_queue == 0 ) 
      {

/*   The queue is currently empty, put the new entry at the front */

         timer_queue = new_entry;
         new_entry->next = 0;
      } 
      else
      {

/*   cancel any current timer while list is modified and change the
     interval at the front of the queue to the time still outstanding */

         timerclear(&timer.it_value);
         timerclear(&timer.it_interval);
         (void) setitimer(ITIMER_REAL, &timer, &oldtime);
         timer_queue->delta_t = oldtime.it_value;

/*   Put the new entry in the queue */

         atimer_insert ( new_entry, status );
      }


/*   Start the timer */

      timer.it_value = timer_queue->delta_t;
      timerclear(&timer.it_interval);
      istat = setitimer ( ITIMER_REAL, &timer, (struct itimerval *) 0 );
      if ( istat == -1) 
      {
         perror ( "atimer_settimr - setitimer call failed" );
         printf ( "sec = %ld, usec = %ld\n", new_entry->delta_t.tv_sec, 
          new_entry->delta_t.tv_usec );
      }
   }
   else
   {
      perror ( "atimer_settimr - malloc call failed" );
   }

}
