/* mspsource
 * A test of msp - run in conjunction with mspsink
 *   % mspsink &
 *   % mspsource
 * mspsource sends 1000 messages to the task reistered as "sink"
 * which on termination prints a message.
 *
 * History:
 *   xx-xxx-1994 (BDK):
 *      Original
 *    8-JUL-1994 (AJC):
 *      Add prologue and tidy header files
*/
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include "sae_par.h"
#include "msp_par.h"
#include "msp.h"

int main()
{
   int status;
   sendq_type slavecom;
   receiveq_type replyq;
   int j;


   status = SAI__OK;
   msp_enter_task ( "source", &replyq, &status );
   msp_get_task_queue ( "sink", &slavecom, &status );

   for ( j=0; j<10000; j++ )
   {
      msp_send_message ( "source calling", 15, slavecom, replyq, &status );
   }

   if ( status != SAI__OK )
   {
      printf ( "source: bad status = %d\n", status );
   }

   kill ( getpid(), SIGINT );

   return 0;
}
