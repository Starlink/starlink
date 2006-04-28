/*
*+
*  Name:
*     mspslave

*  Purpose:
*     A test of msp

*  Language:
*     Starlink C

*  Description:
*      A test of msp - run in conjunction with mspmaster
*        % mspslave &
*        % mspmaster

*  Authors:
*     {original_author_entry}

*  History:
*     xx-xxx-1994 (BDK):
*      Original
*     8-JUL-1994 (AJC):
*      Add prologue and tidy header files
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include "sae_par.h"
#include "msp_par.h"
#include "msp.h"

int main()
{
   int status;
   sendq_type mastercom;
   receiveq_type qarr[1];
   int actlen;
   receiveq_type qid;
   char message[512];
   int j;
   receiveq_type replyq;


   status = SAI__OK;
   msp_enter_task ( "slave", &replyq, &status );
   qarr[0] = replyq;

   for ( j=0; j<1000; j++ )
   {
      msp_receive_message ( qarr, 1, 1, 512, message, &actlen, &qid,
        &mastercom, &status );
      msp_send_message ( "slave answering", 16, mastercom, replyq, &status );
      msp_send_message ( "slave answering", 16, mastercom, replyq, &status );

      if ( status != SAI__OK )
      {
         break;
      }
   }


   if ( status != SAI__OK )
   {
      printf ( "slave: bad status = %d\n", status );
   }
   else
   {
      message[actlen] = '\0';
      printf ( "slave: received size %d mess %s\n", actlen, message );
      printf ( "slave: connection %hd queue %hd\n", mastercom.connection,
        mastercom.ack_queue ); 
   }

/*   Trigger exit handler */

   kill ( getpid(), SIGINT );
   return 0;
}
