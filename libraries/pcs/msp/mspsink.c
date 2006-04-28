/*
*+
*  Name:
*     mspsink

*  Purpose:
*     A test of msp

*  Language:
*     Starlink C

*  Description:
*     A test of msp - run in conjunction with mspsink
*      % mspsink &
*      % mspsource
*     mspsource sends 1000 messages to the task registered as "sink"
*     which on termination prints a message.

*  Authors:
*     {original_author_entry}

*  History:
*     xx-xxx-1994 (BDK):
*      Original
*     8-JUL-1994 (AJC):
*      Add prologue and add message number to final message
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
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
   sendq_type sourcecom;
   receiveq_type commandq;
   receiveq_type qarr[1];
   int actlen;
   receiveq_type qid;
   char message[512];
   int j;


   status = SAI__OK;
   msp_enter_task ( "sink", &commandq, &status );
   qarr[0] = commandq;

   for ( j=0; j<10000; j++ )
   {
      msp_receive_message ( qarr, 1, 1, 512, message, &actlen, &qid,
        &sourcecom, &status );
   }

   if ( status != SAI__OK )
   {
      printf ( "sink: bad status = %d message number %d \n", status, j );
   }
   else
   {
      message[actlen] = '\0';
      printf ( "slave: message %d; %s\n ", j, message );
   }

   kill ( getpid(), SIGINT );

   return 0;

}
