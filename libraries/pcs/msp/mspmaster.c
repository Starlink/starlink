/*
*+
*  Name:
*     mspmaster

*  Purpose:
*     A test of msp

*  Language:
*     Starlink C

*  Description:
*     A test of msp - run in conjunction with mspslave
*      % mspslave &
*      % mspmaster

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council. All
*     Rights Reserved.

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
   sendq_type slavecom;
   receiveq_type commq;
   receiveq_type replyq;
   receiveq_type queues[1];
   char answer[512];
   int actlen;
   receiveq_type used;
   sendq_type slaveq;
   int j;


   status = SAI__OK;
   msp_enter_task ( "master", &commq, &status );
   msp_get_task_queue ( "slave", &slavecom, &status );
   msp_create_receiveq ( &replyq, &status );

   for ( j=0; j<1000; j++ )
   {
      msp_send_message ( "master calling", 15, slavecom, replyq, &status );
      queues[0] = replyq;
      msp_receive_message ( queues, 1, 1, 512, answer, &actlen, &used,
        &slaveq, &status );
      msp_receive_message ( queues, 1, 1, 512, answer, &actlen, &used,
        &slaveq, &status );
      if ( status != SAI__OK )
      {
         break;
      }
   }

   msp_delete_queue ( replyq, &status );

   if ( status != SAI__OK )
   {
      printf ( "master: bad status = %d\n", status );
   }
   else
   {
      answer[actlen] = '\0';
      printf ( "master: received - %s\n", answer );
   }

/*   Trigger exit handler */

   kill ( getpid(), SIGINT );
   return 0;
}
