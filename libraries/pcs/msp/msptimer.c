/*
*+
*  Name:
*     msptimer

*  Purpose:
*     A test of msp and the atimer library

*  Language:
*     Starlink C

*  Description:
*     A test of msp and the atimer library
*       % msptimer
*     msptimer send a message to itself after 1 second and prints a
*     message on recipt of the message

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
#include <time.h>
#include <sys/time.h>
#include <unistd.h>
#include "sae_par.h"
#include "atimer_par.h"
#include "atimer.h"
#include "msp_par.h"
#include "msp.h"

sendq_type timersend;
receiveq_type timerrecv;

void timeout
(
int timerid
)

{
   int status;
   status = SAI__OK;
   msp_send_message ( "timer calling", 14, timersend, timerrecv, &status );
}


int main()
{
   int status;
   receiveq_type commq;
   receiveq_type queues[1];
   char answer[512];
   int actlen;
   receiveq_type used;
   sendq_type slaveq;


   status = SAI__OK;
   msp_enter_task ( "timer", &commq, &status );
   msp_create_localq ( &timersend, &timerrecv, &status );

   atimer_settimr ( 1000, 10001, timeout, &status );

   queues[0] = timerrecv;
   msp_receive_message ( queues, 1, 1, 512, answer, &actlen, &used,
     &slaveq, &status );


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
