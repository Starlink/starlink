/*  ADAMNET - ADAM networking program
     Description :
      Implements the ADAM networking process. It interfaces the ADAM 
      intertask communication software on unix to TCP/IP, thereby
      allowing ADAM tasks on different machines to communicate with one
      another. 
     Invocation :
      This program has to be run from the shell.
     Method :
      Initialize into the low-level intertask communication system
      (MSP) and create a queue for receiving messages from other
      processes on this machine (COMMAND_Q) and a pair of send/receive
      queues for communicating between an i/o signal handler and the
      main-line code (NETWORKR_Q, NETWORKS_Q).
      Connect to TCP/IP and declare the "well-known port number" for 
      ADAMNET.
      Set up a listen socket for an incoming connection request with an
      associated signal routine and declare an exit handler.
      Any network 'event' - either receipt of a connect request or
      receipt of an actual ADAM network message, results in the signal 
      handling routine being delivered. This uses MSP to write a message to 
      the NETWORKS_Q. The main-line code then interprets the event.
      Any ADAM task on this machine which wants a message forwarding to 
      another machine does so by writing the message onto this processes 
      COMMAND_Q.
     Authors :
      B.D.Kelly (REVAD::BDK)
     History :
      20.02.1987:  original (REVAD::BDK)
      23.03.1988:  revise variable names (REVAD::BDK)
      25.03.1988:  remove COMMAND_Q to COMMON (REVAD::BDK)
      07.04.1988:  initialise ANT library (REVAD::BDK)
      18.04.1988:  use MESSYS__NETNAME (REVAD::BDK)
      26.04.1988:  check IOSB (REVAD::BDK)
      28.12.1989:  make maximum message size C_MAXMSG_LEN (REVAD::BDK)
      01.05.1990:  declare exit handler (RLVAD::AJC)
      14.04.1994:  TCP/IP version (REVAD::BDK)
     endhistory
*/

/*   Global constants */

#include "sae_par.h"
#include "ant.h"

int main()
{

   int status;            /*   Status */


   status = SAI__OK;


/*   Initialize the ANT library */

   ant_init ( &status );

/*  If OK, proceed */

   if ( status == SAI__OK ) 
   {
      ant_serve ( &status );
   }
   else 
   {
      printf ( "adamnet: bad initialisation status = %d\n", status );
   }
   return 0;
}
