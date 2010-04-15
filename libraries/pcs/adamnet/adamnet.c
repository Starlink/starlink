
/*
*+
*  Name:
*     ADAMNET

*  Purpose:
*     ADAM networking program

*  Language:
*     Starlink C

*  Invocation:
*     This program has to be run from the shell.

*  Description:
*     Implements the ADAM networking process. It interfaces the ADAM
*     intertask communication software on unix to TCP/IP, thereby
*     allowing ADAM tasks on different machines to communicate with one
*     another.

*  Algorithm:
*     Initialize into the low-level intertask communication system
*     (MSP) and create a queue for receiving messages from other
*     processes on this machine (COMMAND_Q) and a pair of send/receive
*     queues for communicating between an i/o signal handler and the
*     main-line code (NETWORKR_Q, NETWORKS_Q).
*     Connect to TCP/IP and declare the "well-known port number" for
*     ADAMNET.
*     Set up a listen socket for an incoming connection request with an
*     associated signal routine and declare an exit handler.
*     Any network 'event' - either receipt of a connect request or
*     receipt of an actual ADAM network message, results in the signal
*     handling routine being delivered. This uses MSP to write a message to
*     the NETWORKS_Q. The main-line code then interprets the event.
*     Any ADAM task on this machine which wants a message forwarding to
*     another machine does so by writing the message onto this processes
*     COMMAND_Q.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     20-FEB-1987 (REVAD::BDK):
*        Original
*     23-MAR-1988 (REVAD::BDK):
*        Revise variable names
*     25-MAR-1988 (REVAD::BDK):
*        Remove COMMAND_Q to COMMON
*     07-APR-1988 (REVAD::BDK):
*        Initialise ANT library
*     18-APR-1988 (REVAD::BDK):
*        Use MESSYS__NETNAME
*     26-APR-1988 (REVAD::BDK):
*        Check IOSB
*     28-DEC-1989 (REVAD::BDK):
*        Make maximum message size C_MAXMSG_LEN
*     01-MAY-1990 (RLVAD::AJC):
*        Declare exit handler
*     14-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
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
