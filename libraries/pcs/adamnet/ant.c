
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <ctype.h>
#include <unistd.h>

#include <sys/wait.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <dirent.h>

#include "sae_par.h"
#include "adam_defns.h"
#include "messys_len.h"

#include "messys_err.h"
#include "messys_par.h"

#include "ams_struc.h"
#include "ams_sys.h"

#include "msp_par.h"
#include "msp_err.h"
#include "msp.h"
#include "sock.h"
#include "ant_par.h"
#include "ant_err.h"
#include "ant_sys.h"
#include "ant_cmn.h"


/*   Signal handlisng structures */

/* Linked list of signal numbers and the RTL handler routine addresses */

struct siglist {
                 int signo;
                 struct sigaction act;
                 struct siglist *next;
               };

/* Linked list head */

static struct siglist *stack_top = NULL;

/* Status bitmask for signals we have processed */

static unsigned int sigisset=0;

#include "ant.h"
#include "ant_static.h"



#define ANT_CHECKLEN(len) if((len<0)||(len>MSG_VAL_LEN)) \
   { \
      printf ( "checklen - length too big = %d\n", len ); \
      len=MSG_VAL_LEN; \
   }





void ant_accept
(
int *status    /* global status (given and returned) */
)

/*
*+
*  Name:
*     ANT_ACCEPT

*  Purpose:
*     Accept a network connection request

*  Language:
*     Starlink C

*  Algorithm:
*     A connection request has been received.
*     Store the new channel and unit numbers in COMMON arrays indexed by
*     MACHNUM.
*     Start the read on the new channel.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     20-FEB-1987 (REVAD::BDK):
*        Original
*     23-MAR-1988 (REVAD::BDK):
*        Revise variable names
*     27-APR-1988 (REVAD::BDK):
*        Fix interpretation of RXBUF
*     19-MAY-1988 (REVAD::BDK):
*        Improve status checking
*     18-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int machnum;                       /* index to machine data */
   int found;                         /* flag for free slot located */
   int fileflags;                     /* socket characteristics */
   int istat;                         /* local status */


   if ( *status != SAI__OK ) return;


/* locate an unused machine slot */

   found = 0;
   for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
   {
      if ( n_mach[machnum].mach_state == ANT__NULL_MACH )
      {
         found = 1;
         break;
      }
   }

   if ( found == 1 )
   {

/*   accept the call */

      sock_accept ( listen_channel, &(n_mach[machnum].local_channel),
        status );


/*   Get the name of the other machine */

      if ( *status == SAI__OK )
      ant_verify ( n_mach[machnum].local_channel,
        n_mach[machnum].machine_names, status );

/*      strcpy ( n_mach[machnum].machine_names, "other" ); */
      if ( *status == SAI__OK )
      {
         n_mach[machnum].remote_machine_num = MESSYS__NULL_M;
         n_mach[machnum].mach_state = ANT__OTHER_INIT;

/*   enable the new network channel for signals */

         fcntl ( n_mach[machnum].local_channel, F_SETOWN, getpid() );
         fileflags = fcntl ( n_mach[machnum].local_channel, F_GETFL );
         if ( fileflags == -1 )
         {
            perror ( "ant_accept: GETFL error" );
         }
         else
         {
            istat = fcntl ( n_mach[machnum].local_channel, F_SETFL,
              fileflags | FASYNC );
            if ( istat == -1 )
            {
               perror ( "ant_accept: SETFL error" );
            }
         }
      }
   }
   else
   {

/*   No room. Just let the other end timeout */

   }
}



void ant_accept_netack
(
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_ACCEPT_NETACK

*  Purpose:
*     Accept a netack message

*  Language:
*     Starlink C

*  Algorithm:
*     Complete the relevant N_PATH entry from information in the
*     received message.
*     Forward an acknowledgement message to the local task.
*     On error, return a DEINIT message across the network and annul the
*     N_PATH entry.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Check path is in range
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;     /* structure for sending to local task */
   int path;                    /* path number */
   int found;                   /* error flag */
   int machnum;                 /* machine number */
   int npath;                   /* path number in network format */
   int ntype;                   /* message type in network format */

   if ( *status != SAI__OK ) return;


/*   Complete the entry in the tables */

   found = 0;
   memcpy ( &npath, k_rpath, 4 );
   path = ntohl ( npath );

   if ( ( path >= 0 ) && ( path < MESSYS__MXPATH ) )
   {

      if ( n_paths[path].path_state == ANT__THIS_P )
      {
         found = 1;
         memcpy ( &npath, k_spath, 4 );
         n_paths[path].remote_nettask_n_path_num = ntohl ( npath );
         n_paths[path].path_state = ANT__FULL_P;

/*   Build a structure and send it to the target task */

         outbuf.mess_in_type = C_REM_ACK_IN;
         outbuf.u.rem_ack_in.local_task_t_path_num =
           n_paths[path].local_task_t_path_num;
         outbuf.u.rem_ack_in.local_nettask_n_path_num = path;
         msp_send_message ( (char *)&outbuf, C_REM_ACK_IN_LEN,
           n_paths[path].local_task_reminit_ack_q, command_q, status );

      }

      if ( ( found == 0 ) || ( *status != SAI__OK ) )
      {

/*  Send a rejection across the network */

         *status = SAI__OK;
         machnum = n_paths[path].local_machine_num;
         ntype = htonl ( C_NET_DEINIT_OUT );
         memcpy ( netbuffer, &ntype, 4 );
         memcpy ( &npath, k_spath, 4 );
         memcpy ( d_rpath, &npath, 4 );
         sock_write ( n_mach[machnum].local_channel,
           C_NET_MAXMSG_LEN, netbuffer, status );

/*   Bad status ignored. Hope that a network EVENT is in the
     process of occurring and things will be tidied elsewhere. */

         if ( found == 1 )
         {

/*   Remove the PATH entry */

            n_paths[path].local_taskname[0] = '\0';
            n_paths[path].remote_taskname[0] = '\0';
            n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
            n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
            n_paths[path].local_machine_num = MESSYS__NULL_M;
            n_paths[path].path_state = ANT__NULL_P;
         }

      }
   }
   else
   {
         *status = ANT__IVNETACK;
   }

}



void ant_accept_netdeinit
(
int *status                            /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_ACCEPT_NETDEINIT

*  Purpose:
*     Accept a netdeinit message

*  Language:
*     Starlink C

*  Algorithm:
*     Find the relevant N_PATH entry from information in the
*     received message.
*     Forward a deinit message to the local task.
*     Search for any transactions associated with the N_PATH entry. Send
*     deinit messages to their associated queues and annul the
*     transaction entries.
*     Annul the N_PATH entry.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1988 (REVAD::BDK):
*        Original
*     08-JUN-1988 (REVAD::BDK):
*        Use local status
*     31-MAY-1990: handle case where deinit is the reply to init
*                  (REVAD::BDK)
*     11-JAN-1996: only send DEINIT on path, not separate transactions
*                  (BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;    /* structure for sending to local task */
   int path;                   /* path number */
   int trans_num;              /* transaction number */
   int istat;                  /* local status */
   int npath;                  /* path number in network format */

   if ( *status != SAI__OK ) return;


/*   Locate the entry in the tables */

   memcpy ( &npath, d_rpath, 4 );
   path = ntohl ( npath );


/* ****** is path in range? ********** */

   if ( ( path < 0 ) || ( path >= MESSYS__MXPATH ) )
   {
      printf ( "ant_accept_netdeinit: path out of range\n" );
      return;
   }

   if ( n_paths[path].path_state == ANT__THIS_P )
   {

/*   The local task is waiting for the reply to an INIT.
     Send the DEINIT to the queue expecting the acknowledgement. */

      outbuf.mess_in_type = C_REM_DEINIT_IN;
      outbuf.u.rem_deinit_in.local_task_t_path_num =
        n_paths[path].local_task_t_path_num;
      istat = SAI__OK;
      msp_send_message ( (char *)&outbuf, C_REM_DEINIT_IN_LEN,
        n_paths[path].local_task_reminit_ack_q, command_q, &istat );

/*   Remove the PATH entry */

      n_paths[path].local_taskname[0] = '\0';
      n_paths[path].remote_taskname[0] = '\0';
      n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
      n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
      n_paths[path].local_machine_num = MESSYS__NULL_M;
      n_paths[path].path_state = ANT__NULL_P;

   }
   else if ( n_paths[path].path_state == ANT__FULL_P )
   {

/*   Build a structure and send it to the target task's command queue */

      outbuf.mess_in_type = C_REM_DEINIT_IN;
      outbuf.u.rem_deinit_in.local_task_t_path_num =
        n_paths[path].local_task_t_path_num;
      istat = SAI__OK;
      msp_send_message ( (char *)&outbuf, C_REM_DEINIT_IN_LEN,
        n_paths[path].local_task_q, command_q, &istat );

/*   Search for active transactions and close them */

      for ( trans_num=0; trans_num<MESSYS__MXTRANS; trans_num++ )
      {
         if ( path == n_trans[trans_num].local_nettask_n_path_num )
         {
            n_trans[trans_num].local_nettask_n_path_num = MESSYS__NULL_P;
            n_trans[trans_num].local_task_t_num = MESSYS__NULL_T;
            n_trans[trans_num].remote_nettask_n_trans_num = MESSYS__NULL_T;
            n_trans[trans_num].trans_state = ANT__NULL_T;
         }
      }

/*   Remove the PATH entry */

      n_paths[path].local_taskname[0] = '\0';
      n_paths[path].remote_taskname[0] = '\0';
      n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
      n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
      n_paths[path].local_machine_num = MESSYS__NULL_M;
      n_paths[path].path_state = ANT__NULL_P;

   }
   else if ( n_paths[path].path_state == ANT__OTHER_P )
   {

/*   Remove the PATH entry */

      n_paths[path].local_taskname[0] = '\0';
      n_paths[path].remote_taskname[0] = '\0';
      n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
      n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
      n_paths[path].local_machine_num = MESSYS__NULL_M;
      n_paths[path].path_state = ANT__NULL_P;
   }
   else
   {

/*   Do nothing. The path must have been already removed. */

   }

}



void ant_accept_netinit
(
int machnum,                         /* index to other machine details
                                        (given) */
int *status                          /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_ACCEPT_NETINIT

*  Purpose:
*     Accept a netinit message

*  Language:
*     Starlink C

*  Algorithm:
*     Search the common blocks for an unused N_PATH entry.
*     Put partial information into it from the received message.
*     Open communications with the target task on this machine.
*     On error, return a DEINIT message across the network and annul the
*     new N_PATH entry.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1988 (REVAD::BDK):
*        Original
*     28-APR-1988 (REVAD::BDK):
*        Hardwire MSP prefix for local task (frig)
*     20-MAY-1988 (REVAD::BDK):
*        Use ANT_MSPNAME
*     31-MAY-1990 (REVAD::BDK):
*        Always return ok status
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;      /* structure for sending to local task */
   char mspname[MESSYS__TNAME];  /* MSP name of local task */
   int path;                     /* path number for new entry */
   int found;                    /* loop controller */
   int npath;                    /* path number in network format */
   int ntype;                    /* message type in network format */

   if ( *status != SAI__OK ) return;


/*   Make a partial entry in the tables */

   found = 0;
   for ( path=0; path<MESSYS__MXPATH; path++)
   {
      if ( n_paths[path].path_state == ANT__NULL_P )
      {
         found = 1;
         break;
      }
   }
   if ( found == 1 )
   {

      strcpy ( n_paths[path].local_taskname, i_rtask );
      strcpy ( n_paths[path].remote_taskname, i_stask );
      memcpy ( &npath, i_spath, 4 );
      n_paths[path].remote_nettask_n_path_num = ntohl ( npath );
      n_paths[path].local_machine_num = machnum;
      n_paths[path].path_state = ANT__OTHER_P;

/*   Try to open a queue to the required task */

      strcpy ( mspname, n_paths[path].local_taskname );

      msp_get_task_queue ( mspname,
        &(n_paths[path].local_task_q), status );

      if ( *status == SAI__OK )
      {

/*   Build a structure and send it to the target task */

         outbuf.mess_in_type = C_REM_INIT_IN;
         strcpy ( outbuf.u.rem_init_in.remote_taskname,
           n_paths[path].remote_taskname );
         strcpy ( outbuf.u.rem_init_in.local_taskname,
           n_paths[path].local_taskname );
         strcpy ( outbuf.u.rem_init_in.remote_machine_name,
           n_mach[machnum].machine_names );
         outbuf.u.rem_init_in.local_nettask_n_path_num = path;
         msp_send_message ( (char *)&outbuf, C_REM_INIT_IN_LEN,
           n_paths[path].local_task_q, command_q, status );
      }

   }

   if ( ( found == 0 ) || ( *status != SAI__OK ) )
   {

/*   Send a rejection across the network */

      *status = SAI__OK;
      ntype = htonl ( C_NET_DEINIT_OUT );
      memcpy ( netbuffer, &ntype, 4 );
      memcpy ( d_rpath, i_spath, 4 );
      sock_write ( n_mach[machnum].local_channel,
        C_NET_MAXMSG_LEN, netbuffer, status );

/*   Bad status ignored. There is nothing useful can be done. */

      *status = SAI__OK;

/*   Remove the PATH entry */

      n_paths[path].local_taskname[0] = '\0';
      n_paths[path].remote_taskname[0] = '\0';
      n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
      n_paths[path].local_machine_num = MESSYS__NULL_M;
      n_paths[path].path_state = ANT__NULL_P;

   }

}



void ant_call_out
(
struct a_rem_call_out rem_call_out, /* the received message (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_CALL_OUT

*  Purpose:
*     Handle request to make network call

*  Language:
*     Starlink C

*  Algorithm:
*     Get the name of the target machine from the given message. Check
*     the ANT common blocks to see if a link to it exists. If not, open
*     a network connection to it. Return an acknowledgement to the
*     requesting task.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-APR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Change order of if-then-else
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in reply;   /* message returned to task */
   int found;                /* loop controller */
   int machnum;              /* machine number */


   if ( *status != SAI__OK ) return;


/*   Check whether network link with machine exists */

   found = 0;

   for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
   {
      if ( strcmp ( n_mach[machnum].machine_names,
        rem_call_out.remote_machine_name ) == 0 )
      {
         found = 1;
         break;
      }
   }

   if ( found == 0 )
   {

/*   Need to open a network link. */

      ant_opennet ( rem_call_out.remote_machine_name,
        reply_q, status );
      if ( *status != SAI__OK )
      {

/*   Return an informational message */

         reply.mess_in_type = C_REM_ACCEPT_IN;
         reply.u.rem_accept_in.accept_status = *status;
         *status = SAI__OK;
         msp_send_message ( (char *)&reply, C_REM_ACCEPT_IN_LEN,
           reply_q, command_q, status );
      }

   }
   else if ( n_mach[machnum].mach_state == ANT__THIS_START )
   {

/*   Connection to other machine has been already been requested by
     an ADAM task on this machine but is not yet complete.
     Return an informational message. */

      reply.mess_in_type = C_REM_ACCEPT_IN;
      reply.u.rem_accept_in.accept_status = ANT__CONTEND;
      msp_send_message ( (char *)&reply, C_REM_ACCEPT_IN_LEN,
        reply_q, command_q, status );

   }
   else
   {

/*  Connection to other machine is already complete. Return an
    ACCEPT message. */

      reply.mess_in_type = C_REM_ACCEPT_IN;
      reply.u.rem_accept_in.accept_status = SAI__OK;
      msp_send_message ( (char *)&reply, C_REM_ACCEPT_IN_LEN,
        reply_q, command_q, status );

   }

}



void ant_commq
(
struct a_mess_out rxbuf,   /* the received message (given) */
sendq_type reply_q,        /* reply queue for the message (given) */
int *status                /* global status (given and returned) */
)

/*
*+
*  Name:
*     ANT_COMMQ

*  Purpose:
*     Handle messages received on the command queue

*  Language:
*     Starlink C

*  Algorithm:
*     The given message will have been sent by an ADAM task on this
*     machine. The message will be one of the MESSYS REM_ data structures.
*     Inspect its TYPE flag to see what action is requested.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     25-MAR-1988 (REVAD::BDK):
*        Original
*     18-APR-1988 (REVAD::BDK):
*        Add call to ANT_OBEY
*     20-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK type
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   if ( *status != SAI__OK ) return;

   if ( rxbuf.mess_out_type == C_REM_CALL_OUT )
   {
      ant_call_out ( rxbuf.u.rem_call_out, reply_q, status );
   }
   else if ( rxbuf.mess_out_type == C_REM_ACK_OUT )
   {
      ant_send_netack ( rxbuf.u.rem_ack_out, reply_q, status );
   }
   else if ( rxbuf.mess_out_type == C_REM_DEINIT_OUT )
   {
      ant_send_netdeinit ( rxbuf.u.rem_deinit_out, status );
   }
   else if ( rxbuf.mess_out_type == C_REM_INIT_OUT )
   {
      ant_send_netinit ( rxbuf.u.rem_init_out, reply_q, status );
   }
   else if ( rxbuf.mess_out_type == C_REM_GSOC_START_OUT )
   {
      ant_send_start_out ( rxbuf.u.rem_gsoc_start_out, reply_q, status );
   }
   else if ( rxbuf.mess_out_type == C_REM_MSG_OUT )
   {
      ant_send_ack_out ( rxbuf.u.rem_msg_out, reply_q, status );
   }
   else if ( rxbuf.mess_out_type == C_REM_GSOC_END_OUT )
   {
      ant_send_end_out ( rxbuf.u.rem_msg_out, status );
   }
   else if ( rxbuf.mess_out_type == C_LOC_GSOC_START_OUT )
   {

/*      Request for diagnostics */

      ant_obey ( rxbuf.u.loc_gsoc_start_out, status );
   }
   else
   {
      *status = ANT__IVMESSOUT;
   }

}



void ant_connect
(
char *rmach,               /* name of remote machine (given) */
int *channel,              /* i/o channel for communications,
                              bound to a socket (returned) */
int *status                /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_CONNECT

*  Purpose:
*     Connect to an adamnet on another machine

*  Language:
*     Starlink C

*  Algorithm:
*     Create a channel for communications to another machine.
*     Bind the channel to a socket, make the call, enable SIGIO on the
*     socket.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     15-APR-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   struct sockaddr_in connect_addr;  /* socket info */
   struct hostent hostentstruct;     /* network data structure for other
                                        machine */


   if ( *status != SAI__OK ) return;

/*   get network data structure for remote machine */

   sock_ghbn ( rmach, &hostentstruct, status );

/*   construct the data structure for the connection request */

   *channel = socket ( AF_INET, SOCK_STREAM, 0 );

   memset ( &connect_addr, 0, sizeof(connect_addr) );
   connect_addr.sin_family = AF_INET;
   connect_addr.sin_addr = *((struct in_addr *)hostentstruct.h_addr);
   connect_addr.sin_port = htons ( ANT__PORTNUM );

   sock_connect ( *channel, (struct sockaddr *)&connect_addr,
     status );
   if ( *status == SAI__OK )
   {
      fcntl ( *channel, F_SETOWN, getpid() );
      fcntl ( *channel, F_SETFL, FASYNC );
   }
}



void ant_discon
(
int machnum,       /* machine number associated with the call to
                      be disconnected (given) */
int *status        /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_DISCON

*  Purpose:
*     Disconnect a network call

*  Language:
*     Starlink C

*  Algorithm:
*     Search for all active paths and transactions dependent on the
*     machine number and close them all down.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     29-MAR-1988 (REVAD::BDK):
*        Original
*     29-APR-1988 (REVAD::BDK):
*        Declare UNITNUM I*2
*     30-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK structures
*     30-MAY-1988 (REVAD::BDK):
*        Handle partly installed paths
*     08-JUN-1988 (REVAD::BDK):
*        Use local status
*     18-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


{
   struct a_mess_in t_close;  /* end transaction message */
   struct a_mess_in p_close;  /* close path message */
   int path;                  /* path number */
   int trans_num;             /* transaction number */
   int istat;                 /* local status */


   if ( *status != SAI__OK ) return;


/*   Close the network channel */

   close ( n_mach[machnum].local_channel );

/*   Free the common storage */

   n_mach[machnum].machine_names[0] = '\0';
   n_mach[machnum].local_channel = 0;
   n_mach[machnum].remote_machine_num = MESSYS__NULL_M;
   n_mach[machnum].mach_state = ANT__NULL_MACH;

/*   Search for PATHs depending on MACHNUM */

   for ( path=0; path<MESSYS__MXPATH; path++ )
   {
      if ( machnum == n_paths[path].local_machine_num )
      {
         for ( trans_num=0; trans_num<MESSYS__MXTRANS; trans_num++)
         {
            if ( path == n_trans[trans_num].local_nettask_n_path_num )
            {
               if ( ( n_trans[trans_num].trans_state == ANT__FULL_T ) ||
                 ( n_trans[trans_num].trans_state == ANT__THIS_T ) )
               {

/*   Construct the message and send it to the task */

                  t_close.mess_in_type = C_REM_GSOC_END_IN;
                  t_close.u.rem_msg_in.local_task_t_trans_num =
                    n_trans[trans_num].local_task_t_num;
                  t_close.u.rem_msg_in.local_nettask_n_trans_num = trans_num;
                  t_close.u.rem_msg_in.gsoc_flag = OBEY;
                  strcpy ( t_close.u.rem_msg_in.gsoc_name, "NETDISCON" );
                  t_close.u.rem_msg_in.gsoc_len = 26;
                  t_close.u.rem_msg_in.gsoc_status = ANT__NETSHUT;
                  strcpy ( t_close.u.rem_msg_in.gsoc_value,
                    "network call disconnected" );

                  istat = SAI__OK;
                  msp_send_message ( (char *)&t_close, C_REM_MSG_IN_LEN,
                    n_trans[trans_num].local_task_ack_q, command_q, &istat );
               }

/*   Close the transaction */

               n_trans[trans_num].local_nettask_n_path_num =
                 MESSYS__NULL_P;
               n_trans[trans_num].local_task_t_num = MESSYS__NULL_T;
               n_trans[trans_num].remote_nettask_n_trans_num =
                 MESSYS__NULL_T;
               n_trans[trans_num].trans_state = ANT__NULL_T;
            }
         }

/*   Close the path */

         p_close.mess_in_type = C_REM_DEINIT_IN;
         p_close.u.rem_deinit_in.local_task_t_path_num =
           n_paths[path].local_task_t_path_num;

         istat = SAI__OK;
         if ( n_paths[path].path_state == ANT__THIS_P )
         {
            msp_send_message ( (char *)&p_close, C_REM_DEINIT_IN_LEN,
              n_paths[path].local_task_reminit_ack_q, command_q, &istat );
         }
         else if ( n_paths[path].path_state == ANT__FULL_P )

         {
            msp_send_message ( (char *)&p_close, C_REM_DEINIT_IN_LEN,
              n_paths[path].local_task_q, command_q, &istat );
         }

         n_paths[path].local_taskname[0] = '\0';
         n_paths[path].remote_taskname[0] = '\0';
         n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
         n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
         n_paths[path].local_machine_num = MESSYS__NULL_M;
         n_paths[path].path_state = ANT__NULL_P;
      }
   }

}



void ant_exit
(
/* int *status  */                       /* global status (give and returned) */
void
)

/*
*+
*  Name:
*     ANT_EXIT

*  Purpose:
*     The ADAMNET exit handler

*  Language:
*     Starlink C

*  Algorithm:
*     Search the lists of PATHs and TRANSACTIONs and send deinit
*     messages to all the relevant queues. It is not necessary to send
*     messages to the remote machine because the network itself should
*     report the exit of this process to ADAMNET on the other machine.
*     Close the listen socket.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     15-FEB-1990 (REVAD::BDK):
*        Original
*     11-JAN-1996: Don't send DE_INIT on each transaction - just on each open
*                 path (BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   struct a_mess_in outbuf;   /* structure for sending to local task */
   int path;                  /* path number */
   int trans_num;             /* transaction number */
   int istat;                 /* local status */



/*   Search for active paths and send deinit messages to the target
     task's command queue */

   for ( path=0; path<MESSYS__MXPATH; path++ )
   {
      if ( ( n_paths[path].path_state == ANT__FULL_P ) ||
        ( n_paths[path].path_state == ANT__THIS_P ) )
      {
         outbuf.mess_in_type = C_REM_DEINIT_IN;
         outbuf.u.rem_deinit_in.local_task_t_path_num =
           n_paths[path].local_task_t_path_num;
         istat = SAI__OK;
         msp_send_message ( (char *)&outbuf, C_REM_DEINIT_IN_LEN,
           n_paths[path].local_task_q, command_q, &istat );
      }
   }

   close ( listen_channel );

}



void ant_forward_ack_in
(
int *status                      /* global status (given and returned) */
)

/*
*+
*  Name:
*     ANT_FORWARD_ACK_IN

*  Purpose:
*     Forward a network message to the target task

*  Language:
*     Starlink C

*  Algorithm:
*     Given a NET_MSG_IN, forward it to the target task and make
*     the necessary entries in the common blocks. In the event of
*     failure, do what tidying is possible.
*     Check the indicated path and transaction number are valid.
*     Complete the N_TRANS entry if necessary.
*     Send the message to the target task on this machine.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Stop use of GSOC_ACK structures
*     09-JUN-1988: remove unused variables, calculate LENGTH
*                  (REVAD::BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;   /* structure for sending to local task */
   int path;                  /* path number for new entry */
   int trans_num;             /* transaction number */
   int length;                /* length of message */
   int ntrans;                /* transaction number in network format */
   int nstatus;               /* message status in network format */
   int nlen;                  /* value length in network format */
   int nflag;                 /* message flag in network format */


   if ( *status != SAI__OK ) return;


/*   Check the entries in the tables */

   memcpy ( &ntrans, m_rtrans, 4);
   trans_num = ntohl ( ntrans );

   if ( ( trans_num >= 0 ) && ( trans_num < MESSYS__MXTRANS ) )
   {
      path = n_trans[trans_num].local_nettask_n_path_num;
   }
   else
   {

/*   Should be impossible, but avoid indexing outside array. */

      path = MESSYS__NULL_P;
   }

   if ( ( path != MESSYS__NULL_P ) &&
     ( n_paths[path].path_state == ANT__FULL_P ) )
   {
      if ( n_trans[trans_num].trans_state != ANT__FULL_T )
      {

/*   Complete the transaction details. */

         memcpy ( &ntrans, m_strans, 4 );
         n_trans[trans_num].remote_nettask_n_trans_num =
           ntohl ( ntrans );
         n_trans[trans_num].trans_state = ANT__FULL_T;
      }

/*   Construct the message and send it to the task */

      outbuf.mess_in_type = C_REM_MSG_IN;
      outbuf.u.rem_msg_in.local_task_t_trans_num =
        n_trans[trans_num].local_task_t_num;
      outbuf.u.rem_msg_in.local_nettask_n_trans_num = trans_num;
      memcpy ( &nflag, m_flag, 4 );
      outbuf.u.rem_msg_in.gsoc_flag = ntohl ( nflag );
      strcpy ( outbuf.u.rem_msg_in.gsoc_name, m_name );
      memcpy ( &nlen, m_len, 4 );
      outbuf.u.rem_msg_in.gsoc_len = ntohl ( nlen );
      ANT_CHECKLEN(outbuf.u.rem_msg_in.gsoc_len);
      memcpy ( &nstatus, m_status, 4 );
      outbuf.u.rem_msg_in.gsoc_status = ntohl ( nstatus );
      memcpy ( outbuf.u.rem_msg_in.gsoc_value, m_value,
        outbuf.u.rem_msg_in.gsoc_len  );
      length = C_REM_MSG_IN_LEN - MSG_VAL_LEN +
        outbuf.u.rem_msg_in.gsoc_len;

      msp_send_message ( (char *)&outbuf, length,
        n_trans[trans_num].local_task_ack_q, command_q, status );

   }
   else if ( path == MESSYS__NULL_P )
   {

/*   The path is nonexistent. This means that the communication route
     back to the other machine is also unknown.
     Trust that this has happened because the incoming message has
     crossed with an outgoing DEINIT. So do nothing. */
   }
   else
   {

/*  The path is partly installed. Something has got out of step.
    This should be impossible unless it has arisen by the PATH
    number being reused. In which case, the incoming message has
    crossed with an outgoing DEINIT message. The entry in the
    N_PATH structure doesn't have anything to do with the incoming
    message, so DO NOTHING. */


   }

}



void ant_forward_end_in
(
int *status                       /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_FORWARD_END_IN

*  Purpose:
*     Forward a net END_IN message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a NET_GSOC_END_IN, forward it to the target task and make
*     the necessary entries in the common blocks.
*     Check the indicated path and transaction number are valid.
*     Send the GSOC_END to the target task on this machine.
*     Remove the transaction.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     04-APR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Stop using GSOC_ACK structures
*     09-JUN-1988: remove unused variables, calculate LENGTH
*                  (REVAD::BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;   /* structure for sending to local task */
   int path;                  /* path number for new entry */
   int trans_num;             /* transaction number */
   int length;                /* length of message */
   int ntrans;                /* transaction number in network format */
   int nstatus;               /* message status in network format */
   int nlen;                  /* value length in network format */
   int nflag;                 /* message flag in network format */


   if ( *status != SAI__OK ) return;


/*   Check the entries in the tables */

   memcpy ( &ntrans, m_rtrans, 4 );
   trans_num = ntohl ( ntrans );
   if ( ( trans_num >= 0 ) && ( trans_num < MESSYS__MXTRANS ) )
   {
      path = n_trans[trans_num].local_nettask_n_path_num;
   }
   else
   {

/*   Should be impossible, but avoid addressing outside array. */

      path = MESSYS__NULL_P;
   }

   if ( ( path != MESSYS__NULL_P ) &&
     ( n_paths[path].path_state == ANT__FULL_P ) )
   {
      if ( ( n_trans[trans_num].trans_state == ANT__FULL_T ) ||
        ( n_trans[trans_num].trans_state == ANT__THIS_T ) )
      {

/*   Construct the message and send it to the task */

         outbuf.mess_in_type = C_REM_GSOC_END_IN;
         outbuf.u.rem_msg_in.local_task_t_trans_num =
           n_trans[trans_num].local_task_t_num;
         outbuf.u.rem_msg_in.local_nettask_n_trans_num = trans_num;
         memcpy ( &nflag, m_flag, 4 );
         outbuf.u.rem_msg_in.gsoc_flag = ntohl ( nflag );
         strcpy ( outbuf.u.rem_msg_in.gsoc_name, m_name );
         memcpy ( &nlen, m_len, 4 );
         outbuf.u.rem_msg_in.gsoc_len = ntohl ( nlen );
      ANT_CHECKLEN(outbuf.u.rem_msg_in.gsoc_len);
         memcpy ( &nstatus, m_status, 4 );
         outbuf.u.rem_msg_in.gsoc_status = ntohl ( nstatus );
         memcpy ( outbuf.u.rem_msg_in.gsoc_value, m_value,
           outbuf.u.rem_msg_in.gsoc_len );
         length = C_REM_MSG_IN_LEN - MSG_VAL_LEN +
           outbuf.u.rem_msg_in.gsoc_len;

         msp_send_message ( (char *)&outbuf, length,
           n_trans[trans_num].local_task_ack_q, command_q, status );
      }

/*   Annul the transaction */

      n_trans[trans_num].local_nettask_n_path_num = MESSYS__NULL_P;
      n_trans[trans_num].local_task_t_num = MESSYS__NULL_T;
      n_trans[trans_num].remote_nettask_n_trans_num = MESSYS__NULL_T;
      n_trans[trans_num].trans_state = ANT__NULL_T;

   }
   else if ( path == MESSYS__NULL_P )
   {

/*   The path is nonexistent. This means that the communication route
     back to the other machine is also unknown.
     Trust that this has happened because the incoming message has
     crossed with an outgoing DEINIT. So do nothing. */

   }
   else
   {

/*   The path is partly installed. Something has got out of step.
     This should be impossible unless it has arisen by the PATH
     number being reused. In which case, the incoming message has
     crossed with an outgoing DEINIT message. The entry in the
     N_PATH structure doesn't have anything to do with the incoming
     message, so DO NOTHING. */

   }

}



void ant_forward_start_in
(
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_FORWARD_START_IN

*  Purpose:
*     Forward a net GSOC start message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a NET_GSOC_START, forward it to the target task and make the
*     necessary entries in the common blocks. In the event of failure,
*     return a GSOC_END message across the network.
*     Check the indicated path is valid
*     Search the common blocks for an unused N_TRANS entry.
*     Put partial information into it from the received message.
*     Send the GSOC_START to the target task on this machine.
*     On error, return a GSOC_END message across the network if
*     relevant.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1988 (REVAD::BDK):
*        Original
*     30-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK structures
*     09-JUN-1988 (REVAD::BDK):
*        Check PATH in range, calculate LENGTH
*     04-JUL-1991: for failure, set NET_OUT_TYPE to C_NET_GSOC_END_OUT
*                  (REVAD::BDK)
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;    /* structure for sending to local task */
   int path;                   /* path number for new entry */
   int npath;                  /* path number in network format */
   int found;                  /* loop controller */
   int machnum;                /* machine number */
   int trans_num;              /* transaction number */
   int ntrans;                 /* transaction number in network format */
   int length;                 /* length of message */
   int nstatus;                /* message status in network format */
   int ntype;                  /* message type in network format */
   int nflag;                  /* message flag in network format */
   int nlen;                   /* value length in network format */


/*   space for copy of incoming message, needed only if a rejection is to
     be returned */

   int in_strans;
   int in_rpath;
   int in_flag;
   char in_name[MSG_NAME_LEN];
   int in_len;
   char in_value[MSG_VAL_LEN];



   if ( *status != SAI__OK ) return;


/*   Make a partial entry in the tables */

   memcpy ( &npath, g_rpath, 4 );
   path = ntohl ( npath );

   if ( ( path >=0 ) && ( path < MESSYS__MXPATH ) )
   {

      if ( n_paths[path].path_state == ANT__FULL_P )
      {

/*   Look for a free transaction slot. */

         found = 0;

         for ( trans_num=0; trans_num<MESSYS__MXTRANS; trans_num++ )
         {
            if ( n_trans[trans_num].trans_state == ANT__NULL_T )
            {
               found = 1;
               break;
            }
         }

         if ( found == 1 )
         {
            n_trans[trans_num].trans_state = ANT__OTHER_T;
            n_trans[trans_num].local_nettask_n_path_num = path;
            memcpy ( &ntrans, g_strans, 4 );
            n_trans[trans_num].remote_nettask_n_trans_num =
              ntohl ( ntrans );

/*   Construct the message and send it to the task */

            outbuf.mess_in_type = C_REM_GSOC_START_IN;
            outbuf.u.rem_gsoc_start_in.local_task_t_path_num =
              n_paths[path].local_task_t_path_num;
            outbuf.u.rem_gsoc_start_in.local_nettask_n_trans_num =
              trans_num;
            memcpy ( &nflag, g_flag, 4 );
            outbuf.u.rem_gsoc_start_in.gsoc_flag = ntohl ( nflag );
            strcpy ( outbuf.u.rem_gsoc_start_in.gsoc_name, g_name );
            memcpy ( &nlen, g_len, 4 );
            outbuf.u.rem_gsoc_start_in.gsoc_len = ntohl ( nlen );
            ANT_CHECKLEN(outbuf.u.rem_gsoc_start_in.gsoc_len);
            memcpy ( outbuf.u.rem_gsoc_start_in.gsoc_value, g_value,
              outbuf.u.rem_gsoc_start_in.gsoc_len );
            length = C_REM_GSOC_START_IN_LEN - MSG_VAL_LEN +
              outbuf.u.rem_gsoc_start_in.gsoc_len;

            msp_send_message ( (char *)&outbuf, length,
              n_paths[path].local_task_q, command_q, status );
         }
         else
         {

/*   Reject the new transaction by sending a GSOC end. */

            ntype = htonl ( C_NET_GSOC_END_OUT );
            ntrans = htonl ( MESSYS__NULL_T );
            nstatus = htonl ( ANT__REMESNUM );

/*   Copy the contents of the netbuffer before filling it with the reply */

            memcpy ( &in_strans, g_strans, 4 );
            memcpy ( &in_rpath, g_rpath, 4 );
            memcpy ( &in_flag, g_flag, 4 );
            strcpy ( in_name, g_name );
            memcpy ( &in_len, g_len, 4 );
            memcpy ( in_value, g_value, MSG_VAL_LEN );

/*   Build the reply */

            memcpy ( netbuffer, &ntype, 4 );
            memcpy ( m_strans, &ntrans, 4 );
            memcpy ( m_rtrans, &in_strans, 4 );
            memcpy ( m_flag, &in_flag, 4 );
            strcpy ( m_name, in_name );
            memcpy ( m_len, &in_len, 4 );
            memcpy ( m_status, &nstatus, 4 );
            memcpy ( m_value, in_value, MSG_VAL_LEN );
            machnum = n_paths[path].local_machine_num;

            sock_write ( n_mach[machnum].local_channel,
              C_NET_MAXMSG_LEN, netbuffer, status );
         }

      }
      else
      {

/*   Shouldn't happen. The message must have crossed with a DEINIT
     and the path number in it is no longer relevant to this NETTASK.
     DO NOTHING. */

      }

   }
   else
   {

/*   Shouldn't happen. Can't do anything in the way of sending
     messages. */

      *status = ANT__IVPATH;

   }

}



void ant_init
(
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_INIT

*  Purpose:
*     Initialise the ANT library

*  Language:
*     Starlink C

*  Algorithm:
*     Find the name of this machine and store it.
*     Initialise the global arrays. Get the local IP address.
*     Set up the i/o signal handler.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     07-APR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Check for SS$_NORMAL
*     18-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     15-APR-1996 (BDK):
*        Use sigaction() to handle SIGIO
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   int machnum;                /* index to machine entries */
   int path;                   /* index to path entries */
   int trans_num;              /* index to transaction entries */
/*   struct hostent myhostentstruct; */ /* network data structure */
   struct sigaction ioaction;  /* structure for io signal handling */


   if ( *status != SAI__OK ) return;


/*   Set up catch-all signal/exit handling */

   ant_setsig();

/*   Store this machine's name and IP address */

   gethostname ( this_machine, MESSYS__MNAME );
/*   sock_ghbn ( "localhost", &myhostentstruct, status ); */


/*   Initialise the global data */

   for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
   {
      n_mach[machnum].machine_names[0] = '\0';
      n_mach[machnum].local_channel = 0;
      n_mach[machnum].remote_machine_num = MESSYS__NULL_M;
      n_mach[machnum].mach_state = ANT__NULL_MACH;
   }

   for ( path=0; path<MESSYS__MXPATH; path++ )
   {
      n_paths[path].local_taskname[0] = '\0';
      n_paths[path].remote_taskname[0] = '\0';
      n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
      n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
      n_paths[path].local_machine_num = MESSYS__NULL_M;
      n_paths[path].path_state = ANT__NULL_P;
   }

   for ( trans_num=0; trans_num<MESSYS__MXTRANS; trans_num++ )
   {
      n_trans[trans_num].local_nettask_n_path_num = MESSYS__NULL_P;
      n_trans[trans_num].local_task_t_num = MESSYS__NULL_T;
      n_trans[trans_num].remote_nettask_n_trans_num = MESSYS__NULL_T;
      n_trans[trans_num].trans_state = ANT__NULL_T;
   }

/*   Pointers into network messages */

/*   INIT messages */

   i_stask = &(netbuffer[4]);
   i_rtask = &(netbuffer[4+MESSYS__TNAME]);
   i_smach = &(netbuffer[4+2*MESSYS__TNAME]);
   i_rmach = &(netbuffer[4+2*MESSYS__TNAME+MESSYS__MNAME]);
   i_snum = &(netbuffer[4+2*MESSYS__TNAME+2*MESSYS__MNAME]);
   i_rnum = &(netbuffer[4+2*MESSYS__TNAME+2*MESSYS__MNAME+4]);
   i_spath = &(netbuffer[4+2*MESSYS__TNAME+2*MESSYS__MNAME+8]);

/*   GSOC messages */

   g_strans = &(netbuffer[4]);
   g_rpath = &(netbuffer[8]);
   g_flag = &(netbuffer[12]);
   g_name = &(netbuffer[16]);
   g_len = &(netbuffer[16+MSG_NAME_LEN]);
   g_value = &(netbuffer[20+MSG_NAME_LEN]);

/*   MSG messages */

   m_strans = &(netbuffer[4]);
   m_rtrans = &(netbuffer[8]);
   m_flag = &(netbuffer[12]);
   m_name = &(netbuffer[16]);
   m_len = &(netbuffer[16+MSG_NAME_LEN]);
   m_status = &(netbuffer[20+MSG_NAME_LEN]);
   m_value = &(netbuffer[24+MSG_NAME_LEN]);

/*   DEINIT messages */

   d_rpath = &(netbuffer[4]);

/*   ACK messages */

   k_rpath = &(netbuffer[4]);
   k_spath = &(netbuffer[8]);

/*   CALL messages */

   c_rmach = &(netbuffer[4]);
   c_smach = &(netbuffer[4+MESSYS__MNAME]);
   c_snum = &(netbuffer[4+2*MESSYS__MNAME]);

/*   ACCEPT messages */

   a_rmach = &(netbuffer[4]);
   a_smach = &(netbuffer[4+MESSYS__MNAME]);
   a_rnum = &(netbuffer[4+2*MESSYS__MNAME]);
   a_snum = &(netbuffer[8+2*MESSYS__MNAME]);


/*   Setup the signal handler */

   ioaction.sa_sigaction = ant_sighdlr;
   sigemptyset ( &ioaction.sa_mask );
   ioaction.sa_flags = 0;
   sigaction ( SIGIO, &ioaction, NULL );

/*   Initialize into ADAM low-level intertask communication system */

   msp_enter_task ( this_machine, &command_q, status );

/*   Create a queue for receiving messages forwarded from the
     signal-handling routine. */

   msp_create_localq ( &networks_q, &networkr_q, status );

/*   Initialise to TCP/IP and start an asynchronous listen on the port */

   ant_listen ( status );

/*   Setup the exit handler */

   atexit ( ant_exit );

}




void ant_listen
(
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_LISTEN

*  Purpose:
*     Start an asynchronous connection acceptance

*  Language:
*     Starlink C

*  Algorithm:
*     Create a socket for listening for incoming connection requests on
*     the adamnet well-known port. Set the socket to deliver SIGIO if a
*     request arrives.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     14-APR-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int found;                       /* loop controller */
   int machnum;                     /* index to machine details */
   struct sockaddr_in connect_addr; /* description of socket for incoming
                                       connection requests */
   int optval;                      /* flag for setting socket options */
   int istat;                       /* local status */


   if ( *status != SAI__OK ) return;


/*   Get a free slot for machine details */

   found = 0;
   for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
   {
      if ( n_mach[machnum].mach_state == ANT__NULL_MACH )
      {
         found = 1;
         break;
      }
   }

   if ( found == 0 )
   {
      *status = ANT__TOOMANY;
   }
   else
   {
/*   Create the connection socket */

      listen_channel = socket ( AF_INET, SOCK_STREAM, 0 );
      if ( listen_channel < 0 )
      {
         perror ( "adamnet failed to create listen channel" );
         *status = ANT__NETSHUT;
      }

      optval = 1;
      istat = setsockopt ( listen_channel, SOL_SOCKET, SO_REUSEADDR,
        (char *)&optval, sizeof(optval) );
      if ( istat != 0 )
      {
         perror ( "adamnet failed to set option on listen channel" );
         *status = ANT__NETSHUT;
      }

/*   Set-up the addressing */

      memset ( &connect_addr, 0, sizeof(connect_addr) );
      connect_addr.sin_family = AF_INET;
      connect_addr.sin_addr.s_addr = htonl ( INADDR_ANY );
      connect_addr.sin_port = htons ( ANT__PORTNUM );
      istat = bind ( listen_channel, (struct sockaddr *) &connect_addr,
        sizeof(connect_addr) );

      if ( istat == 0 )
      {
         istat = listen ( listen_channel, 5 );
         if ( istat == 0 )
         {

/*   enable the new network channel for signals */

            istat = fcntl ( listen_channel, F_SETOWN, getpid() );
            if ( istat == 0 )
            {
               istat = fcntl ( listen_channel, F_SETFL, FASYNC );
               if ( istat != 0 )
               {
                  perror ( "adamnet failed to FASYNC on listen channel" );
                  *status = ANT__NETSHUT;
               }
            }
            else
            {
               perror ( "adamnet failed to SETOWN on listen channel" );
               *status = ANT__NETSHUT;
            }
         }
         else
         {
            perror ( "adamnet failed to listen" );
            *status = ANT__NETSHUT;
         }
      }
      else
      {
         perror ( "adamnet failed to bind listen socket" );
         *status = ANT__NETSHUT;
      }
   }
}


void ant_netmsg
(
int machnum,         /* index to the received message (given) */
int *status          /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_NETMSG

*  Purpose:
*     Handle messages received across the network

*  Language:
*     Starlink C

*  Algorithm:
*     Read the message.
*     Identify message type and pass the relevant structure component to
*     the corresponding routine.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     29-MAR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK structures
*     18-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   int messtype;        /* message type in host format */
   int intype;          /* message type in network format */

   if ( *status != SAI__OK ) return;

   sock_read ( n_mach[machnum].local_channel, C_NET_MAXMSG_LEN,
     netbuffer, status );

/*   Extract the message type field and convert from network format */

   if ( *status == SAI__OK )
   {

      memcpy ( (char *)&intype, netbuffer, 4 );
      messtype = ntohl ( intype );

      if ( messtype == C_NET_INIT_IN )
      {
         ant_accept_netinit ( machnum, status );
      }
      else if ( messtype == C_NET_ACK_IN )
      {
         ant_accept_netack ( status );
      }
      else if ( messtype == C_NET_DEINIT_IN )
      {
         ant_accept_netdeinit ( status );
      }
      else if ( messtype == C_NET_GSOC_START_IN )
      {
         ant_forward_start_in ( status );
      }
      else if ( messtype == C_NET_MSG_IN )
      {
         ant_forward_ack_in ( status );
      }
      else if ( messtype == C_NET_GSOC_END_IN )
      {
         ant_forward_end_in ( status );
      }
      else
      {
         *status = ANT__IVNETIN;
      }

   }
   else
   {

/*   Bad status on connection to that machine */

      *status = SAI__OK;
      ant_discon ( machnum, status );
   }

}



void ant_network
(
int *status          /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_NETWORK

*  Purpose:
*     Handle messages received across the network

*  Language:
*     Starlink C

*  Algorithm:
*     Search for any messages present on the existing network sockets and
*     process them.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     25-APR-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int machnum;            /* index to machine details */
   fd_set read_mask;       /* read mask for select() */
   struct timeval wait;    /* select() timeout structure */
   int nready;             /* no of inputs available */

   if ( *status != SAI__OK ) return;


   for ( ; ; )
   {

/*   See if data present on any of the open sockets */

      FD_ZERO ( &read_mask );
      FD_SET ( listen_channel, &read_mask );
      for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
      {
         if ( n_mach[machnum].mach_state != ANT__NULL_MACH )
         {
            FD_SET ( n_mach[machnum].local_channel, &read_mask );
         }
      }

/*   Going to poll select() with zero timeout */

      wait.tv_sec = 0;
      wait.tv_usec = 0;
      nready = select ( FD_SETSIZE, &read_mask, (fd_set *)0, (fd_set *)0,
        &wait );

      if ( nready > 0 )
      {

/*   Find the first message and process it */

         if ( FD_ISSET ( listen_channel, &read_mask ) )
         {
            ant_accept ( status );
         }
         else
         {
            for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
            {
               if ( n_mach[machnum].mach_state != ANT__NULL_MACH )
               {
                  if ( FD_ISSET ( n_mach[machnum].local_channel,
                    &read_mask ) )
                  {
                     ant_netmsg ( machnum, status );
                  }
               }
            }
         }
      }
      else
      {
/*   no more messages */

         break;
      }
   }

}




void ant_obey
(
struct a_loc_gsoc_start_out loc_gsoc_start_out, /* the incoming message
                                                 (given) */
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_OBEY

*  Purpose:
*     Obey a command to dump diagnostics

*  Language:
*     Starlink C

*  Algorithm:
*     A message has been received requesting ADAMNET to output
*     diagnostics.
*     Write the contents of the ADAMNET common blocks to a text file.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     18-APR-1988 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   FILE *fd;             /* descriptor for output file */
   int found;            /* flag for existence of output data */
   int machnum;          /* counter for machine numbers */
   int path;             /* counter for paths */


   if ( *status != SAI__OK ) return;


/*   Open reporting file */

   fd = fopen ( "adamnet_report", "w" );

/*   Details of known machines */

   fprintf ( fd, "ADAMNET REPORT\n" );
   fprintf ( fd, "\n" );
   fprintf ( fd, "\n" );
   fprintf ( fd, "\n" );
   fprintf ( fd, "Known machines\n" );
   fprintf ( fd, "\n" );

   found = 0;
   for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
   {
      if ( n_mach[machnum].mach_state != ANT__NULL_MACH )
      {
         found = 1;
         fprintf ( fd, "%s  %d  %d  %d\n",
           n_mach[machnum].machine_names,
           n_mach[machnum].local_channel,
           n_mach[machnum].remote_machine_num,
           n_mach[machnum].mach_state );
      }
   }
   if ( found == 0 )
   {
      fprintf ( fd, "No machines known\n" );
   }
   fprintf ( fd, "\n" );
   fprintf ( fd, "\n" );
   fprintf ( fd, "\n" );
   fprintf ( fd, "Paths known\n" );

   found = 0;
   for ( path=0; path<MESSYS__MXPATH; path++ )
   {
      if ( n_paths[path].path_state != ANT__NULL_P )
      {
         found = 1;
         fprintf ( fd, "%s  %s %d\n",
           n_paths[path].local_taskname,
           n_paths[path].remote_taskname,
           n_paths[path].path_state );
      }
   }
   if ( found == 0 )
   {
      fprintf ( fd, "No paths known\n" );
   }

/*   Close the report file */

   fclose ( fd ) ;

}



void ant_opennet
(
char *remote_machine_name,  /* name of other machine (given) */
sendq_type accept_q,        /* queue for sending acknowledgement to task
                               on this machine (given) */
int *status                 /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_OPENNET

*  Purpose:
*     Initiate communication with a task across a network

*  Language:
*     Starlink C

*  Algorithm:
*     Initiate the call.
*     If the connection request was successful return an
*     ACK_INIT message to the ADAM task on this machine which requested
*     the path. Start a read on the newly-opened network channel.
*     If the connection request was rejected, return a rejection message
*     to the task which requested the path.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     25-MAR-1988 (REVAD::BDK):
*        Original
*     07-APR-1988 (REVAD::BDK):
*        Set-up N_MACH(I).LOCAL_UNIT
*     25-APR-1988 (REVAD::BDK):
*        Assume '::' is in MACHINE_NAME
*     26-APR-1988 (REVAD::BDK):
*        Use MESSYS__NETNAME
*     20-MAY-1988 (REVAD::BDK):
*        Improve error trapping
*     09-JUN-1988 (REVAD::BDK):
*        Set MACH_STATE to ANT__THIS_START
*     15-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   struct a_mess_in mess_in;   /* response sent to local task */
   int found;                  /* search loop controller */
   int machnum;                /* machine number */
   char *endname;              /* end of machine name */
   char rmach[MESSYS__MNAME];  /* name of remote machine */


   if ( *status != SAI__OK ) return;


/*   Search for spare space for connection information */

   found = 0;
   for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
   {
      if ( n_mach[machnum].mach_state == ANT__NULL_MACH )
      {
         found = 1;
         break;
      }
   }

   if ( found == 0 )
   {
      *status = ANT__TOOMANY;
   }
   else
   {

/*   Translate the remote machine name into an IP address. */

      endname = strstr ( remote_machine_name, "!!" );
      *endname = '\0';
      strcpy ( rmach, remote_machine_name );
      *endname = '!';

/*   Make the network connection */

      ant_connect ( rmach, &(n_mach[machnum].local_channel), status );

      if ( *status == SAI__OK )
      {
         strcpy ( n_mach[machnum].machine_names, remote_machine_name );

/*   Mark the entry in COMMON as complete
     Build an acceptance message */

         n_mach[machnum].mach_state = ANT__THIS_INIT;
         mess_in.mess_in_type = C_REM_ACCEPT_IN;
         mess_in.u.rem_accept_in.accept_status = SAI__OK;

      }
      else
      {

/*   Build a rejection message
     Release the local machine number */

         mess_in.mess_in_type = C_REM_ACCEPT_IN;
         mess_in.u.rem_accept_in.accept_status = ANT__REJECT;
         n_mach[machnum].machine_names[0] = '\0';
         n_mach[machnum].local_channel = 0;
         n_mach[machnum].remote_machine_num = MESSYS__NULL_M;
         n_mach[machnum].mach_state = ANT__NULL_MACH;
      }

/*   Send the acknowledgement to the local task */

      msp_send_message ( (char *)&mess_in, C_REM_ACCEPT_IN_LEN,
        accept_q, command_q, status );

   }

}



static void ant_retarget
(
int signo,               /* no. of signal to be retargetted (given) */
struct sigaction oact    /* action structure with flag initialised
                            (given) */
)

/*
*+
*  Name:
*     ANT_RETARGET

*  Purpose:
*     Retarget a signal to the ant handling routine

*  Language:
*     Starlink C

*  Algorithm:
*     If ant has not already been set to handle this particular signal,
*     inquire the function handler for the signal and store it in the list
*     of ant signal handlers. Declare ant_exhdlr as the new handler for
*     the signal.

*  Authors:
*     Brian McIlwrath (Starlink)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1996 (BDK):
*        Taken from the DTASK signal handler
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct siglist *entry;
   struct siglist *new_entry;


   if ( ( sigisset >> signo ) & 1 )
   {

 /* Signal handler already processed */

     return;
   }

   if ( oact.sa_handler != SIG_DFL )
   {

/* Create an entry to add to the list */

      new_entry = (struct siglist *) malloc ( sizeof(struct siglist) );

      if ( new_entry == NULL )
      {
         perror ( "ant_retarget - malloc error" );
         exit(1);
      }
      new_entry->signo = signo;
      oact.sa_flags |= SA_RESTART;
      new_entry->act   = oact;
      new_entry->next  = NULL;

/* Find the end of our linked list */

      if ( stack_top != NULL )
      {
         for ( entry = stack_top; entry->next != NULL; entry=entry->next )
           /* Null statement */;
         entry->next = new_entry;
      }
      else
      {
         stack_top = new_entry;
      }
   }

/* Retarget the signal handler towards ant_exhdlr - leave all flags etc
   as they were  */

   oact.sa_sigaction = ant_exhdlr;
   sigaction ( signo, &oact, NULL );

/* Mark signal handler set in bitmask */

   sigisset |= (1<<signo);

}



void ant_send_ack_out
(
struct a_rem_msg_out rem_msg_out, /* the received message (given) */
sendq_type reply_q,               /* task reply queue (given) */
int *status                       /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_SEND_ACK_OUT

*  Purpose:
*     Send a net ACK_OUT message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a REM_MSG_OUT, send it to the other machine and make
*     the necessary entries in the global structures. In the event of
*     failure, do what tidying is possible.
*     Check the indicated path and transaction number are valid.
*     Complete the N_TRANS entry if necessary.
*     Send the message to the other machine.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-APR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK structures
*     09-JUN-1988: check TRANS_NUM in range, calculate LENGTH
*                  (REVAD::BDK)
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;    /* structure for sending to local task */
   int path;                   /* path number for new entry */
   int trans_num;              /* transaction number */
   int machnum;                /* machine number */
   int length;                 /* message length */
   int nflag;                  /* message flag in network format */
   int ntype;                  /* message type in network format */
   int ntrans;                 /* transaction number in network format */
   int nlen;                   /* value length in network format */
   int nstatus;                /* message status in network format */


   if ( *status != SAI__OK ) return;


/*   Check the entries in the tables */

   trans_num = rem_msg_out.local_nettask_n_trans_num;
   if ( ( trans_num >= 0 ) && ( trans_num < MESSYS__MXTRANS ) )
   {
      path = n_trans[trans_num].local_nettask_n_path_num;
   }
   else
   {
      path = MESSYS__NULL_P;
   }

   if ( ( path != MESSYS__NULL_P ) &&
     ( n_paths[path].path_state == ANT__FULL_P ) )
   {
      if ( n_trans[trans_num].trans_state == ANT__OTHER_T )
      {

/*   Complete the transaction details. */

         n_trans[trans_num].local_task_ack_q = reply_q;
         n_trans[trans_num].local_task_t_num =
           rem_msg_out.local_task_t_trans_num;
         n_trans[trans_num].trans_state = ANT__FULL_T;
      }

/*   Construct the message and send it to the other machine */

      ntype = htonl ( C_NET_MSG_OUT );
      memcpy ( netbuffer, &ntype, 4 );
      ntrans = htonl ( trans_num );
      memcpy ( m_strans, &ntrans, 4 );
      ntrans = htonl ( n_trans[trans_num].remote_nettask_n_trans_num );
      memcpy ( m_rtrans, &ntrans, 4 );
      nflag = htonl ( rem_msg_out.gsoc_flag );
      memcpy ( m_flag, &nflag, 4 );
      strcpy ( m_name, rem_msg_out.gsoc_name );
      ANT_CHECKLEN(rem_msg_out.gsoc_len);
      nlen = htonl ( rem_msg_out.gsoc_len );
      memcpy ( m_len, &nlen, 4 );
      nstatus = htonl ( rem_msg_out.gsoc_status );
      memcpy ( m_status, &nstatus, 4 );
      memcpy ( m_value, rem_msg_out.gsoc_value, rem_msg_out.gsoc_len );
      machnum = n_paths[path].local_machine_num;

      sock_write ( n_mach[machnum].local_channel,
        C_NET_MAXMSG_LEN, netbuffer, status );

   }
   else
   {

/*   Return a GSOC_END to the task's acknowledgement queue.
     Presumably the path is in the process of closing down and the
     task's command_q will have already received a message, but the
     acknowledgement queue for this transaction may have been unknown
     at the time the DEINIT was issued. */

      outbuf.mess_in_type = C_REM_GSOC_END_IN;
      outbuf.u.rem_msg_in.local_task_t_trans_num =
        rem_msg_out.local_task_t_trans_num;
      outbuf.u.rem_msg_in.local_nettask_n_trans_num =
        rem_msg_out.local_nettask_n_trans_num;
      outbuf.u.rem_msg_in.gsoc_flag = rem_msg_out.gsoc_flag;
      strcpy ( outbuf.u.rem_msg_in.gsoc_name, rem_msg_out.gsoc_name );
      ANT_CHECKLEN(rem_msg_out.gsoc_len);
      outbuf.u.rem_msg_in.gsoc_len = rem_msg_out.gsoc_len;
      outbuf.u.rem_msg_in.gsoc_status = ANT__NOPATH;
      memcpy ( outbuf.u.rem_msg_in.gsoc_value, rem_msg_out.gsoc_value,
        outbuf.u.rem_msg_in.gsoc_len );
      length = C_REM_MSG_IN_LEN - MSG_VAL_LEN + rem_msg_out.gsoc_len;

      msp_send_message ( (char *)&outbuf, length, reply_q, command_q,
        status );

   }

/*   If the TRANS_NUM was invalid, return a bad status. */

   if ( ( trans_num < 0 ) || ( trans_num >= MESSYS__MXTRANS ) )
   {
      *status = ANT__IVMSG;
   }
}



void ant_send_end_out
(
struct a_rem_msg_out rem_msg_out,  /* the received message, this is a MSG
                                      of type END (given) */
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_SEND_END_OUT

*  Purpose:
*     Send a net END_OUT message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a REM_GSOC_END_OUT, send it to the other machine and make
*     the necessary entries in the common blocks. In the event of
*     failure, do what tidying is possible.
*     Check the indicated path and transaction number are valid.
*     Send the GSOC_END to the other machine.
*     Remove the transaction entry.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     07-APR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK structures
*     09-JUN-1988: check TRANS_NUM in range, calculate LENGTH
*                  (REVAD::BDK)
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int path;                  /* path number for entry */
   int trans_num;             /* transaction number */
   int machnum;               /* machine number */
   int nflag;                  /* message flag in network format */
   int ntype;                  /* message type in network format */
   int ntrans;                 /* transaction number in network format */
   int nlen;                   /* value length in network format */
   int nstatus;                /* message status in network format */


   if ( *status != SAI__OK ) return;


/*   Check the entries in the tables */

   trans_num = rem_msg_out.local_nettask_n_trans_num;
   if ( ( trans_num >= 0 ) && ( trans_num < MESSYS__MXTRANS ) )
   {
      path = n_trans[trans_num].local_nettask_n_path_num;
   }
   else
   {
      path = MESSYS__NULL_P;
   }

   if ( ( path != MESSYS__NULL_P ) &&
     ( n_paths[path].path_state == ANT__FULL_P ) )
   {

/*      Construct the message and send it to the other machine */

      ntype = htonl ( C_NET_GSOC_END_OUT );
      memcpy ( netbuffer, &ntype, 4 );
      ntrans = htonl ( trans_num );
      memcpy ( m_strans, &ntrans, 4 );
      ntrans = htonl ( n_trans[trans_num].remote_nettask_n_trans_num );
      memcpy ( m_rtrans, &ntrans, 4 );
      nflag = htonl ( rem_msg_out.gsoc_flag );
      memcpy ( m_flag, &nflag, 4 );
      strcpy ( m_name, rem_msg_out.gsoc_name );
      ANT_CHECKLEN(rem_msg_out.gsoc_len);
      nlen = htonl ( rem_msg_out.gsoc_len );
      memcpy ( m_len, &nlen, 4 );
      nstatus = htonl ( rem_msg_out.gsoc_status );
      memcpy ( m_status, &nstatus, 4 );
      memcpy ( m_value, rem_msg_out.gsoc_value,
        rem_msg_out.gsoc_len );

      machnum = n_paths[path].local_machine_num;

      sock_write ( n_mach[machnum].local_channel,
        C_NET_MAXMSG_LEN, netbuffer, status );

/*   Remove the transaction details. */

      n_trans[trans_num].local_nettask_n_path_num = MESSYS__NULL_P;
      n_trans[trans_num].local_task_t_num = MESSYS__NULL_T;
      n_trans[trans_num].remote_nettask_n_trans_num = MESSYS__NULL_T;
      n_trans[trans_num].trans_state = ANT__NULL_T;

   }
   else
   {

      if ( ( trans_num >= 0 ) && ( trans_num < MESSYS__MXTRANS ) )
      {

/*   Presumably the path is in the process of closing down.
     DO NOTHING. */

      }
      else
      {
         *status = ANT__IVEND;
      }

   }

}



void ant_send_netack
(
struct a_rem_ack_out rem_ack_out,  /* the received message (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                        /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_SEND_NETACK

*  Purpose:
*     Send a netack message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a REM_ACK, send a NETACK it to the other machine and
*     make the necessary entries in the common blocks. In the event of
*     failure, return a DEINIT to the local task.
*     Complete the relevant N_PATH entry from information in the
*     received message.
*     Forward an acknowledgement message to the other machine.
*     On error, return a DEINIT message to the local task.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Check PATH is in range
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;    /* structure for sending to local task */
   int path;                   /* path number */
   int machnum;                /* machine number */
   int ntype;                  /* message type in network format */
   int npath;                  /* path number in network format */


   if ( *status != SAI__OK ) return;


/*   Complete the entry in the tables */

   path = rem_ack_out.local_nettask_n_path_num;

   if ( ( path >= 0 ) && ( path < MESSYS__MXPATH ) )
   {

      if ( n_paths[path].path_state == ANT__OTHER_P )
      {
         n_paths[path].local_task_t_path_num =
           rem_ack_out.local_task_t_path_num;
         n_paths[path].path_state = ANT__FULL_P;

/*   Build a structure and send it across the network */

         machnum = n_paths[path].local_machine_num;
         ntype = htonl ( C_NET_ACK_OUT );
         memcpy ( netbuffer, &ntype, 4 );
         npath = htonl ( n_paths[path].remote_nettask_n_path_num );
         memcpy ( k_rpath, &npath, 4 );
         npath = htonl ( path );
         memcpy ( k_spath, &npath, 4 );

         sock_write ( n_mach[machnum].local_channel,
           C_NET_MAXMSG_LEN, netbuffer, status );

/*   Bad status ignored. Assume a network EVENT is in the process
     of happening. */

      }
      else
      {

/*   Return a deinit to the task */

         outbuf.mess_in_type = C_REM_DEINIT_IN;
         outbuf.u.rem_deinit_in.local_task_t_path_num =
           rem_ack_out.local_task_t_path_num;
         msp_send_message ( (char *)&outbuf, C_REM_DEINIT_IN_LEN,
           reply_q, command_q, status );

      }

   }
   else
   {

/*   Return a deinit to the task */

      outbuf.mess_in_type = C_REM_DEINIT_IN;
      outbuf.u.rem_deinit_in.local_task_t_path_num =
        rem_ack_out.local_task_t_path_num;
      msp_send_message ( (char *)&outbuf, C_REM_DEINIT_IN_LEN,
        reply_q, command_q, status );

   }

}



void ant_send_netdeinit
(
struct a_rem_deinit_out rem_deinit_out,  /* the received message (given) */
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_SEND_NETDEINIT

*  Purpose:
*     Send a NETDEINIT message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a REM_DEINIT, send a NETDEINIT to the other machine and
*     annul the necessary entries in the common blocks.
*     Look-up the relevant N_PATH entry from information in the
*     received message.
*     Send a deinit message to the other machine.
*     Close msp communications with the local task.
*     Remove the N_PATH entry.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1988 (REVAD::BDK):
*        Original
*     09-JUN-1988 (REVAD::BDK):
*        Remove the transaction entries
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int path;                   /* path number */
   int trans_num;              /* transaction number */
   int machnum;                /* machine number */
   int ntype;                  /* message type in network format */
   int npath;                  /* path number in network format */


   if ( *status != SAI__OK ) return;


/*   Look-up the entry in the tables */

   path = rem_deinit_out.local_nettask_n_path_num;

   if ( ( path >= 0 ) && ( path < MESSYS__MXPATH ) )
   {

      if ( n_paths[path].path_state != ANT__NULL_P )
      {

/*   Build a structure and send it across the network */

         ntype = htonl ( C_NET_DEINIT_OUT );
         memcpy ( netbuffer, &ntype, 4 );
         npath = htonl ( n_paths[path].remote_nettask_n_path_num );
         memcpy ( d_rpath, &npath, 4 );
         machnum = n_paths[path].local_machine_num;
         sock_write ( n_mach[machnum].local_channel,
           C_NET_MAXMSG_LEN, netbuffer, status );

/*   Remove any transaction entries dependent on this PATH. */

         for ( trans_num=0; trans_num<MESSYS__MXTRANS; trans_num++ )
         {
            if ( path == n_trans[trans_num].local_nettask_n_path_num )
            {
               n_trans[trans_num].local_nettask_n_path_num =
                 MESSYS__NULL_P;
               n_trans[trans_num].local_task_t_num = MESSYS__NULL_T;
               n_trans[trans_num].remote_nettask_n_trans_num =
                 MESSYS__NULL_T;
               n_trans[trans_num].trans_state = ANT__NULL_T;
            }
         }

/*   Remove the N_PATH entry */

         msp_close_task_queue ( n_paths[path].local_task_q, status );
         n_paths[path].local_taskname[0] = '\0';
         n_paths[path].remote_taskname[0] = '\0';
         n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
         n_paths[path].remote_nettask_n_path_num = MESSYS__NULL_P;
         n_paths[path].local_machine_num = MESSYS__NULL_M;
         n_paths[path].path_state = ANT__NULL_P;

      }
      else
      {

/*   Path already gone. Do nothing. */

      }

   }
   else
   {

/*   There was an impossible path number in the message. */

      *status = ANT__IVDEINIT;

   }

}



void ant_send_netinit
(
struct a_rem_init_out rem_init_out,  /* the received message (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_SEND_NETINIT

*  Purpose:
*     Send a netinit message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a NETINIT, send it to the target machine and make the
*     necessary entries in the common blocks. In the event of failure,
*     return a DEINIT message to the local task.
*     Search the common blocks for an unused N_PATH entry.
*     Put partial information into it from the received message.
*     Send the init message across the network.
*     On error, return a DEINIT to the local task and annul the
*     new N_PATH entry.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-APR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Trap error on qiow
*     09-JUN-1988 (REVAD::BDK):
*        Ensure MACH_STATE is ok
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;     /* structure for sending to local task */
   int path;                    /* path number for new entry */
   int found;                   /* loop controller */
   int machnum;                 /* machine number */
   int nmach;                   /* machine number in network format */
   int ntype;                   /* message type in network format */
   int npath;                   /* path number in network format */

   if ( *status != SAI__OK ) return;


/*   Find a free N-PATH slot */

   found = 0;
   for ( path=0; path<MESSYS__MXPATH; path++ )
   {
      if ( n_paths[path].path_state == ANT__NULL_P )
      {
         found = 1;
         break;
      }
   }

   if ( found == 1 )
   {

/*   Search for the right machine */

      found = 0;
      for ( machnum=0; machnum<MESSYS__MXMACH; machnum++ )
      {
         if ( ( n_mach[machnum].mach_state == ANT__THIS_INIT ) ||
           ( n_mach[machnum].mach_state == ANT__OTHER_INIT ) )
         {
            if ( strcmp ( n_mach[machnum].machine_names,
              rem_init_out.remote_machine_name ) == 0 )
            {
               found = 1;
               break;
            }

         }

      }

      if ( found == 1 )
      {

/*   Make a partial entry in the tables */

         strcpy ( n_paths[path].local_taskname,
           rem_init_out.local_taskname );
         strcpy ( n_paths[path].remote_taskname,
           rem_init_out.remote_taskname );
         msp_mkcomq ( reply_q, &n_paths[path].local_task_q, status );
         n_paths[path].local_task_t_path_num =
           rem_init_out.local_task_t_path_num;
         n_paths[path].local_machine_num = machnum;
         n_paths[path].local_task_reminit_ack_q = reply_q;
         n_paths[path].path_state = ANT__THIS_P;

/*   Construct the INIT message and send it */

         ntype = htonl ( C_NET_INIT_OUT );
         memcpy ( netbuffer, &ntype, 4 );
         strcpy ( i_stask, rem_init_out.local_taskname );
         strcpy ( i_rtask, rem_init_out.remote_taskname );
         strcpy ( i_smach, this_machine );
         strcpy ( i_rmach, rem_init_out.remote_machine_name );
         nmach = htonl ( n_mach[machnum].remote_machine_num );
         memcpy ( i_rnum, &nmach, 4 );
         nmach = htonl ( machnum );
         memcpy ( i_smach, &nmach, 4 );
         npath = htonl ( path );
         memcpy ( i_spath, &npath, 4 );
         sock_write ( n_mach[machnum].local_channel,
           C_NET_MAXMSG_LEN, netbuffer, status );

         if ( *status != SAI__OK )
         {
            *status = SAI__OK;
            n_paths[path].local_taskname[0] = '\0';
            n_paths[path].remote_taskname[0] = '\0';
            n_paths[path].local_task_t_path_num = MESSYS__NULL_P;
            n_paths[path].local_machine_num = MESSYS__NULL_M;
            n_paths[path].path_state = ANT__NULL_P;
            found = 0;
         }

      }

   }

   if ( found == 0 )
   {

/*   Return a DEINIT to the local task */

      outbuf.mess_in_type = C_REM_DEINIT_IN;
      outbuf.u.rem_deinit_in.local_task_t_path_num =
        rem_init_out.local_task_t_path_num;
      msp_send_message ( (char *)&outbuf, C_REM_DEINIT_IN_LEN,
        reply_q, command_q, status );

   }

}



void ant_send_start_out
(
struct a_rem_gsoc_start_out rem_gsoc_start_out, /* the received message
                                                   (given) */
sendq_type reply_q,                 /* task reply queue (given) */
int *status                         /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_SEND_START_OUT

*  Purpose:
*     Send a net GSOC start message

*  Language:
*     Starlink C

*  Algorithm:
*     Given a REM_GSOC_START, record the new transaction and send it to
*     the target machine. In the event of failure, return a GSOC_END to
*     the local task.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-APR-1988 (REVAD::BDK):
*        Original
*     29-APR-1988 (REVAD::BDK):
*        Check qio status
*     29-APR-1988 (REVAD::BDK):
*        Set value of MACHNUM
*     30-MAY-1988 (REVAD::BDK):
*        Remove GSOC_ACK structures
*     09-JUN-1988 (REVAD::BDK):
*        Check PATH is in range
*     20-APR-1994 (REVAD::BDK):
*        Constant message size
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in outbuf;  /* structure for sending to local task */
   int path;                 /* path number for new entry */
   int found;                /* loop controller */
   int machnum;              /* machine number */
   int trans_num;            /* transaction number */
   int length;               /* length of message */
   int nflag;                /* message flag in network format */
   int npath;                /* path numner in network format */
   int ntrans;               /* transaction number in network format */
   int nlen;                 /* value length in network format */
   int ntype;                /* message type in network format */


   if ( *status != SAI__OK ) return;


/*   Make a partial entry in the tables */

   found = 0;
   path = rem_gsoc_start_out.local_nettask_n_path_num;

   if ( ( path >= 0 ) && ( path < MESSYS__MXPATH ) )
   {

      if ( n_paths[path].path_state == ANT__FULL_P )
      {

/*   Find a free transaction slot */

         for ( trans_num=0; trans_num<MESSYS__MXTRANS; trans_num++ )
         {
            if ( n_trans[trans_num].trans_state == ANT__NULL_T )
            {
               found = 1;
               break;
            }
         }

         if ( found == 1 )
         {
            n_trans[trans_num].trans_state = ANT__THIS_T;
            n_trans[trans_num].local_nettask_n_path_num = path;
            n_trans[trans_num].local_task_ack_q = reply_q;
            n_trans[trans_num].local_task_t_num =
              rem_gsoc_start_out.local_task_t_trans_num;

/*   Construct the message and send it to the other machine */

            ntype = htonl ( C_NET_GSOC_START_OUT );
            memcpy ( netbuffer, &ntype, 4 );
            ntrans = htonl ( trans_num );
            memcpy ( g_strans, &ntrans, 4 );
            npath = htonl ( n_paths[path].remote_nettask_n_path_num );
            memcpy ( g_rpath, &npath, 4 );
            nflag = htonl ( rem_gsoc_start_out.gsoc_flag );
            memcpy ( g_flag, &nflag, 4 );
            strcpy ( g_name, rem_gsoc_start_out.gsoc_name );
            ANT_CHECKLEN(rem_gsoc_start_out.gsoc_len);
            nlen = htonl ( rem_gsoc_start_out.gsoc_len );
            memcpy ( g_len, &nlen, 4 );
            memcpy ( g_value, rem_gsoc_start_out.gsoc_value,
              rem_gsoc_start_out.gsoc_len );

            machnum = n_paths[path].local_machine_num;

            sock_write ( n_mach[machnum].local_channel,
              C_NET_MAXMSG_LEN, netbuffer, status );
         }

      }

      if ( found == 0 )
      {

/*  Return a GSOC_END to the local task */

         outbuf.mess_in_type = C_REM_GSOC_END_IN;
         outbuf.u.rem_msg_in.local_task_t_trans_num =
           rem_gsoc_start_out.local_task_t_trans_num;
         outbuf.u.rem_msg_in.local_nettask_n_trans_num = MESSYS__NULL_T;
         outbuf.u.rem_msg_in.gsoc_flag = rem_gsoc_start_out.gsoc_flag;
         strcpy ( outbuf.u.rem_msg_in.gsoc_name,
           rem_gsoc_start_out.gsoc_name );
         ANT_CHECKLEN(rem_gsoc_start_out.gsoc_len);
         outbuf.u.rem_msg_in.gsoc_len = rem_gsoc_start_out.gsoc_len;
         outbuf.u.rem_msg_in.gsoc_status = ANT__NOPATH;
         memcpy ( outbuf.u.rem_msg_in.gsoc_value,
           rem_gsoc_start_out.gsoc_value,
           outbuf.u.rem_msg_in.gsoc_len );
         length = C_REM_MSG_IN_LEN - MSG_VAL_LEN +
           rem_gsoc_start_out.gsoc_len;

         msp_send_message ( (char *)&outbuf, length,
           reply_q, command_q, status );

      }

   }
   else
   {

/*   Invalid PATH number in message. */

      *status = ANT__IVSTART;

   }

}




void ant_serve
(
int *status          /* global status (given and returned) */
)

/*
*+
*  Name:
*     ANT_SERVE

*  Purpose:
*     Forward network messages

*  Language:
*     Starlink C

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     26-APR-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   receiveq_type queue_id;   /* queue number of received message */
   struct a_mess_out rxbuf;  /* received message */
   receiveq_type lqueues[2]; /* list of MSP queues */
   int actual_length;        /* length of received message (unused) */
   sendq_type reply_q;       /* MSP reply queue */
   int numq;                 /* number of queues */

   if ( *status != SAI__OK ) return;

/*  Wait for messages from the queues. */

   lqueues[0] = command_q;
   lqueues[1] = networkr_q;
   numq = 2;
/*   printf ( "comm_q = %d, net_q = %d\n", command_q, networkr_q); */

/*  Loop waiting for incoming MSP messages. */

   for ( ; ; )
   {

/*      printf("Waiting...\n"); */

      msp_receive_message ( lqueues, numq, 1, sizeof(rxbuf),
        (char *)&rxbuf, &actual_length, &queue_id, &reply_q, status );

/*	printf ( "serve: received %d, qid = %d\n", actual_length, queue_id);*/

      if ( *status != SAI__OK )
      {
         printf ( "adamnet: bad status on receive = %d\n", *status );
         printf ( "adamnet: queues were = %d, %d\n", lqueues[0], lqueues[1] );
         printf ( "adamnet: queues were = %d, %d\n", command_q, networkr_q );
      }

      if ( lqueues[0] !=0 )
      {
         printf ( "adamnet: queues bad after receive\n" );
      }
      if ( queue_id == networkr_q )
      {
         ant_network ( status );
      if ( lqueues[0] !=0 )
      {
         printf ( "adamnet: queues bad after ant_network\n" );
      }
      }
      else if ( queue_id == command_q )
      {
         ant_commq ( rxbuf, reply_q, status );
      if ( lqueues[0] !=0 )
      {
         printf ( "adamnet: queues bad after ant_commq\n" );
      }
      }

      if ( *status == ANT__NETSHUT )
      {
         printf ( "adamnet: NETSHUT requested\n" );
         break;
      }
      else if ( *status != SAI__OK )
      {
/*         *status = SAI__OK; */
         break;
      }

   }

}



static void ant_setsig
(
void
)

/*
*+
*  Name:
*     ANT_SETSIG

*  Purpose:
*     Set up signal handlers

*  Language:
*     Starlink C

*  Algorithm:
*     Called during process initialisation to alter the defaults for likely
*     Unix Signals (from the terminal, programming or system faults) to
*     cause the process to terminate via the exit() system service. This allows
*     any library "exit handlers" declared via atexit() to be called.
*     The routine ant_exhdlr is called by Unix when a signal occurs
*     Uses standard Unix C system service routines
*     Note: all signal handlers can be overridden by the user elsewhere in
*     the process and we should document any side effects of doing this.

*  Authors:
*     BKM: B.K. McIlwrath (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     02-DEC-1994 (BKM):
*       Original version.
*     14-JUL-1995 (BKM):
*       Totally revised logic (exit handler idea from RFWS)
*     12-Jan-1996 (BDK):
*       adapted from DTASK library for ANT
*     11-APR-1996: (AB, from BDK):
*       Modified SIGALRM handling, added SIGVTALRM and lots of others
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct sigaction ignact;
   struct sigaction oact;
   sigset_t curset;



   ignact.sa_handler = SIG_IGN;
   ignact.sa_flags = 0;
   sigemptyset ( &ignact.sa_mask );

/*   The defaults for three terminal related signals */

   sigaction ( SIGINT,  &ignact, NULL );
   sigaction ( SIGQUIT, &ignact, NULL );

/*   socket events from msp */

   sigaction ( SIGPIPE, &ignact, NULL );

/* Hangup and Default kill signals  - we use these to make an ADAM task exit
   quietly after running all exit handlers */

   sigaction ( SIGHUP, NULL, &oact );
   sigaddset ( &oact.sa_mask, SIGTERM );
   ant_retarget ( SIGHUP, oact );
   sigaction ( SIGTERM, NULL, &oact );
   sigaddset ( &oact.sa_mask, SIGHUP );
   ant_retarget ( SIGTERM, oact );

/* Make various other programming and system errors report and tidy up */

/* Signals common to most Unix variants - from Stevens page 266 */

   sigaction ( SIGABRT, NULL, &oact );
   ant_retarget ( SIGABRT, oact );
   sigaction ( SIGFPE, NULL, &oact );
   ant_retarget ( SIGFPE, oact );
   sigaction ( SIGILL, NULL, &oact );
   ant_retarget ( SIGILL, oact );
   sigaction ( SIGSEGV, NULL, &oact );
   ant_retarget ( SIGSEGV, oact );

/* Signals which may not be present on all variants of Unix */
#ifdef SIGALRM
/*   if ( oact.sa_handler != SIG_DFL ) ant_retarget ( SIGALRM, oact ); */
   sigaction ( SIGALRM, NULL, &oact );
   ant_retarget ( SIGALRM, oact );
#endif
#ifdef SIGBUS
   sigaction ( SIGBUS, NULL, &oact );
   ant_retarget ( SIGBUS, oact );
#endif
#ifdef SIGVTALRM
   sigaction ( SIGVTALRM, NULL, &oact );
   ant_retarget ( SIGVTALRM, oact );
#endif
#ifdef SIGEMT
   sigaction ( SIGEMT, NULL, &oact );
   ant_retarget ( SIGEMT, oact );
#endif
#ifdef SIGIOT
   sigaction ( SIGIOT, NULL, &oact );
   ant_retarget ( SIGIOT, oact );
#endif
#ifdef SIGSYS
   sigaction ( SIGSYS, NULL, &oact );
   ant_retarget ( SIGSYS, oact );
#endif
#ifdef SIGTRAP
   sigaction ( SIGTRAP, NULL, &oact );
   ant_retarget ( SIGTRAP, oact );
#endif
#ifdef SIGXCPU
   sigaction ( SIGXCPU, NULL, &oact );
   ant_retarget ( SIGXCPU, oact );
#endif
#ifdef SIGXFSZ
   sigaction ( SIGXFSZ, NULL, &oact );
   ant_retarget ( SIGXFSZ, oact );
#endif
#ifdef SIGURG
   sigaction ( SIGURG, NULL, &oact );
   ant_retarget ( SIGURG, oact );
#endif
#ifdef SIGTSTP
   sigaction ( SIGTSTP, NULL, &oact );
   ant_retarget ( SIGTSTP, oact );
#endif
#ifdef SIGCHLD
   sigaction ( SIGCHLD, NULL, &oact );
   ant_retarget ( SIGCHLD, oact );
#endif
#ifdef SIGTTIN
   sigaction ( SIGTTIN, NULL, &oact );
   ant_retarget ( SIGTTIN, oact );
#endif
#ifdef SIGTTOU
   sigaction ( SIGTTOU, NULL, &oact );
   ant_retarget ( SIGTTOU, oact );
#endif
#ifdef SIGPROF
   sigaction ( SIGPROF, NULL, &oact );
   ant_retarget ( SIGPROF, oact );
#endif
#ifdef SIGWINCH
   sigaction ( SIGWINCH, NULL, &oact );
   ant_retarget ( SIGWINCH, oact );
#endif
#ifdef SIGLOST
   sigaction ( SIGLOST, NULL, &oact );
   ant_retarget ( SIGLOST, oact );
#endif
#ifdef SIGUSR1
   sigaction ( SIGUSR1, NULL, &oact );
   ant_retarget ( SIGUSR1, oact );
#endif
#ifdef SIGUSR2
   sigaction ( SIGUSR2, NULL, &oact );
   ant_retarget ( SIGUSR2, oact );
#endif

}



void ant_exhdlr
(
int isig,
siginfo_t *info,
void *dummy
)
{
   int i;
   int pid;
   int child_status;
   struct siglist *entry;
   struct siglist *cur_entry = NULL;
   sigset_t set;
   sigset_t curset;

/* Find the RTL handler for the current signal */

/*   printf ("In exhdlr\n"); */
   for ( entry=stack_top; entry != NULL; entry=entry->next )
   {
      if ( entry->signo == isig )
      {
         cur_entry = entry;
         break;
      }
   }

/* Reset all signal handlers to the system default */

   for ( i=0; i<sizeof(int)*8; i++ )
   {
      if ( ( sigisset >> i ) & 1 )
      {
         signal ( i, SIG_DFL );
      }
   }

/* SIGHUP or SIGTERM are the normal way of quietly exiting an ADAM program */

   if ( ( isig == SIGHUP ) || ( isig == SIGTERM ) )
   {
/*      printf("HUP/TERM exit\n"); */
      exit(0);
   }
   else
   {
/*   We fork a duplicate process - the parent calls the RTL handler for the
     current signal while the child process merely exits. This allows exit
     handlers to be obeyed for abormal termination. */
    printf("Forking\n");
      if ( ( pid = fork() ) < 0 )
      {
         perror ( "ant_exhdlr - fork error" );
      }
      else if ( pid == 0 )
      {

/* Child */

         exit(isig);
      }
      else
      {

/* Parent */

         if ( waitpid ( pid, &child_status, 0 ) < 0 )
         {
            perror("ant_exhdlr - waitpid error");
         }
         if ( cur_entry != NULL )
         {
             cur_entry->act.sa_sigaction(isig, info, dummy);

/*   We do NOT expect the RTL handler for fatal signals to return. Just in case
     it does we exit as fast as possible! */
/*     printf("Going quickly...\n"); */
            _exit(isig);
         }
         else
         {

/* No RTL handler - we just resignal and exit */
/*     printf("Resignal, then go\n"); */
            kill ( getpid(), isig );
         }
      }
   }
}




void ant_sighdlr
(
int astparam,              /* the signal parameter (given) */
siginfo_t *infop,          /* info pointer (given) */
void *ucp                  /* context pointer (given) */
)

/*
*+
*  Name:
*     ANT_EXHDLR

*  Purpose:
*     ADAM task signal handler for all process signals

*  Language:
*     Starlink C

*  Algorithm:
*     This routine is declared as the signal handler for SIGIO events.
*     All created network sockets are set to deliver signals whenever
*     they move from the state of having no input data.
*     Write an MSP message to the NETWORKS_Q so that the main-line code
*     is aware of the presence of socket action.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     23-MAR-1988 (REVAD::BDK):
*        Original
*     20-MAY-1988 (REVAD::BDK):
*        Signal bad status from READMBX
*     18-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     15-APR-1996 (BDK):
*        Change to using sigaction() in main-line
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
/*
*+
*  Name:
*     ANT_SIGHDLR

*  Purpose:
*     Signal handler network messages

*  Language:
*     Starlink C

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int status;                /* status */
   char tempmsg[4];           /* dummy message */

/*   Send a message - its content is unimportant. The send will fail if
     it would block - this is ok, it means there are messages already
     waiting to be read by the main-line code. If this routine was
     allowed to block it would deadlock this process. */

   status = SAI__OK;
/*   printf ("sighdlr entered\n"); */
   msp_send_message ( tempmsg, 4, networks_q, networkr_q, &status );
/*   printf ("message sent\n"); */

}




void ant_verify
(
int channel,         /* i/o channel for communications, bound to a socket
                       (given) */
char *machine_name,  /* name of other machine (returned) */
int *status          /* global status (give and returned) */
)

/*
*+
*  Name:
*     ANT_VERIFY

*  Purpose:
*     Verify an incoming connection

*  Language:
*     Starlink C

*  Algorithm:
*     Inquire the details of the peer machine

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     15-APR-1994 (REVAD::BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct sockaddr_in peer;     /* structure for peer details */
   struct hostent peerent;      /* structure for peer entry */
   int addrlen;                 /* length of peer structure */


   if ( *status != SAI__OK ) return;

   addrlen = sizeof ( peer );
   sock_gpn ( channel, &peer, status );
   if ( *status == SAI__OK )
   {
      peer.sin_family = AF_INET;
      sock_ghba ( peer, &peerent, status );
      if ( *status == SAI__OK )
      {
         strcpy ( machine_name, peerent.h_name );
      }
      else
      {
         perror ( "ant_verify: sock_gbha failed" );
         machine_name[0] = '\0';
      }

   }
   else
   {
      perror ( "sock_gpn failed" );
      machine_name[0] = '\0';
   }

}
