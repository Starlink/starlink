/*
*+
*  Name:
*     AMS

*  Purpose:
*     Adam Message System

*  Language:
*     Starlink C

*  Copyright:
*     Copyright (C) 1992-1994 Science & Engineering Research Council.
*     Copyright (C) 1996, 1999, 2004 Central Laboratory of the Research
*     Councils. Copyright (C) 2005 Particle Physics & Astronomy
*     Research Council. All Rights Reserved.

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
*     IRJ: Ian Jenkins (RAL)
*     BDK: Dennis Kelly (ROE)
*     BKM: Brian McIlwrath (STARLINK)
*     AJC: Alan Chipperfield (STARLINK)
*     DLT: David Terrett (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     16-JUN-1992 (IRJ/SKR):
*        Tidied
*     16-JUN-1993 (BKM):
*        Tidied and reformatted
*     01-SEP-1993 (BKM):
*        Further reformatted and ANSI-C only
*     17-MAR-1994 (BDK):
*        Changed to ams
*     07-JUL-1994 (AJC):
*        Rename Some Include Files:-
*          adamdefns.h -> adam_defns.h
*          mesdefns.h -> messys_len.h
*     16-AUG-1994 (BKM):
*        Bring up to date with BDK version (and comments)
*     15-NOV-1994 (AJC):
*        Dimension transfree MESSYS__MXTRANS not MXPATH
*     08-FEB-1996 (AJC):
*        Remove diagnostic printfs in ams_raccept and ams_receive
*     14-SEP-1999 (DLT):
*        Enable initialization without exit handler
*     03-SEP-2004 (TIMJ):
*        Fix compiler warnings due to lack of include files
*     29-DEC-2005 (TIMJ):
*        Tidy up config ATEXIT logic
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if HAVE_ATEXIT
# define USE_ATEXIT 1
#elif HAVE_ON_EXIT
# define USE_ON_EXIT 1
#else
  Unable to find an exit handler
#endif

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <signal.h>
#include <errno.h>
#include "sae_par.h"
#include "adam_defns.h"             /* adam constants */
#include "dtask_err.h"              /* dtask codes */

#include "messys_err.h"
#include "messys_par.h"
#include "messys_len.h"

#include "msp_par.h"               /* low-level message system */
#include "msp.h"

#include "atimer_par.h"            /* timer facility */
#include "atimer.h"

#include "ams_sys.h"
#include "ams_mac.h"
#include "ams_struc.h"
#include "ams.h"
#include "ams_static.h"

/*   Local structure definitions */

struct a_t_path {                          /* holds details of a path */
      char other_taskname[MESSYS__TNAME];  /* taskname */
      int other_pathnum;                   /* path index in other task */
      sendq_type other_com_q;              /* command queue for other task */
      int path_state;                      /* flag for partially-inserted path */
      int machine_num;                     /* index to CPU running this task */
      };

struct a_t_trans {                   /* holds details of a transaction */
      int t_path_num;                /* index to T_PATHS structure */
      receiveq_type this_task_ack_q; /* acknowledgement queue */
      sendq_type other_task_ack_q;   /* acknowledgement queue */
      int other_transnum;            /* transaction index in other task */
      };

/*   File global variables */

typedef enum { false, true } BOOL;       /* FORTRAN BOOLEAN type */

static char taskname[MESSYS__TNAME];  /* this task's name */

/*   "receive" end of the local queues */

static receiveq_type command_q;       /* this task's command queue */
static receiveq_type extint_q;        /* this task's extint queue */
static receiveq_type astint_q;        /* this task's astint queue */
static receiveq_type resched_q;       /* this task's resched queue */
static receiveq_type timeout_q;       /* this task's timeout queue */
static receiveq_type kick_q;          /* this task's kick queue */

/*   "send" end of the local queues */

static sendq_type sigresch_q;
static sendq_type sigast_q;
static sendq_type sigext_q;
static sendq_type sigtimeout_q;
static sendq_type sigkick_q;

/*   The currently active paths are held in an array of A_T_PATH
     structures, indexed by THIS_TASK_T_PATH_NUM */

static struct a_t_path t_paths[MESSYS__MXPATH];

/*   The currently active transactions are held in an array of A_T_TRANS
     structures, indexed by T_TRANS_NUM  */

static struct a_t_trans t_trans[MESSYS__MXTRANS];

/*   Arrays of flags indicate which path and transaction numbers are
     unused. */

static BOOL pathfree[MESSYS__MXPATH];
            /* reflects corresponding element in t_paths[] */
static BOOL transfree[MESSYS__MXTRANS];
            /* relects corresponding element in  t_trans[] */

/*   The names of machines which are being accessed are held in an array
     indexed by T_PATHS(N).MACHINE_NUM. */

static char machine_names[MESSYS__MXMACH][MESSYS__MNAME];

/*   The command queues for the ADAMNET processes are obtained when the first
     network call is requested to the corresponding network. */

static char messys_netsep[MESSYS__MAXNET][MESSYS__SEPLEN];
                        /* network separators */
static sendq_type messys_netqueue[MESSYS__MAXNET];
                        /* command queues for network processes */
static char messys_netname[MESSYS__MAXNET][MESSYS__TNAME];
                        /* names of network processes */

static char MSG_EMPTYNAME[MSG_NAME_LEN] = "";
static char MSG_EMPTYVAL[MSG_VAL_LEN] = "";





static void ams_accept
(
const struct a_loc_init_in * loc_init_in,  /* the message requesting
                                              initialisation (given) */
sendq_type ackq,                     /* queue for returning
                                        acknowledgement (given) */
int *status                          /* global status (given and
                                        returned) */
)

/*
*+
*  Name:
*     AMS_ACCEPT

*  Purpose:
*     Accept a request to open a path

*  Language:
*     Starlink C

*  Algorithm:
*     An "init" message has been received from another task requesting a
*     connecting path to be set up. Allocate a data structure to the path
*     and return an acceptance message.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     27-JUN-1994 (BDK):
*        Use msp_mkcomq to get command queue of other task
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int j;                  /* path number in this task */
   int istat;              /* local status */
   int otherpathno;        /* path number in other task */


   if ( *status != SAI__OK ) return;

   otherpathno = loc_init_in->other_task_t_path_num;
   ams_getfreepath ( &j, status );

   if ( *status != SAI__OK )
   {
      istat = SAI__OK;
      ams_senddeinit ( 1, MESSYS__NULL_P, ackq,
        otherpathno, &istat );
   }
   else
   {
      strcpy ( t_paths[j].other_taskname,
        loc_init_in->other_taskname );
      msp_mkcomq ( ackq, &t_paths[j].other_com_q, status );
      t_paths[j].other_pathnum = otherpathno;
      t_paths[j].path_state = MESSYS__FULL_P;
      t_paths[j].machine_num = MESSYS__NULL_M;
      ams_sendinitack ( 1, ackq, otherpathno, j, status );
   }
   *status = SAI__OK;
}


static void ams_addrest
(
const struct a_loc_ack_in * loc_ack_in,  /* received ack_in structure (given) */
int path,                          /* path of init transaction (given) */
int messid,                        /* messid of init transaction (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *status                        /* global status (given and returned) */
)


/*
*+
*  Name:
*     AMS_ADDREST

*  Language:
*     Starlink C

*  Algorithm:
*     This task has sent an "init" message to another task requesting a
*     connecting path to be set up. A positive reply has been received.
*     Check the reply is valid and complete the data structure for the
*     path.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     17-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int otherpathno;

   if ( *status != SAI__OK ) return;

   otherpathno = loc_ack_in->other_task_t_path_num;
   if ( ( path == loc_ack_in->this_task_t_path_num ) &&
     ( t_paths[path].path_state == MESSYS__PART_P ) )
   {
      t_paths[path].other_pathnum = otherpathno;
      t_paths[path].path_state = MESSYS__FULL_P;
      ams_freetrans ( messid, status );
   }
   else
   {
      ams_senddeinit ( 1, MESSYS__NULL_P, reply_q, otherpathno, status );
      *status = MESSYS__IVACKINIT;
   }
}


static void ams_analysename
(
const char *name, /* full taskname (given) */
int *netind,      /* index to network type (returned) */
char *task,       /* task name (returned) */
char *mach,       /* machine name (returned) */
int *remote,      /* flag for whether task is remote (returned) */
int *status       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_ANALYSENAME

*  Language:
*     Starlink C

*  Algorithm:
*     Given, by the application code a taskname 'name' of the form
*     xxxxxbbyyyyyyyy or yyyyyyyy where yyyyy is the actual taskname,
*     xxxxxx is a remote host's name and bb is the separator for that
*     host. This function given the form yyyyyy (it detects this form by
*     the absence of a known separator bb (all known host separators are
*     held in messys_netsep[0..MESSYS__MAXNET-1])) sets task to a
*     trailing space trimmed yyyyyy, machine to "", remote to 0 and
*     netind to MESSSY_MAXNET. If given the other form xxxxxbbyyyyyy
*     (detected by bb existing in messys_netsep[], the function sets
*     'task' to the trailing space trimmed version of yyyyyy, 'mach' to
*     xxxxxbb, 'remote' to 1 and 'netind' to the index position in
*     messys_netsep[] of bb. If the taskname is longer than MESSYS__TNAME
*     or the machine name given is longer than MESSYS_MNAME, this
*     function returns MESSYS__TOOLONG in *status, otherwise returns
*     SAI__OK.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   char *machend;         /* pointer to end of machine name */
   char *taskstart;       /* pointer to start of task name */
   int nlen;              /* length of task name */


   if ( *status != SAI__OK ) return;

   *netind = 0;
   mach[0] = '\0';
   *remote = 0;

   while ( *netind < MESSYS__MAXNET )
   {
      if ( (machend = strstr(name, messys_netsep[*netind]) ) == CHARNIL )
      {
          (*netind)++;
      }
      else
      {
         taskstart = machend + strlen(messys_netsep[*netind]);
         if ( (taskstart - name) >= MESSYS__MNAME )
         {
            *status = MESSYS__TOOLONG;
         }
         else if ( (taskstart - name) == 0 )
         {
            *status = MESSYS__TOOLONG;
         }
         else
         {

/* copy the task name after the network separator into task */

            nlen = strlen ( taskstart );
            if ( ( nlen < MESSYS__TNAME ) && ( nlen > 0 ) )
            {
               strcpy ( task, taskstart );
            }
            else
            {
               *status = MESSYS__TOOLONG;
            }

            if ( *status == SAI__OK)
            {

/* copy the machine name plus the separator into mach and null terminate
   it */
               strncpy ( mach, name, (taskstart - name) );

               mach[(taskstart - name)] = '\0';
               *remote = 1;
            }
         }
         break;
      }
   }
   if ( ( *remote == 0 ) && ( *status == SAI__OK ) )
   {
      nlen = strlen ( name );
      if ( ( nlen > 0 ) && ( nlen < MESSYS__TNAME ) )
      {
         strcpy ( task, name );
      }
      else
      {
         *status = MESSYS__TOOLONG;
      }
   }
}


void ams_astint
(
int *status         /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_ASTINT

*  Purpose:
*     Send an ASTINT message from a signal handler

*  Language:
*     Starlink C

*  Algorithm:
*     Send obey message to astint_q as ast interrupt has occurred.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   ams_sendobey ( sigast_q, MSG_EMPTYNAME, 1, MSG_EMPTYVAL, status );
}


void ams_astmsg
(
const char *name, /* name of the action to be rescheduled (given) */
int length,       /* number of significant bytes in value (given) */
const char *value,/* message to be passed to main-line code (given) */
int *status       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_ASTMSG

*  Purpose:
*     Send an ASTMSG from a signal handler

*  Language:
*     Starlink C

*  Algorithm:
*     Send a soft ast interrupt message 'value' qualified by 'name' to
*     astint_q.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   ams_sendobey ( sigast_q, name, length, value, status );
}


static void ams_call_out
(
const char *machname,/* name of remote machine (given) */
int netind,        /* index to network parameters (given) */
int *machnum,      /* number allocated to machine in MESSYS common blocks
                      (returned) */
int *status        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_CALL_OUT

*  Language:
*     Starlink C

*  Algorithm:
*     Return a machine number corresponding the node name embedded in the
*     given machine name which has been extracted from a given taskname.
*
*     This may involve initiating a network connection with a remote
*     machine

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     17-JUN-1992 (IRJ,SKR):
*        Tidied
*     add extint_q to list waited on (BDK)
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   struct a_mess_out mess_out;   /* message sent */
   struct a_mess_in mess_in;     /* message received */

   int actual_length;            /* length of received message */
   int queue_id;                 /* received queue (unused) */
   receiveq_type accept_queue;   /* accept queue */
   sendq_type reply_queue;       /* reply queue */
   int istat;                    /* local status */
   int queues[3];                /* queues for replies */
   BOOL timer_set;               /* timer flag */
   int added;                    /* flag for whether machname is new */

   if ( *status != SAI__OK ) return;

   memset( &mess_out, 0, sizeof(mess_out));
   timer_set = false;
   ams_getmachnum ( machname, machnum, &added, status );
   if ( *status != SAI__OK )
   {
      return;
   }

   if (added)
   {

/*   If we had to add the machine name we must establish communication */

      msp_get_task_queue ( messys_netname[netind],
        &messys_netqueue[netind], status );

      if ( *status != SAI__OK )
      {
         *status = MESSYS__NONET;
         machine_names[*machnum][0] = '\0';
         *machnum = MESSYS__NULL_M;
      }
      else
      {
         memset( &mess_out, 0, sizeof(mess_out));
         mess_out.mess_out_type = C_REM_CALL_OUT;
         strcpy ( mess_out.u.rem_call_out.remote_machine_name,
           machname );
         msp_create_receiveq ( &accept_queue, status );
         if ( *status == SAI__OK )
         {
            msp_send_message ( (char *) &mess_out, C_REM_CALL_OUT_LEN,
              messys_netqueue[netind], accept_queue, status );
            ams_settimeout ( MESSYS__INIT_WAIT_TIME, status );
            if (*status == SAI__OK)
            {
               timer_set = true;
            }
            queues[0] = accept_queue;
            queues[1] = timeout_q;
            queues[2] = extint_q;
            msp_receive_message ( queues, 3, true, C_REM_ACCEPT_IN_LEN,
              (char *) &mess_in, &actual_length, &queue_id, &reply_queue,
              status );
            if ( *status == SAI__OK )
            {
               if ( queue_id == timeout_q )
               {
                  timer_set = false;
                  *status = MESSYS__NETTIME;
               }
               else if ( mess_in.mess_in_type == C_REM_ACCEPT_IN )
               {
                  if ( mess_in.u.rem_accept_in.accept_status != SAI__OK )
                  {
                     machine_names[*machnum][0] = '\0';
                     *machnum = MESSYS__NULL_M;
                     *status = mess_in.u.rem_accept_in.accept_status;
                  }
               }
               else
               {
                  machine_names[*machnum][0] = '\0';
                  *machnum = MESSYS__NULL_M;
                  *status = MESSYS__IVNETCALL;
               }
            }
            istat = SAI__OK;
            msp_delete_queue ( accept_queue, &istat );
         }
         else
         {
            machine_names[*machnum][0] = '\0';
            *machnum = MESSYS__NULL_M;
         }
      }
      if (timer_set)
      {
         istat = SAI__OK;
         atimer_cantim ( MESSYS__TIMEOUTID, &istat );
      }
   }
}


static void ams_endtrans
(
const struct a_loc_msg_in * loc_msg_in,  /* the end-transaction message in
                                            internal format (given) */
int *path,                         /* the communications path to the
                                      other task (returned) */
int *messid,                       /* the transaction end index for this
                                      transaction (returned) */
int message_name_s,                /* space for name (given) */
int message_value_s,               /* space for value (given) */
int *message_status,               /* message status (returned) */
int *message_context,              /* message context (returned) */
char *message_name,                /* message name (returned) */
int *message_length,               /* length of value (returned) */
char *message_value,               /* message value (returned) */
int *status                        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_ENDTRANS

*  Language:
*     Starlink C

*  Algorithm:
*     The system has received a local gsoc_end message, the last message
*     in the transaction 'loc_msg_in->this_task_t_trans_num' (set into
*     *messid) on path t_trans[*messid].t_path_num (set into *path.).
*     Using ams_unpacklocmsg() this routine returns the contents of the
*     last message to the caller and uses ams_freetrans() to close and
*     free the relevent transaction.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
    if ( *status != SAI__OK ) return;

   *messid = loc_msg_in->this_task_t_trans_num;
   AMS_checktrans ( *messid, status );
   if ( *status == SAI__OK )
   {
      *path = t_trans[*messid].t_path_num;
      ams_unpacklocmsg ( loc_msg_in, message_name_s, message_value_s,
        message_status, message_context, message_name,
        message_length, message_value, status );
      ams_freetrans ( *messid, status );
   }
}



#if USE_ON_EXIT

/*
*+
*  Name:
*     AMS_EXIT1

*  Purpose:
*     SunOS on_exit handler

*  Language:
*     Starlink C

*  Algorithm:
*     Send a DE_INIT message to each open path.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     01-JUN-1994 (BDK):
*        Fix arguments to sendgsocend
*     01-DEC-1994: Don't send DE_INIT on each transaction - just on each open
*                 path (BKM)
*     05-DEC-1994: Make routine global BUT should NOT be called directly except
*                 by error handling code!
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

static void ams_exit1
(
 int iarg,		/* exit() value  - not used */
 void * arg,            /* Argument to on_exit - not used */
)
{
    ams_exit();
}
#endif

void ams_exit
(
void
)

/*
*+
*  Name:
*     AMS_EXIT

*  Purpose:
*     Ams exit handler

*  Language:
*     Starlink C

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int istat;
   int j;

/* Close path connections */
   for (j = 0; j < MESSYS__MXPATH; j++)
   {
      if ( pathfree[j] == false )
      {
         istat = SAI__OK;
         ams_senddeinit ( (t_paths[j].machine_num == MESSYS__NULL_M),
           j, t_paths[j].other_com_q, t_paths[j].other_pathnum, &istat);
      }
   }
   msp_exit();
}



void ams_extint
(
int *status         /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_EXTINT

*  Purpose:
*     Send an EXTINT message from a signal handler

*  Language:
*     Starlink C

*  Algorithm:
*     Send obey message to extint_q as ext interrupt has occured.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   ams_sendobey ( sigext_q, MSG_EMPTYNAME, 1, MSG_EMPTYVAL, status );
}



static void ams_freepath
(
int j              /* path number (given) */
)

/*
*+
*  Name:
*     AMS_FREEPATH

*  Language:
*     Starlink C

*  Algorithm:
*     Free the path whose index into pathfree[] and t_paths[] is j.
*     Free all transactions associated with the path, then close msp
*     communications on this path, then null the path entry.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   int jj;               /* loop counter */
   int istat;            /* local status */


   if ( ( j >= 0 ) && ( j < MESSYS__MXPATH ) )
   {
      for ( jj=0; jj<MESSYS__MXTRANS; jj++ )
      {
         if ( ( transfree[jj] == false ) &&
           ( t_trans[jj].t_path_num == j ) )
         {
            istat = SAI__OK;
            ams_freetrans ( jj, &istat );
         }
      }

      if ( t_paths[j].machine_num == MESSYS__NULL_M )
      {

/*   task on this machine - can close the msp communications completely */

         istat = SAI__OK;
         msp_close_task_queue ( t_paths[j].other_com_q, &istat );
      }
      pathfree[j] = true;
      (t_paths[j].other_taskname)[0] = '\0';
      t_paths[j].other_pathnum = MESSYS__NULL_P;
      t_paths[j].path_state = MESSYS__NULL_P;
      t_paths[j].other_com_q = MSP__NULL_SENDQ;
      t_paths[j].machine_num = MESSYS__NULL_M;
    }
}


static void ams_freetrans
(
int messid,       /* identifier of transaction to be freed (given) */
int *status       /* global status (given and received) */
)

/*
*+
*  Name:
*     AMS_FREETRANS

*  Language:
*     Starlink C

*  Algorithm:
*     This frees an active transaction entry in t_trans[]. The first
*     thing it does, subject to the transaction index being legal (ie
*     lies between 0 and MESSYS__MXTRANS-1 (inclusive), is to check to
*     see if the transaction had an acknowledge queue and if so delete it
*     using msp_delete_queue(). It then clears the transaction structure
*     using the appropriate NULLs (see messys_par.h) and then sets
*     transfree[] to true.
*
*     A transaction entry records the path associated with the
*     transaction (t_path_num, an index into t_paths[] that records other
*     information about that path), this task's acknowledge queue for
*     this transaction (this_task_ack_q), the others task's acknowledge
*     queue for this transaction (other_task_ack_q) and the other task's
*     transaction index into its own t_trans[] (other_transnum).
*     transfree[j] records the active use of a transaction entry in
*     t_trans[j] when set false otherwise indicates that the
*     corresponding t_trans[] entry is unused and free for (re)use.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   if ( *status != SAI__OK ) return;

   if ( messid >= 0 && messid < MESSYS__MXTRANS )
   {
      if ( t_trans[messid].this_task_ack_q != MSP__NULL_RECEIVEQ )
      {
         msp_delete_queue ( t_trans[messid].this_task_ack_q, status );
      }
      t_trans[messid].t_path_num = MESSYS__NULL_P;
      t_trans[messid].other_task_ack_q = MSP__NULL_SENDQ;
      t_trans[messid].this_task_ack_q = MSP__NULL_RECEIVEQ;
      t_trans[messid].other_transnum = MESSYS__NULL_T;
      transfree[messid] = true;
    }
}


static void ams_getfreepath
(
int *path,         /* path number (returned) */
int *status        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_GETFREEPATH

*  Language:
*     Starlink C

*  Algorithm:
*     Searches pathfree[] for a free path, setting *path to index and
*     pathfree[*path] to false if one found or *status to
*     MESSYS_COMFULL.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     21-JUN-1994: don't use path number zero - there is software outside
*                 the message system which assumes that it can check for
*                 a valid path number on the basis that it has to be
*                 greater than zero (BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   int wpath;

   if ( *status != SAI__OK ) return;


   wpath = 1;
   while ( ( wpath < MESSYS__MXPATH ) && ( pathfree[wpath] == false ) )
   {
      ++wpath;
   }
   if ( wpath == MESSYS__MXPATH )
   {
      *status = MESSYS__COMFULL;
   }
   else
   {
      *path = wpath;
      pathfree[wpath] = false;
   }
}


static void ams_getfreetrans
(
int getq,          /* flag for whether reply queue required (given) */
int path,          /* associated path (given) */
sendq_type otaq,   /* other task's reply queue (given) */
int ottn,          /* other task;s transaction number (given) */
int *messid,       /* identifier for the transaction (returned) */
int *status        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_GETFREETRANS

*  Language:
*     Starlink C

*  Algorithm:
*     Searches transfree[] for a free transaction slot and if it finds
*     one sets
*
*       *messid to its index,
*       *status to SAI__OK,
*       transfree[index] to false,
*       t_trans[index].t_path_num to path,
*       t_trans[index].this_task_ack_q to a new queue  (if getq == 1) or
*                                   NULL_Q (if getq == 0)
*       t_trans[index].other_task_ack_q to otaq
*       t_trans[index].other_transnum to ottn
*
*     otherwise sets
*
*      *messid to MESSYS__NULL_T and
*      *status to MESSYS_COMFULL.
*
*     Creation of the reply queue is made optional as an optimisation for
*     the cases where no reply queue is going to be involved.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int wm;                      /* transaction number */
   receiveq_type ttaq;          /* reply queue */

   if ( *status != SAI__OK ) return;

   *messid = MESSYS__NULL_T;
   wm = 0;
   while ( ( wm < MESSYS__MXTRANS ) && ( transfree[wm] == false ) )
   {
      wm++;
   }
   if ( wm == MESSYS__MXTRANS )
   {
      *status = MESSYS__COMFULL;
      return;
   }

   *messid = wm;
   if ( getq )
   {
      msp_create_receiveq ( &ttaq, status );
   }
   else
   {
      ttaq = MSP__NULL_RECEIVEQ;
   }
   if ( *status == SAI__OK)
   {
      transfree[wm] = false;
      t_trans[wm].t_path_num = path;
      t_trans[wm].this_task_ack_q = ttaq;
      t_trans[wm].other_task_ack_q = otaq;
      t_trans[wm].other_transnum = ottn;
   }
}


static void ams_getmachnum
(
const char *machinename,      /* name of machine (given) */
int *machinenumber,     /* index to machine (returned) */
int *added,             /* flag for if new entry added (returned) */
int *status             /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_GETMACHNUM

*  Purpose:
*     Return the index to the named machine

*  Language:
*     Starlink C

*  Algorithm:
*     The system maintains an array of known remote machines that this
*     process is in communication with.  When this task makes a path
*     connection with a remote task (named, say, xxxxx::yyyyyy) then the
*     "xxxx::" part is stored in a free entry in machine_names[] and the
*     t_paths[].machine_num part of the relevant t_path[]'s entry records
*     the index into machine_names[]
*
*     This function tries to find an existing entry in machine_names[]
*     for the machine 'machinename' but failing this tries to create a
*     new entry.  If either of these succeed it sets *machinenumber to
*     the relevant machine_names[] index, otherwise it returns a bad status.
*     If it does add a new entry then *added is set to 1 otherwise added
*     will be 0 on return

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int machnum;                 /* machine number */

   if ( *status != SAI__OK ) return;

   *added = 0;
   *machinenumber = MESSYS__NULL_M;
   machnum = -1;

   while ( ( *machinenumber == MESSYS__NULL_M ) &&
     ( (machnum += 1) < MESSYS__MXMACH ) )
   {
      if ( strcmp(machine_names[machnum], machinename) == 0 )
      {
         *machinenumber = machnum;
      }
   }
   if ( *machinenumber == MESSYS__NULL_M )
   {
      machnum = -1;
      while ( ( *machinenumber == MESSYS__NULL_M ) &&
        ( (machnum += 1) < MESSYS__MXMACH) )
      {
         if ( machine_names[machnum][0] == '\0' )
         {
            strcpy ( machine_names[machnum], machinename );
            *added = 1;
            *machinenumber = machnum;
         }
      }
   }
   if ( *machinenumber == MESSYS__NULL_M )
   {
      *status = MESSYS__TOOMACH;
   }
}


void ams_getreply
(
int timeout,              /* timeout time in milliseconds (given) */
int path,                 /* pointer to the path (given) */
int messid,               /* message number of incoming message (given) */
int message_name_s,       /* space for name (given) */
int message_value_s,      /* space for value (given) */
int *message_status,      /* message status (returned) */
int *message_context,     /* message context (returned) */
char *message_name,       /* message name (returned) */
int *message_length,      /* length of value (returned) */
char *message_value,      /* message value (returned) */
int *status               /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_GETREPLY

*  Purpose:
*     Receive a message on a specified path, messid

*  Language:
*     Starlink C

*  Algorithm:
*     The application has sent a message on path 'path' as part of
*     transaction 'messid' and wishes to obtain the reply. The function
*     first checks the transaction is legally identified and that there
*     exists an acknowledge queue for that transaction (using
*     AMS_checktransactive()) It then, if 'timeout' is not
*     MESSYS_INFINITE, sets the timer clock going so that we get a
*     timeout if there is no response within timeout milliseconds. It
*     then, using msp_receive_message() determines input on either the
*     external interrupt queue, the transaction acknowlege queue or the
*     timeout queue.
*
*     If the message received is of type C_LOC_MSG_IN, the origin is
*     determined as either an external interrupt, a timeout interrupt OR a
*     normal message setting *status to MESSYS_EXTINT, MESSYS_TIMEOUT or
*     SAI__OK appropriately. messys_translate() is used to unpack the
*     message.
*
*     If the message is                                      use
*
*      message from remote source    (C_REM_MSG_IN)        messys_rtranslate()
*      end transaction message       (C_LOC_GSOC_END_IN)   messys_endtrans()
*      remote end transaction        (C_REM_GSOC_END_IN)   messys_rendtrans()
*      local deinitialise path       (C_LOC_DEINIT_IN)     messys_remove()
*      remote deinitialise path      (C_REM_DEINIT_IN)     messys_remove()
*      local initialise acknowldge   (C_LOC_ACK_IN)        messys_addrest()
*      remote initialise acknowledge (C_REM_ACK_IN)        messys_raddrest()
*      other                         we set *status to MESSYS_MSGFUNC
*
*     If the timer is still running, then we use messys_cantim() to turn
*     it off.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     16-JUN-1992 (IRJ,SKR):
*        Tidied
*     17-MAR-1994 (BDK):
*        Use the reply_q returned from msp
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


{
   struct a_mess_in mess_in;            /* message structure in internal
                                           format */
   BOOL timer_set;                      /* flag for timer set */
   receiveq_type queues[AMS__MXQUEUE];  /* list of active queues */
   int actual_length;                   /* length of message received */
   receiveq_type queue_id;              /* queue where message received */
   sendq_type reply_q;                  /* other task's reply q */
   int istat;                           /* internal status */
   int locpath;                         /* local copy of path */
   int locmessid;                       /* local copy of messid */

   if ( *status != SAI__OK ) return;

   locpath = path;
   locmessid = messid;
   timer_set = false;
   AMS_checktransactive(locmessid, status);
   if ( *status != SAI__OK ) return;

   if ( timeout != MESSYS__INFINITE )
   {
      ams_settimeout ( timeout, status );
      if ( *status == SAI__OK )
      {
          timer_set = true;
      }
   }
   queues[0] = extint_q;
   queues[1] = t_trans[locmessid].this_task_ack_q;
   queues[2] = timeout_q;
   msp_receive_message ( queues, 3, true, C_MAXMSG_LEN,
     (char *) &mess_in, &actual_length, &queue_id, &reply_q, status);

   if ( *status == SAI__OK )
   {
      switch ( mess_in.mess_in_type )
      {
         case C_LOC_MSG_IN:
            ams_translate ( &(mess_in.u.loc_msg_in), reply_q, &locpath,
              &locmessid, message_name_s, message_value_s,
              message_status, message_context, message_name,
              message_length, message_value, status );
            if (queue_id == extint_q)
            {
               *status = MESSYS__EXTINT;
            }
            else if (queue_id == timeout_q)
            {
               timer_set = false;
               *status = MESSYS__TIMEOUT;
            }
            break;
         case C_REM_MSG_IN:
            ams_rtranslate ( &(mess_in.u.rem_msg_in), reply_q, &locpath,
              &locmessid, message_name_s, message_value_s,
              message_status, message_context, message_name,
              message_length, message_value, status );
            break;
         case C_LOC_GSOC_END_IN:
            ams_endtrans ( &(mess_in.u.loc_msg_in), &locpath,
              &locmessid, message_name_s, message_value_s,
              message_status, message_context, message_name,
              message_length, message_value, status );
            break;
         case C_REM_GSOC_END_IN:
            ams_rendtrans ( &(mess_in.u.rem_msg_in), &locpath,
              &locmessid, message_name_s, message_value_s,
              message_status, message_context, message_name,
              message_length, message_value, status );
            break;
         case C_LOC_DEINIT_IN:
            ams_remove ( mess_in.u.loc_deinit_in.this_task_t_path_num );
            *status = MESSYS__NOTFOUND;
            break;
         case C_REM_DEINIT_IN:
            ams_remove ( mess_in.u.rem_deinit_in.local_task_t_path_num );
            *status = MESSYS__NOTFOUND;
            break;
         case C_LOC_ACK_IN:
            ams_addrest ( &(mess_in.u.loc_ack_in), locpath, locmessid,
              reply_q, status );
            break;
         case C_REM_ACK_IN:
            ams_raddrest ( &(mess_in.u.rem_ack_in), locpath, locmessid,
              reply_q, status );
            break;
         default:
            *status = MESSYS__MSGFUNC;
            break;
      }
   }
   if ( timer_set == true )
   {
      istat = SAI__OK;
      atimer_cantim ( MESSYS__TIMEOUTID, &istat );
   }
}


void ams_init
(
const char *own_name,      /* name of this task (given) */
int *status
)
/*
*+
*  Name:
*     AMS_INIT

*  Purpose:
*     Initialise ams

*  Language:
*     Starlink C

*  Algorithm:
*     Call ams_initeh

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   ams_initeh( own_name, 1, status);
}


void ams_initeh
(
const char *own_name,/* name of this task (given) */
int eh,              /* register exit handler */
int *status
)

/*
*+
*  Name:
*     AMS_INITEH

*  Purpose:
*     Initialise ams

*  Language:
*     Starlink C

*  Algorithm:
*     Initialise the internal data structures.
*
*     Register with msp, obtain the command queue for incoming messages,
*     then create the queues used for this task sending messages to
*     itself. Finally set up the signal handler.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     16-JUN-1992 (IRJ,SKR):
*        Tidied
*     07-JUL-1992 (SKR):
*        Commented
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   int nlen;                  /* length of task name */
   int i;                     /* loop counter */


   if (*status != SAI__OK) return;


   for (i = 0; i < MESSYS__MXTRANS; i++)
   {
      t_trans[i].this_task_ack_q = MSP__NULL_RECEIVEQ;
      ams_freetrans ( i, status );
   }
   for (i = 0; i < MESSYS__MXPATH; i++)
   {
      ams_freepath ( i );
   }
   for (i = 0; i < MESSYS__MXMACH; i++)
   {
      machine_names[i][0] = '\0';
   }

   messys_netqueue[0] = MSP__NULL_SENDQ;
   strcpy ( messys_netname[0], "ADAMNET" );
   strcpy ( messys_netsep[0], "::" );

   messys_netqueue[1] = MSP__NULL_SENDQ;
   strcpy ( messys_netname[1], "ADAMNET_2" );
   strcpy ( messys_netsep[1], "^^" );

/*   TCP/IP adamnet has a name the same as the machine */

   messys_netqueue[2] = MSP__NULL_SENDQ;
/*   strcpy ( messys_netname[2], "ADAMNET_3" ); */
   gethostname ( messys_netname[2], MESSYS__TNAME );
   strcpy ( messys_netsep[2], "!!" );

   messys_netqueue[3] = MSP__NULL_SENDQ;
   strcpy ( messys_netname[3], "ADAMNET_4" );
   strcpy ( messys_netsep[3], "##" );

   nlen = strlen ( own_name );

   if ( ( nlen > 0 ) && ( nlen < MESSYS__TNAME ) )
   {
      strcpy ( taskname, own_name );
      msp_enter_task ( taskname, &command_q, status );
      msp_create_localq ( &sigext_q, &extint_q, status );
      msp_create_localq ( &sigast_q, &astint_q, status );
      msp_create_localq ( &sigresch_q, &resched_q, status );
      msp_create_localq ( &sigtimeout_q, &timeout_q, status );
      msp_create_localq ( &sigkick_q, &kick_q, status );

/*   Add the ams exit handler to those called when the process exits */

      if ( eh ) {
#if USE_ATEXIT
         atexit( ams_exit );		/* ANSI C */
#elif USE_ON_EXIT
         on_exit( ams_exit1, 0);	/* SunOS only */
#endif
      }
   }
   else
   {
      *status = MESSYS__TOOLONG;
   }
}

void ams_kick
(
const char *name, /* name of the action to be rescheduled (given) */
int length,       /* number of significant bytes in value (given) */
const char *value,/* message to be passed to application code (given) */
int *status       /* global status (given and returned) */
)
/*
*+
*  Name:
*     AMS_KICK

*  Purpose:
*     Send a message to this task's kick queue

*  Language:
*     Starlink C

*  Algorithm:
*     Send a soft kick interrupt message 'value' qualified by 'name' to
*     kick_q.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   ams_sendobey ( sigkick_q, name, length, value, status );
}



static void ams_nalookup
(
const char *name,  /* full machine/task name (given) */
int *path,         /* path number for communication to task (returned) */
int *netind,       /* index to network type (returned) */
char *task,        /* task part of name (returned) */
char *mach,        /* machine part of name (returned) */
int *remote,       /* flag for whether task is across network (returned) */
int *status        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_NALOOKUP

*  Language:
*     Starlink C

*  Algorithm:
*     Given name as xxxx::yyyy task will be set to yyyyyy and mach to
*     xxxx:: and remote to 1.  Given yyyyyy, task will be set to yyyyyy
*     mach to "" and remote to zero.
*
*     We start by checking if the name given is prefixed with a known
*     machine name (ie of the form xxxxx::yyyyy).  We check this by
*     scanning all the known machinename/taskname separators (held in
*     messys_netsep[0..netint-1]) and if a separator is found we select
*     out the machinename part (xxxxx) and the taskname (yyyyyyy).
*
*     If one is not found we scan the t_paths array checking for a match
*     in the given name and the "other_taskname" member of each t_paths[]
*     entry. If one is found and the task is local ( machine_num member
*     of t_paths[] is MESSYS_NULL_M) path is set and we return with
*     status set to SAI__OK, otherwise we return with status set to
*     MESSYS_NOTFOUND.
*
*     If a separator is found we scan the t_paths[] array looking for an
*     entry where the 'other_taskname' member and the 'machine_num' entry
*     is positive and when used to index machine_names[] also gives match
*     with xxxxx (ie task yyyyyy running on machine xxxxx has path
*     existing to it).  If we find such a match we return the path indexs
*     in 'path', otherwise we return MESSYS_NOTFOUND.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     03-JUN-1994 (BDK):
*        Return ok status is path already open
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int mno;             /* machine number */
   int n;               /* loop counter */

   if ( *status != SAI__OK ) return;


   ams_analysename ( name, netind, task, mach, remote, status );

   if ( *status == SAI__OK )
   {
      n = 0;
      do
      {
         if ( strcmp ( t_paths[n].other_taskname, task ) == 0)
         {
            mno = t_paths[n].machine_num;
            if ( ( !(*remote) && mno == MESSYS__NULL_M ) ||
              ( (*remote) &&
               mno != MESSYS__NULL_M &&
               strcmp ( machine_names[mno], mach ) == 0) )
            {
               *path = n;
               return;
            }
         }
      }
      while ((n = n + 1) < MESSYS__MXPATH);
      *status = MESSYS__NOTFOUND;
   }
}


static void ams_newtrans
(
const struct a_loc_gsoc_start_in *loc_gsoc_start_in,
                                   /* start-transaction message in internal
                                      format (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *path,                         /* the communications path to the other
                                      task (returned) */
int *messid,                       /* the message identifier for this
                                      transaction (returned) */
int message_name_s,                /* space for name (given) */
int message_value_s,               /* space for value (given) */
int *message_status,               /* message status (returned) */
int *message_context,              /* message context (returned) */
char *message_name,                /* message name (returned) */
int *message_length,               /* length of value (returned) */
char *message_value,               /* message value (returned) */
int *status                        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_NEWTRANS

*  Language:
*     Starlink C

*  Algorithm:
*     System has just received a gsoc_start transaction message.  This
*     function tries to establish a transaction end at this end using
*     ams_getfreetrans() filling in the t_trans[] entry with data from
*     the gsoc message. The only tricky bit here is if the gsoc_flag of
*     the message just received is set to OBEY, then we must establish a
*     transaction acknowledge queue this end (ams_getfreetrans() does
*     this conditional on its second parameter) If we fail to establish a
*     transaction end we send a C_LOC_GSOC_END_OUT message to the other
*     task's transaction acknowledge queue using ams_sendgsocend(). If we
*     succeed in establishing a transaction end we unpack the message for
*     the caller (using ams_unpacklocgsoc()).

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int istat;                        /* local status */

   if ( *status != SAI__OK ) return;


   *path = MESSYS__NULL_P;
   *messid = MESSYS__NULL_T;

   istat = SAI__OK;

   ams_getfreetrans ( ( loc_gsoc_start_in->gsoc_flag == OBEY ),
     loc_gsoc_start_in->this_task_t_path_num, reply_q,
     loc_gsoc_start_in->other_task_t_trans_num, messid, status );

   if ( *status != SAI__OK )
   {
      ams_sendgsocend ( 1, reply_q,
        MESSYS__NULL_T,
        loc_gsoc_start_in->other_task_t_trans_num,
        loc_gsoc_start_in->gsoc_flag,
        loc_gsoc_start_in->gsoc_name,
        loc_gsoc_start_in->gsoc_len,
        MESSYS__TRANSFAIL,
        loc_gsoc_start_in->gsoc_value,
        &istat );
   }
   else
   {
      *path = t_trans[*messid].t_path_num;
      ams_unpacklocgsoc ( loc_gsoc_start_in, message_name_s, message_value_s,
        message_status, message_context, message_name,
        message_length, message_value, status );
   }
}


static void ams_nlookup
(
const char *name, /* task name (given) */
int *path,        /* path number for communication to task (returned) */
int *status       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_NLOOKUP

*  Language:
*     Starlink C

*  Algorithm:
*     Check to see if a path already exists to the task 'name' and if so
*     set path to reflect this.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     17-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int netind, remote;
   char task[MESSYS__TNAME];       /* taskname */
   char mach[MESSYS__MNAME];       /* machine name */

   ams_nalookup ( name, path, &netind, task, mach, &remote, status );
}


void ams_path
(
const char *other_task_name,  /* name of task to which path is required (given) */
int *path,              /* pointer to the path (returned) */
int *status             /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_PATH

*  Purpose:
*     Get a communications path to another task

*  Language:
*     Starlink C

*  Algorithm:
*     Open a path to the task whose name is 'other_task_name' and return
*     the path index in 'path'.
*
*     Using nalookup() we obtain the machinename, taskname and netindex
*     of a remote task (remote = 1) or obtain the local task name (remote
*     = 0) We then obtain a freepath and depending on whether the task is
*     local or remote:
*
*      local:
*                  fill in t_paths[p] with the othertasks command_q (by
*     calling msp_get_task_queue() and set
*     t_paths[].machine_num to NULL_M and
*     t_paths[].path_state to PART_P (nearly opened)
*
*      remote:
*               using messys_call_out() (that also exchanges a
*     C_REM_CALL_OUT message with the remote server) we obtain
*     the remote machine's machine number (an index into
*     machine_names[], a table of known remote host names).
*     Then set t_paths[].other_com_q to messys_netqueue[netind]
*     (which is the command queue for the remote server for
*     that machine set_up when the system is first required to
*     messys_call_out() that remote host),
*     t_paths[].machine_num to the remote host's machine number
*     and t_paths[].path_state to PART_P (nearly opened)
*
*     We then obtain a temporary transaction acknowledge queue using
*     ams_getfreetrans() and then send, using ams_sendinit(), a MESSYS_INIT
*     message and using messys_getreply() obtain the reply.  If this
*     short transaction fails to complete we free the path and any
*     associated transactions using messys_remove(), otherwise we return
*     with 'path' set to the path (index) established.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


{
   int message_status;               /* returned from getreply */
   int message_context;              /* returned from getreply */
   char message_name[32];            /* returned from getreply */
   int message_length;               /* returned from getreply */
   char message_value[MSG_VAL_LEN];  /* returned from getreply */
   int netind;                       /* index to network parameters */
   int wpath;                        /* local version of path */
   int idmess;                       /* messid for init handshake */
   int machnum;                      /* machine number */
   int remote;                       /* flag to hold if other_task_name is
                                        remote */
   char otaskname[MESSYS__TNAME];    /* task name implicit in
                                        other_task_name */
   char machname[MESSYS__MNAME];     /* name of remote machine implicit in
                                        other_task_name */

   if ( *status != SAI__OK ) return;

   ams_nalookup ( other_task_name, path, &netind, otaskname, machname,
     &remote, status );
   if ( *status != MESSYS__NOTFOUND )
   {
      return;
   }

   *status = SAI__OK;
   ams_getfreepath ( path, status);
   if ( *status != SAI__OK )
   {
      return;
   }
   wpath = (*path);
   strcpy ( t_paths[wpath].other_taskname, otaskname );
   if ( !remote )
   {
      if ( *status == SAI__OK )
      {
         msp_get_task_queue ( other_task_name, &(t_paths[wpath].other_com_q),
           status );
         t_paths[wpath].machine_num = MESSYS__NULL_M;
      }
   }
   else
   {
      ams_call_out ( machname, netind, &machnum, status );
      t_paths[wpath].other_com_q = messys_netqueue[netind];
      t_paths[wpath].machine_num = machnum;
   }

   t_paths[wpath].path_state = MESSYS__PART_P;
   ams_getfreetrans ( 1, wpath, MSP__NULL_SENDQ, MESSYS__NULL_T, &idmess,
     status );
   ams_sendinit ( wpath, idmess, status );
   ams_getreply ( MESSYS__INIT_WAIT_TIME, wpath, idmess, 32, MSG_VAL_LEN,
     &message_status, &message_context, message_name, &message_length,
     message_value, status );

   if ( (*status != SAI__OK) && (*path != MESSYS__NULL_P) )
   {
      ams_remove ( wpath );
      *path = MESSYS__NULL_P;
   }
}


void ams_plookup
(
int path,             /* the path number (given) */
char *name,           /* the task name (returned) */
int *status           /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_PLOOKUP

*  Purpose:
*     Look up a taskname given a path to it

*  Language:
*     Starlink C

*  Algorithm:
*     PATHINDEX -> TASKNAME
*
*     Given a path 'path', we check that the path is legal and then use
*     it to index t_paths[] to ascertain whether the path is linked to a
*     remote task or a local task (by checking t_paths[path].machine_num
*     which will be MESSYS__NULL_M if local).
*
*     If the path is connected to a local task, we copy that task's name
*     into 'name' using t_paths[path].other_taskname. If the task is
*     remote we form xxxxx::yyyyyy in 'name', obtaining xxxxxx:: from
*     machine_names[t_paths[path].machine_num] and yyyyyy from
*     t_paths[path].other_taskname.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   if ( *status != SAI__OK ) return;

   AMS_checkpath ( path, status );

   if ( *status == SAI__OK )
   {
      if ( pathfree[path] == true )
      {
         *status = MESSYS__NOTFOUND;
      }
      else
      {
         if ( t_paths[path].machine_num != MESSYS__NULL_M )
         {
            sprintf ( name, "%s%s",
              machine_names[t_paths[path].machine_num],
              t_paths[path].other_taskname );
         }
         else
         {
            strcpy ( name, t_paths[path].other_taskname );
         }
      }
   }
}



static void ams_raccept
(
const struct a_rem_init_in *rem_init_in, /* init request (given) */
sendq_type ackq,                   /* acknowledgement queue (given) */
int *status                        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RACCEPT

*  Language:
*     Starlink C

*  Algorithm:
*     An "init" message has been received from a task on another machine
*     requesting a connecting path to be set up. Allocate a data structure
*     to the path and return an acceptance message.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*      IRJ: Ian R Jenkins (RAL)
*      BDK: B.D.Kelly (ROE)
*      AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     08-FEB-1996 (AJC):
*        Remove diagnostic printfs
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int j;               /* path number */
   int istat;           /* local status */
   int added;           /* flag for new machine name */

   if ( *status != SAI__OK ) return;

   ams_getfreepath ( &j, status );
   ams_getmachnum ( rem_init_in->remote_machine_name,
     &(t_paths[j].machine_num), &added, status );

   if ( *status != SAI__OK )
   {
      istat = SAI__OK;
      ams_senddeinit ( 0, j, ackq, rem_init_in->local_nettask_n_path_num,
        &istat );
   }
   else
   {
      strcpy ( t_paths[j].other_taskname, rem_init_in->remote_taskname );
      t_paths[j].other_pathnum = rem_init_in->local_nettask_n_path_num;
      t_paths[j].other_com_q = ackq;
      t_paths[j].path_state = MESSYS__FULL_P;
      ams_sendinitack ( 0, ackq, rem_init_in->local_nettask_n_path_num, j,
        status );
   }
   *status = SAI__OK;
}


static void ams_raddrest
(
const struct a_rem_ack_in *rem_ack_in,  /* received ack_in structure (given) */
int path,                         /* path of init transaction (given) */
int messid,                       /* messid of init transaction (given) */
sendq_type replyq,                /* queue for sending rejections (given) */
int *status                       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RADDREST

*  Language:
*     Starlink C

*  Algorithm:
*     This task has sent an "init" message to a task on another machine
*     requesting a connecting path to be set up. A positive reply has
*     been received. Check the reply is valid and complete the data
*     structure for the path.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     16-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   if ( *status != SAI__OK ) return;

   if ( ( path == rem_ack_in->local_task_t_path_num ) &&
     ( t_paths[path].path_state == MESSYS__PART_P ) )
   {
      t_paths[path].other_pathnum = rem_ack_in->local_nettask_n_path_num;
      t_paths[path].path_state = MESSYS__FULL_P;
      ams_freetrans ( messid, status );
   }
   else
   {
      ams_senddeinit ( 0, MESSYS__NULL_P, replyq,
        rem_ack_in->local_nettask_n_path_num, status );
      *status = MESSYS__IVACKINIT;
   }
}


void ams_receive
(
int timeout,              /* timeout time in milliseconds (given) */
int message_name_s,       /* space for name (given) */
int message_value_s,      /* space for value (given) */
int *message_status,      /* message status (returned) */
int *message_context,     /* message context (returned) */
char *message_name,       /* message name (returned) */
int *message_length,      /* length of value (returned) */
char *message_value,      /* message value (returned) */
int *path,                /* path on which message received (returned) */
int *messid,              /* message number of incoming message (returned) */
int *status               /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RECEIVE

*  Purpose:
*     Receive any incoming message

*  Language:
*     Starlink C

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     IRJ: I.R.Jenkins (RAL)
*     BDK: B.D. Kelly (ROE)
*     BKM: B.K. McIlwrath (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     17-MAR-1994 (BDK):
*        Use the reply_q returned from msp
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     18-AUG-1994: correct confusion between status and message_status
*     01-DEC-1994 (BKM):
*        Correct 'numq' when transaction queues have gone away
*     08-FEB-1996 (AJC):
*        Remove diagnostic printfs
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_in mess_in;    /* message structure in internal format */
   BOOL message_found;          /* flag for message wait completed */
   BOOL timer_set;              /* flag for timer set */
   int j;                       /* counter for queues */
   int queues[AMS__MXQUEUE];    /* list of active queues */
   int numq;                    /* number of active queues */
   int actual_length;           /* length of message received */
   int queue_id;                /* queue where message received */
   sendq_type reply_q;          /* other task's reply q */
   int istat;                   /* internal status */


   if (*status != SAI__OK) return;

   message_found = false;
   timer_set = false;

   if ( timeout != MESSYS__INFINITE )
   {
      ams_settimeout ( timeout, status );
      if ( *status == SAI__OK )
      {
          timer_set = true;
      }
   }
   queues[0] = extint_q;
   queues[1] = astint_q;
   queues[2] = resched_q;
   queues[3] = timeout_q;
   queues[4] = kick_q;
   queues[5] = command_q;

   while ( ( message_found == false ) && ( *status == SAI__OK) )
   {
      numq = 6;
      for ( j = 0; j < MESSYS__MXTRANS; j++ )
      {
         if ( t_trans[j].this_task_ack_q != MSP__NULL_RECEIVEQ )
         {
            queues[numq] = t_trans[j].this_task_ack_q;
            numq++;
         }
      }

      msp_receive_message ( queues, numq, true, C_MAXMSG_LEN,
        (char *) &mess_in, &actual_length, &queue_id, &reply_q, status );
      if ( *status == SAI__OK )
      {
         switch ( mess_in.mess_in_type )
         {
            case C_LOC_MSG_IN:
               ams_translate ( &(mess_in.u.loc_msg_in), reply_q, path,
                 messid, message_name_s, message_value_s,
                 message_status, message_context, message_name,
                 message_length, message_value, status );
               message_found = true;
               if (queue_id == extint_q)
               {
                  *message_status = MESSYS__EXTINT;
		  *status = SAI__OK;
               }
               else if (queue_id == astint_q)
               {
                  *message_status = MESSYS__ASTINT;
		  *status = SAI__OK;
               }
               else if (queue_id == resched_q)
               {
                  *message_status = MESSYS__RESCHED;
		  *status = SAI__OK;
               }
               else if (queue_id == timeout_q)
               {
                  *message_status = MESSYS__TIMEOUT;
		  *status = SAI__OK;
                  timer_set = false;
               }
               else if (queue_id == kick_q)
               {
                  *message_status = MESSYS__KICK;
		  *status = SAI__OK;
               }
               else if (queue_id == command_q)
               {
                  *status = MESSYS__IVCOMMSG;
               }
               break;
            case C_REM_MSG_IN:
               ams_rtranslate ( &(mess_in.u.rem_msg_in), reply_q, path,
                 messid, message_name_s, message_value_s,
                 message_status, message_context, message_name,
                 message_length, message_value, status );
               message_found = true;
               if (queue_id == command_q)
               {
                *status = MESSYS__IVCOMMSG;
               }
               break;
            case C_LOC_GSOC_START_IN:
               ams_newtrans ( &(mess_in.u.loc_gsoc_start_in), reply_q,
                 path, messid, message_name_s, message_value_s,
                 message_status, message_context, message_name,
                 message_length, message_value, status );
               message_found = true;
               break;
            case C_REM_GSOC_START_IN:
               ams_rnewtrans ( &(mess_in.u.rem_gsoc_start_in), reply_q,
                 path, messid, message_name_s, message_value_s,
                 message_status, message_context, message_name,
                 message_length, message_value, status );
               message_found = true;
               break;
            case C_LOC_GSOC_END_IN:
               ams_endtrans ( &(mess_in.u.loc_msg_in), path, messid,
                 message_name_s, message_value_s,
                 message_status, message_context, message_name,
                 message_length, message_value, status );
               message_found = true;
               break;
            case C_REM_GSOC_END_IN:
               ams_rendtrans ( &(mess_in.u.rem_msg_in), path, messid,
                 message_name_s, message_value_s,
                 message_status, message_context, message_name,
                 message_length, message_value, status );
               message_found = true;
               break;
            case C_LOC_INIT_IN:
               ams_accept ( &(mess_in.u.loc_init_in), reply_q, status );
               break;
            case C_REM_INIT_IN:
               ams_raccept ( &(mess_in.u.rem_init_in), reply_q, status );
               break;
            case C_LOC_DEINIT_IN:
               ams_remove (mess_in.u.loc_deinit_in.this_task_t_path_num );
               *status = SAI__OK;
               break;
            case C_REM_DEINIT_IN:
               ams_remove ( mess_in.u.rem_deinit_in.local_task_t_path_num );
               *status = SAI__OK;
               break;
            case C_LOC_ACK_IN:
               ams_reject ( &(mess_in.u.loc_ack_in), reply_q, status );
               break;
            case C_REM_ACK_IN:
               ams_rreject ( &(mess_in.u.rem_ack_in), reply_q, status );
               break;
            default:
               *status = MESSYS__MSGFUNC;
               break;
         }
      }
   }
   if ( timer_set )
   {
      istat = SAI__OK;
      atimer_cantim ( MESSYS__TIMEOUTID, &istat );
   }
}


static void ams_reject
(
const struct a_loc_ack_in *loc_ack_in,   /* received ack_in structure (given) */
sendq_type reply_q,                /* queue for sending reply (given) */
int *status                        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_REJECT

*  Language:
*     Starlink C

*  Algorithm:
*     The system has received an unexpected local acknowledgement to an
*     init message. It therefore sends a de_init to the source, whose
*     acknowledge queue is in the replyq part of the received
*     acknowledgement. We use ams_senddeinit() to actually send the message.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   if ( *status != SAI__OK ) return;

   ams_senddeinit ( 1, MESSYS__NULL_P, reply_q,
     loc_ack_in->other_task_t_path_num, status );
   *status = SAI__OK;
}


static void ams_remove
(
int pathnum              /* path number (given) */
)

/*
*+
*  Name:
*     AMS_REMOVE

*  Language:
*     Starlink C

*  Algorithm:
*     This routine removes the path 'pathnum' from the perceived list of
*     active paths (pathfree[] == false).  By using ams_freepath() to do this
*     it checks to see if any currently active transactions use this path
*     (t_trans[].t_path_num == pathnum) and if so, frees the acknowledge
*     queue associated with that transaction using
*     msp_delete_queue(t_trans[].this_task_ack_q) and then frees the
*     transaction entry t_trans[].

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   int istat;               /* local status */

   istat = SAI__OK;
   AMS_checkpath(pathnum, &istat);
   if ( istat == SAI__OK )
   {
      ams_freepath ( pathnum );
   }
}


static void ams_rendtrans
(
const struct a_rem_msg_in *rem_msg_in,  /* the end-transaction message in
                                     internal format (given) */
int *path,                        /* the communications path to the other
                                     task (returned) */
int *messid,                      /* the message identifier for this
                                     transaction (returned) */
int message_name_s,               /* space for name (given) */
int message_value_s,              /* space for value (given) */
int *message_status,              /* message status (returned) */
int *message_context,             /* message context (returned) */
char *message_name,               /* message name (returned) */
int *message_length,              /* length of value (returned) */
char *message_value,              /* message value (returned) */
int *status                       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RENDTRANS

*  Language:
*     Starlink C

*  Algorithm:
*     The system has received a remote gsoc_end message, the last message
*     in the transaction 'rem_msg_in->local_task_t_trans_num' (set into
*     *messid) on path t_trans[*messid].t_path_num (set into *path.).
*     Using ams_unpackremmsg() this routine returns the contents of the
*     last message to the caller and uses ams_freetrans() to close and
*     free the relevant transaction.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   if ( *status != SAI__OK ) return;

   *messid = rem_msg_in->local_task_t_trans_num;
   *path = t_trans[*messid].t_path_num;
   ams_unpackremmsg ( rem_msg_in, message_name_s, message_value_s,
     message_status, message_context, message_name,
     message_length, message_value, status );
   ams_freetrans ( *messid, status );
}


void ams_reply
(
int path,               /* the path number for communicating with the other
                           task (given) */
int messid,             /* the number identifying the transaction (given) */
int message_function,   /* message function (given) */
int message_status,     /* message status (given) */
int message_context,    /* message context (given) */
const char *message_name,/* message name (given) */
int message_length,     /* length of value (given) */
const char *message_value,/* message value (given) */
int *status             /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_REPLY

*  Purpose:
*     Send a message on a given path, messid

*  Language:
*     Starlink C

*  Algorithm:
*     As part of transaction 'messid' on path 'path', the user wishes to
*     send the message AS A REPLY to a previously received message from
*     the other end of the path. The user can ONLY reply with a de_init
*     message (something has gone wrong) or with a transaction end
*     message (GSOC_END) or with a normal message.
*
*     We first check that the path is open (AMS_checkpathopen()) and that the
*     transaction is in use (AMS_checktrans()).  If both these are OK we
*     check the 'function' part of the external form and check exactly

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     27-APR-1993 (AJC):
*        Use AMS_checktrans not AMS_checktransactive
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  What Is Being Sent:
*       MESSYS_DE_INIT
*
*              then we use ams_senddeinit() and if this fails then
*       messsy_remove() to remove the path and free any
*       transactions using the path.
*
*      MESSYS__MESSAGE
*
*              then we use ams_sendmessage() to send the message.  It is
*       inside ams_sendmessage() that an end of transaction
*       message is detected through an examination of the message
*       status (and, by the way, the transaction end at this end
*       is then freed using ams_freetrans())
*
*      other
*
*              the system returns MESSYS__MSGFUNC

*-
*/

{
   if ( *status != SAI__OK ) return;

   AMS_checkpathopen(path, status);
   if ( *status == SAI__OK )
   {
      AMS_checktrans(messid, status);
   }
   if ( *status == SAI__OK)
   {
      switch ( message_function )
      {
         case MESSYS__DE_INIT:
            ams_senddeinit ( (t_paths[path].machine_num == MESSYS__NULL_M ),
              path,
              t_trans[messid].other_task_ack_q,
              t_paths[path].other_pathnum,
              status );
            break;
         case MESSYS__MESSAGE:
            ams_sendmessage ( path, messid, message_status,
              message_context, message_name, message_length,
              message_value, status );
            break;
         default:
            *status = MESSYS__MSGFUNC;
            break;
      }
   }
}


void ams_resmsg
(
int length,        /* number of significant bytes in value (given) */
const char *value, /* message to be passed to main-line code (given) */
int *status        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RESMSG

*  Purpose:
*     Send a message to this task's  reschedule queue

*  Language:
*     Starlink C

*  Algorithm:
*     Send a soft reschedule interrupt message 'value' qualified by
*     'name' to resched_q.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   ams_sendobey ( sigresch_q, MSG_EMPTYNAME, length, value, status );
}


static void ams_rnewtrans
(
const struct a_rem_gsoc_start_in *rem_gsoc_start_in,
                                   /* the initialize-transaction message in
                                      internal format (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *path,                         /* the communications path to the other
                                      task (returned) */
int *messid,                       /* the message identifier for this
                                      transaction (returned) */
int message_name_s,                /* space for name (given) */
int message_value_s,               /* space for value (given) */
int *message_status,               /* message status (returned) */
int *message_context,              /* message context (returned) */
char *message_name,                /* message name (returned) */
int *message_length,               /* length of value (returned) */
char *message_value,               /* message value (returned) */
int *status                        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RNEWTRANS

*  Language:
*     Starlink C

*  Algorithm:
*     System has just received a gsoc_start transaction message from a
*     remote source.  This function tries to establish a transaction end
*     at this end using ams_getfreetrans() filling in the t_trans[] entry
*     with data from the gsoc message.  The only tricky bit here is if
*     the gsoc_flag of the message just received is set to OBEY, then we
*     must establish a transaction acknowledge queue this end
*     (ams_getfreetrans() does this conditional on its second parameter).
*
*     If we fail to establish a transaction end we send a
*     C_REM_GSOC_END_OUT message using ams_sendgsocend() to the local
*     nettask's acknowledge queue.
*
*     If we succeed in establishing a transaction end we unpack the
*     message for the caller (using) ams_unpackremgsoc()).

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int istat;                  /* local status */

   if ( *status != SAI__OK ) return;

   *path = MESSYS__NULL_P;
   *messid = MESSYS__NULL_T;
   istat = SAI__OK;
   ams_getfreetrans ( (rem_gsoc_start_in->gsoc_flag == OBEY),
     rem_gsoc_start_in->local_task_t_path_num, reply_q,
     rem_gsoc_start_in->local_nettask_n_trans_num, messid, status );
   if ( *status != SAI__OK )
   {
      ams_sendgsocend ( 0, reply_q,
        MESSYS__NULL_T,
        rem_gsoc_start_in->local_nettask_n_trans_num,
        rem_gsoc_start_in->gsoc_flag,
        rem_gsoc_start_in->gsoc_name,
        rem_gsoc_start_in->gsoc_len,
        MESSYS__TRANSFAIL,
        rem_gsoc_start_in->gsoc_value,
        &istat );
   }
   else
   {
      *path = t_trans[*messid].t_path_num;
      ams_unpackremgsoc ( rem_gsoc_start_in, message_name_s, message_value_s,
        message_status, message_context, message_name,
        message_length, message_value, status );
   }
}


static void ams_rreject
(
const struct a_rem_ack_in *rem_ack_in,   /* received ack_in structure (given) */
sendq_type reply_q,                /* queue for sending rejection (given) */
int *status                        /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RREJECT

*  Language:
*     Starlink C

*  Algorithm:
*     The system has received an unexpected remote acknowledgement to an
*     init message. It therefore sends a de_init to the source using
*     ams_senddeinit() message.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   if ( *status != SAI__OK ) return;

   ams_senddeinit ( 0, MESSYS__NULL_P, reply_q,
     rem_ack_in->local_nettask_n_path_num, status );
   *status = SAI__OK;
}


static void ams_rtranslate
(
const struct a_rem_msg_in *rem_msg_in,  /* the message in internal format
                                           (given) */
sendq_type reply_q,               /* reply queue (given) */
int *path,                        /* the communications path to the other
                                     task (returned) */
int *messid,                      /* the message identifier for this
                                     transaction (returned) */
int message_name_s,               /* space for name (given) */
int message_value_s,              /* space for value (given) */
int *message_status,              /* message status (returned) */
int *message_context,             /* message context (returned) */
char *message_name,               /* message name (returned) */
int *message_length,              /* length of value (returned) */
char *message_value,              /* message value (returned) */
int *status                       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_RTRANSLATE

*  Language:
*     Starlink C

*  Algorithm:
*     The system has received a remote message as part of the transaction
*     'rem_msg_in->local_task_t_trans_num' (set into *messid) on path
*     t_trans[*messid].t_path_num (set into *path).
*
*     This routine updates the transaction entry for this end (in
*     particular adding the other task's acknowledge queue and
*     transaction end index if the existing transaction end this end is
*     missing this information) and then uses ams_unpackremmsg() to
*     unpack the message contents. If the path associated with the
*     transaction has been closed or changed this routine sets *status to
*     MESSYS_BADPATH but still unpacks the message.  If the transaction
*     referred to in the message is invalid then *status will reflect
*     this but the message will still be unpacked.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int locmessid;     /* transaction number received */
   int istat;         /* local status */

   if (*status != SAI__OK) return;


   *path = MESSYS__NULL_P;
   *messid = MESSYS__NULL_T;
   locmessid = rem_msg_in->local_task_t_trans_num;
   AMS_checktrans ( locmessid, status );
   if ( *status != MESSYS__BADMESS )
   {
      *messid = locmessid;
   }
   if (*status == SAI__OK)
   {
      if ( (*path = t_trans[locmessid].t_path_num) != MESSYS__NULL_P )
      {
         t_trans[locmessid].other_task_ack_q =
           reply_q;
         t_trans[locmessid].other_transnum =
           rem_msg_in->local_nettask_n_trans_num;
      }
      else
      {
         *status = MESSYS__BADPATH;
      }
   }
   istat = SAI__OK;
   ams_unpackremmsg ( rem_msg_in, message_name_s, message_value_s,
     message_status, message_context, message_name,
     message_length, message_value, status );
}


void ams_send
(
int path,               /* pointer to the path (given) */
int message_function,   /* message function (given) */
int message_status,     /* message status (given) */
int message_context,    /* message context (given) */
const char *message_name,     /* message name (given) */
int message_length,     /* length of value (given) */
const char *message_value,    /* message value (given) */
int *messid,            /* message number issued by this task (returned) */
int *status             /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SEND

*  Purpose:
*     Send a message on a given path

*  Language:
*     Starlink C

*  Algorithm:
*     This function is used by the application code to send a message
*     to another task on path 'path'.
*
*     The expectation is that this is one of a DEINIT message, an INIT
*     message or the first message of a NEW transaction whose transaction
*     number is to be set into '*messid'.
*
*     When used for the first message of a transaction, a free
*     transaction is obtained using ams_getfreetrans() and the MESSYS_MESSAGE
*     is sent to the other task's command queue.
*
*     When used to send an INIT message a free temporary transaction is
*     obtained using ams_getfreetrans() and the MESSYS_INIT (using
*     ams_sendinit()) is sent to the other task's command queue.
*
*     When used to send a DEINIT, ams_senddeinit() is used to send the
*     MESSYS_DE_INIT message to the other task's command queue.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     16-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Pass message components as separate arguments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{

   if ( *status != SAI__OK ) return;


   AMS_checkpathopen ( path, status );

   if ( *status == SAI__OK )
   {
      switch (message_function)
      {
         case MESSYS__INIT:
         case MESSYS__MESSAGE:
            ams_getfreetrans ( 1, path, MSP__NULL_SENDQ,
              MESSYS__NULL_T, messid, status );
            if ( *status != SAI__OK )
            {
               return;
            }
            else if ( message_function == MESSYS__INIT )
            {
               ams_sendinit ( path, *messid, status );
            }
            else
            {
               ams_sendgsocstart ( path, *messid, message_status,
                 message_context, message_name, message_length,
                 message_value, status );
            }
            break;
         case MESSYS__DE_INIT:
            ams_senddeinit ( (t_paths[path].machine_num == MESSYS__NULL_M ),
              path, t_paths[path].other_com_q, t_paths[path].other_pathnum,
              status );
            break;
         default:
            *status = MESSYS__MSGFUNC;
            break;
      }
   }
}



static void ams_senddeinit
(
int local,             /* flag for local or remote message (given) */
int path,              /* path number (given) */
sendq_type targetq,    /* queue for sending message (given) */
int otherpathno,       /* path number in other task (given) */
int *status            /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SENDDEINIT

*  Language:
*     Starlink C

*  Algorithm:
*     System wishes to send a de_init message to queue targetq.
*     The format of such a message is
*
*     [C_LOC_DEINIT_OUT,othertaskscommandq,othertaskspathnamber]
*
*     or
*
*     [C_REM_DEINIT_OUT,othertaskscommandq,othertaskspathnamber]
*
*     depending on local (local = 1) or remote (local = 0) target.
*
*     This routine sends such a message using msp_send_message() returning
*     the status from this. If 'path' is not set to MESSYS__NULL_P, then
*     ams_freepath(path) is called to release the path.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_out mess_out;            /* the message sent */

   if ( *status != SAI__OK ) return;
   memset( &mess_out, 0, sizeof(mess_out));

   if (local)
   {
      mess_out.mess_out_type = C_LOC_DEINIT_OUT;
      mess_out.u.loc_deinit_out.other_task_t_path_num = otherpathno;
      msp_send_message ( (char *) (&mess_out), C_LOC_DEINIT_OUT_LEN,
        targetq, command_q, status );
   }
   else
   {
      mess_out.mess_out_type = C_REM_DEINIT_OUT;
      mess_out.u.rem_deinit_out.local_nettask_n_path_num = otherpathno;
      msp_send_message ( (char *) (&mess_out), C_REM_DEINIT_OUT_LEN,
        targetq, command_q, status );
   }
   if ( path != MESSYS__NULL_P )
   {
      ams_freepath ( path );
   }
}



static void ams_sendgsocend
(
int local,              /* flag for local or remote message (given) */
sendq_type targetq,     /* queue for sending message (given) */
int tttn,               /* this task transaction number (given) */
int ottn,               /* other task transaction number (given) */
int gflag,              /* gsoc flag (given) */
const char *gname,      /* message name field (given) */
int glen,               /* length of message value (given) */
int gstatus,            /* message status (given) */
const char *gvalue,     /* message value field (given) */
int *status             /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SENDGSOCEND

*  Language:
*     Starlink C

*  Algorithm:
*     System wishes to send a gsocend message to queue targetq.
*     The format of such a message is (local)
*
*     [C_LOC_GSOC_END_OUT
*     the othertask's transaction number, from ottn
*     thistask's transaction number,      from tttn
*     the message's gsoc flag,            from gflag
*     the message's name qualifier,       from gname
*     the message's length,               from glen
*     the message's status,               from gstatus
*     the message's value                 from gval
*
*
*     or (!local)
*
*     [C_REM_GSOC_END_OUT
*     local nettask's transaction number,              from ottn
*     thistask's (local_task) transaction number,      from tttn
*     the message's gsoc flag,                         from gflag
*     the message's name qualifier,                    from gname
*     the message's length,                            from glen
*     the message's status,                            from gstatus
*     the message's value                              from gval
*
*     If this task's transaction number, 'tttn' is not null, that is we
*     have just sent an end of transaction message as part of an active
*     transaction (as distinct from a failed attempt to establish a
*     transaction end (see newtrans() and rnewtrans())) we use
*     ams_freetrans() to release the transaction recorded in t_trans[].

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_out mess_out;            /* the message sent */
   int istat;                             /* local status */

   if ( *status != SAI__OK ) return;
   memset( &mess_out, 0, sizeof(mess_out));

   if (local)
   {
      mess_out.mess_out_type = C_LOC_GSOC_END_OUT;
      mess_out.u.loc_msg_out.this_task_t_trans_num = tttn;
      mess_out.u.loc_msg_out.other_task_t_trans_num = ottn;
      mess_out.u.loc_msg_out.gsoc_flag = gflag;
      strcpy ( mess_out.u.loc_msg_out.gsoc_name, gname );
      mess_out.u.loc_msg_out.gsoc_len = glen;
      mess_out.u.loc_msg_out.gsoc_status = gstatus;
      memcpy ( mess_out.u.loc_msg_out.gsoc_value, gvalue, glen );
      msp_send_message ( (char *) &mess_out,
                   C_LOC_MSG_OUT_LEN - MSG_VAL_LEN + glen,
                   targetq, command_q, status );
   }
   else
   {
      mess_out.mess_out_type = C_REM_GSOC_END_OUT;
      mess_out.u.rem_msg_out.local_nettask_n_trans_num = ottn;
      mess_out.u.rem_msg_out.local_task_t_trans_num = tttn;
      mess_out.u.rem_msg_out.gsoc_flag = gflag;
      strcpy ( mess_out.u.rem_msg_out.gsoc_name, gname );
      mess_out.u.rem_msg_out.gsoc_len = glen;
      mess_out.u.rem_msg_out.gsoc_status = gstatus;
      memcpy ( mess_out.u.rem_msg_out.gsoc_value, gvalue, glen );
      msp_send_message ( (char *) &mess_out,
                   C_REM_MSG_OUT_LEN - MSG_VAL_LEN + glen,
                   targetq, command_q, status );
   }
   if (tttn != MESSYS__NULL_T)
   {
      istat = SAI__OK;
      ams_freetrans ( tttn, &istat );
   }
}


static void ams_sendgsocstart
(
int path,               /* path number (given) */
int messid,             /* transaction number (given) */
int message_status,     /* message status (given) */
int message_context,    /* message context (given) */
const char *message_name,     /* message name (given) */
int message_length,     /* length of value (given) */
const char *message_value,    /* message value (given) */
int *status             /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SENDGSOCSTART

*  Language:
*     Starlink C

*  Algorithm:
*     The caller wishes to send the first message on path 'path' in
*     messys internal format as the start of a new transaction 'messid'
*     using the command queue of the other task associated with this path
*     (held in t_paths[path].other_com_q) We need to send a local/remote
*     gsoc_start message:- Depending on whether the target is remote or
*     local (decerned by looking at t_paths[path].machine_num which will
*     be MESSYS__NULL_M in the case of a local task) this code sends
*
*     (local)
*
*     [C_LOC_GSOC_START_OUT,
*     the othertask's command queue,  from t_paths[path].other_com_q
*     the othertask's pathnumber,     from t_paths[path].other_pathnum
*     thistask's transaction number,  from messid
*     thistask's acknowledge queue,   from t_trans[messid].this_task_ack_q
*     the message's gsoc flag,        from message_context
*     the message's name qualifier,   from message_name
*     the message's length,           from message_length
*     the message's value             from message_value
*
*
*     or (remote)
*
*     [C_REM_GSOC_START_OUT,
*     local_nettask's command queue,    from t_paths[path].other_com_q
*     local nettask's path number,      from t_paths[path].other_pathnum
*     thistask's (local_task) transaction number,
*                                       from messid
*     thistask's acknowledge queue(local_task_init_ack_q),
*                                       from t_trans[messid].this_task_ack_q
*     thistask's (local_task)name,      from the global taskname
*     the message's gsoc flag,          from message_context
*     the message's name qualifier,     from message_name
*     the message's length,             from message_length
*     the message's value               from message_value
*

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994 (BDK):
*        Pass message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     2013-06-18 (TIMJ):
*        Trap buffer overflow for large message_length
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_out mess_out;   /* message in internal format */
   int nbytes;                   /* length of message sent */
   int local;                    /* flag for local or remote
                                    communications */

   if ( *status != SAI__OK ) return;

   /* Sanity check for buffer overflow */
   if (message_length > MSG_VAL_LEN) {
     /* will not be able to put this message in mess_out so we have to
        generate an error (truncating it would not be helpful) */
     *status = MESSYS__BUFOV;
     return;
   }

   /* initialise the struct before filling it */
   memset( &mess_out, 0, sizeof(mess_out) );

   local = (t_paths[path].machine_num == MESSYS__NULL_M);

   if (local)
   {
      mess_out.mess_out_type = C_LOC_GSOC_START_OUT;
      mess_out.u.loc_gsoc_start_out.other_task_t_path_num =
          t_paths[path].other_pathnum;
      mess_out.u.loc_gsoc_start_out.this_task_t_trans_num = messid;
      mess_out.u.loc_gsoc_start_out.gsoc_flag = message_context;
      strcpy ( mess_out.u.loc_gsoc_start_out.gsoc_name, message_name );
      mess_out.u.loc_gsoc_start_out.gsoc_len = message_length;
      memcpy ( mess_out.u.loc_gsoc_start_out.gsoc_value, message_value,
            message_length);
      nbytes = C_LOC_GSOC_START_OUT_LEN - MSG_VAL_LEN + message_length;
   }
   else
   {
      mess_out.mess_out_type = C_REM_GSOC_START_OUT;
      mess_out.u.rem_gsoc_start_out.local_nettask_n_path_num =
          t_paths[path].other_pathnum;
      mess_out.u.rem_gsoc_start_out.local_task_t_trans_num = messid;
      mess_out.u.rem_gsoc_start_out.gsoc_flag = message_context;
      memcpy ( mess_out.u.rem_gsoc_start_out.gsoc_name, message_name,
            MSG_NAME_LEN );
      mess_out.u.rem_gsoc_start_out.gsoc_len = message_length;
      memcpy ( mess_out.u.rem_gsoc_start_out.gsoc_value, message_value,
            message_length );
      nbytes = C_REM_GSOC_START_OUT_LEN - MSG_VAL_LEN + message_length;
   }
   msp_send_message ( (char *) &mess_out, nbytes, t_paths[path].other_com_q,
               t_trans[messid].this_task_ack_q, status);
   if ( *status != SAI__OK )
   {
      ams_remove(path);
   }
}


static void ams_sendinit
(
int path,           /* path for init (given) */
int messid,         /* temporary transaction (given) */
int *status         /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SENDINIT

*  Language:
*     Starlink C

*  Algorithm:
*     The caller wishes to send an init message for path 'path'
*     as part of the temporary transaction 'messid'.
*     Depending on whether the target is remote or local
*     (decerned by looking at t_paths[path].machine_num which
*     will be MESSYS__NULL_M in the case of a local task)
*     this code sends (local)
*
*     [C_LOC_INIT_OUT,
*     the othertask's command queue,      from t_paths[path].other_com_q
*     thistask's name,                    from the global taskname
*     the othertask's name,               from t_paths[path].other_taskname
*     thistask's pathnumber,              from path
*     thistask's init acknowledge queue,  from t_trans[messid].this_task_ack_q
*     thistask's command queue,           from the global command_q
*
*
*     or (remote)
*
*     [C_REM_INIT_OUT,
*     local_nettask_command_q,     from t_paths[path].other_com_q
*     thistask's (local_task)name, from the global taskname
*     remote_taskname,             from t_paths[path].other_taskname
*     remote_machine_name,         from machine_names[t_paths[path].machine_num]
*     thistask's pathnumber(local_task_t_path_num),      from path
*     thistask's init acknowledge queue(local_task_init_ack_q),
*                                  from t_trans[messid].this_task_ack_q
*     thistask's command queue(local_task_com_q),
*                                  from the global command_q
*
*
*     to the t_paths[path].other_com_q (the other task's command queue)
*     using msp_send_message(). If the send fails, then ams_remove() is
*     used to remove the path connection to the other task.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_out mess_out;   /* message sent */
   int nbytes;                   /* length of message sent */

   if ( *status != SAI__OK ) return;

   /* initialise the struct */
   memset( &mess_out, 0, sizeof(mess_out) );

   if ( t_paths[path].machine_num == MESSYS__NULL_M )
   {
      mess_out.mess_out_type = C_LOC_INIT_OUT;
      strcpy ( mess_out.u.loc_init_out.this_taskname, taskname );
      strcpy ( mess_out.u.loc_init_out.other_taskname,
        t_paths[path].other_taskname );
      mess_out.u.loc_init_out.this_task_t_path_num = path;
      nbytes = C_LOC_INIT_OUT_LEN;
   }
   else
   {
      mess_out.mess_out_type = C_REM_INIT_OUT;
      strcpy ( mess_out.u.rem_init_out.local_taskname, taskname );
      strcpy ( mess_out.u.rem_init_out.remote_taskname,
        t_paths[path].other_taskname );
      strcpy ( mess_out.u.rem_init_out.remote_machine_name,
            machine_names[t_paths[path].machine_num] );
      mess_out.u.rem_init_out.local_task_t_path_num = path;
      nbytes = C_REM_INIT_OUT_LEN;
   }
   msp_send_message ( (char *) &mess_out, nbytes,
     t_paths[path].other_com_q, t_trans[messid].this_task_ack_q, status);
   if ( *status != SAI__OK )
   {
      ams_remove(path);
   }
}


static void ams_sendinitack
(
int local,            /* flag for local or remote (given) */
sendq_type ackq,      /* the other task's init acknowldege queue (given) */
int otherpathno,      /* other task's path number (given) */
int thispathno,       /* this task's path number (given) */
int *status           /* global status (give and returned) */
)

/*
*+
*  Name:
*     AMS_SENDINITACK

*  Language:
*     Starlink C

*  Algorithm:
*     The system has successfully set up a path end 'thispathno' in
*     reponse to a INIT path request and now wishes to send a INIT path
*     acknowlege message to the acknowledge queue of the requester
*     ('ackq') whose path end index was 'otherpathno'. We, depending on
*     'local', send
*
*      local (local == 1)
*
*     [C_LOC_ACK_OUT,
*     the othertasks path end index                  from otherpathno
*     this tasks recently established path end index from thispathno
*
*
*     or remote (local == 0)
*
*     [C_REM_ACK_OUT,
*     the local nettasks path end index              from otherpathno
*     this tasks recently established path end index from thispathno
*
*
*     using msp_send_message() with target queue 'ackq'.  If the send
*     failed we use ams_freepath() to release the path end in the
*     knowledge that the othertask will timeout waiting for the ack and
*     free its path end as well.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_out mess_out;   /* message sent */
   int nbytes;                   /* length of message sent */

   if ( *status != SAI__OK ) return;

   memset( &mess_out, 0, sizeof(mess_out));

   if (local)
   {
      mess_out.mess_out_type = C_LOC_ACK_OUT;
      mess_out.u.loc_ack_out.other_task_t_path_num = otherpathno;
      mess_out.u.loc_ack_out.this_task_t_path_num = thispathno;
      nbytes = C_LOC_ACK_OUT_LEN;
   }
   else
   {
      mess_out.mess_out_type = C_REM_ACK_OUT;
      mess_out.u.rem_ack_out.local_nettask_n_path_num = otherpathno;
      mess_out.u.rem_ack_out.local_task_t_path_num = thispathno;
      nbytes = C_REM_ACK_OUT_LEN;
   }
   msp_send_message ( (char *) (&mess_out), nbytes, ackq, command_q,
     status );

   if ( *status != SAI__OK )
   {
      ams_freepath ( thispathno );
   }
}


static void ams_sendmessage
(
int path,                /* path number (given) */
int messid,              /* transaction number (given) */
int message_status,      /* message status (given) */
int message_context,     /* message context (given) */
const char *message_name,      /* message name (given) */
int message_length,      /* length of value (given) */
const char *message_value,     /* message value (given) */
int *status              /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SENDMESSAGE

*  Language:
*     Starlink C

*  Algorithm:
*     The caller wishes to send the message on path 'path' in messys
*     internal format as part of transaction 'messid' using the
*     othertask's transaction acknowledge queue associated with this
*     transaction on this path.
*
*     We need to send either a local/remote MSG_OUT message OR depending
*     on the message status being MESSYS__PARAMREQ or MESSYS__PARAMREP or
*     MESSYS__INFORM or MESSYS__SYNC or MESSYS__SYNCREP or
*     MESSYS__TRIGGER or DTASK__ACTSTART a local/remote GSOC_END message
*     using ams_sendgsocend().
*
*     Depending on whether the target is remote or local (discerned by
*     looking at t_paths[path].machine_num which will be MESSYS__NULL_M
*     in the case of a local task) this code sends
*
*     (local)
*
*     [C_LOC_MSG_OUT (depending on status member of source),
*     the othertask's acknowledge queue,  from t_trans[messid].other_task_ack_q
*     thistask's acknowldege queue,       from t_trans[messid].this_task_ack_q
*     the othertask's transaction number, from t_trans[messid].other_transnum
*     thistask's transaction number,      from messid
*     the message's gsoc flag,            from message_context
*     the message's name qualifier,       from message_name
*     the message's length,               from message_length
*     the message's status,               from message_status
*     the message's value                 from message_value
*
*
*     or (remote)
*
*     [C_REM_MSG_OUT (depending on status member of source),
*     local_nettask's acknowledge queue,   from t_trans[messid].other_task_ack_q
*     local nettask's transaction number,  from t_trans[messid].other_transnum
*     thistask's acknowledge queue(local_task__ack_q),
*                                          from t_trans[messid].this_task_ack_q
*     thistask's (local_task) transaction number,
*                                          from messid
*     the message's gsoc flag,             from message_context
*     the message's name qualifier,        from message_name
*     the message's length,                from message_length
*     the message's status,                from message_status
*     the message's value                  from message_value
*
*
*     to the target queue targetq using msp_send_message().
*
*     Note : If the code ends up sending a GSOC_END message using
*     ams_sendgsocend() then within ams_sendgsocend() ams_freetrans() is
*     used to free the transaction entry in transfree[] and t_trans[] as
*     the transaction just completed. If the send fails, then
*     ams_remove() is used to remove the path connection to the other
*     task, otherwise we return SAI__OK.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994 (BDK):
*        Pass message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


{
   struct a_mess_out mess_out;   /* message sent */
   sendq_type targetq;           /* queue for sending the message */
   int nbytes;                   /* length of message sent */
   int sourcestatus;             /* message status */
   int local;                    /* flag for local or remote */

   if ( *status != SAI__OK ) return;

   memset( &mess_out, 0, sizeof(mess_out));
   local = (t_paths[path].machine_num == MESSYS__NULL_M);
   targetq = t_trans[messid].other_task_ack_q;
   sourcestatus = message_status;
   if ( ( sourcestatus == MESSYS__PARAMREQ ) ||
     ( sourcestatus == MESSYS__PARAMREP ) ||
     ( sourcestatus == MESSYS__INFORM )   ||
     ( sourcestatus == MESSYS__SYNC )     ||
     ( sourcestatus == MESSYS__SYNCREP )  ||
     ( sourcestatus == MESSYS__TRIGGER )  ||
     ( sourcestatus == DTASK__ACTSTART ) )
   {
      if (local)
      {
          mess_out.mess_out_type = C_LOC_MSG_OUT;
          mess_out.u.loc_msg_out.other_task_t_trans_num =
            t_trans[messid].other_transnum;
          mess_out.u.loc_msg_out.this_task_t_trans_num = messid;
          mess_out.u.loc_msg_out.gsoc_flag = message_context;
          strcpy ( mess_out.u.loc_msg_out.gsoc_name, message_name );
          mess_out.u.loc_msg_out.gsoc_len = message_length;
          mess_out.u.loc_msg_out.gsoc_status = sourcestatus;
          memcpy ( mess_out.u.loc_msg_out.gsoc_value, message_value,
              message_length );
          nbytes = C_LOC_MSG_OUT_LEN - MSG_VAL_LEN + message_length;
      }
      else
      {
          mess_out.mess_out_type = C_REM_MSG_OUT;
          mess_out.u.rem_msg_out.local_nettask_n_trans_num =
            t_trans[messid].other_transnum;
          mess_out.u.rem_msg_out.local_task_t_trans_num = messid;
          mess_out.u.rem_msg_out.gsoc_flag = message_context;
          strcpy ( mess_out.u.rem_msg_out.gsoc_name, message_name );
          mess_out.u.rem_msg_out.gsoc_len = message_length;
          mess_out.u.rem_msg_out.gsoc_status = sourcestatus;
          memcpy ( mess_out.u.rem_msg_out.gsoc_value, message_value,
              message_length );
          nbytes = C_REM_MSG_OUT_LEN - MSG_VAL_LEN + message_length;
      }
      msp_send_message ( (char *) &mess_out, nbytes, targetq,
        t_trans[messid].this_task_ack_q, status );
   }
   else
   {
      ams_sendgsocend(local, targetq,
                messid, t_trans[messid].other_transnum, message_context,
                message_name, message_length, sourcestatus,
                message_value, status);
   }
   if ( *status != SAI__OK )
   {
      ams_remove(path);
   }
}


static void ams_sendobey
(
sendq_type targetq,    /* target queue identifier (given) */
const char *name,      /* qualifier of message (given) */
int length,            /* number of significant bytes in value (given) */
const char *value,     /* message to be passed to main-line code (given) */
int *status            /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_SENDOBEY

*  Language:
*     Starlink C

*  Algorithm:
*     When an the application wishes to send a soft interrupt (ast/kick
*     etc) the messys layer sends itself a message on a queue particular
*     to the hardware source of the interrupt. This routine sends
*
*     [C_LOC_MSG_OUT,nullt,nullt,OBEY,name,length,SAI__OK,value]
*
*     to queue targetq using msp_send_message() using command_q as a
*     reply queue. The given length specifies the length of the 'value'
*     part of the message. The name argument (whose length must be
*     MSG_NAME_LEN or less) qualifies the message being sent in 'value'
*     See messys_struct.h for layout of messys messages.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   struct a_mess_out mess_out;      /* the message sent */

   if ( *status != SAI__OK ) return;

   memset( &mess_out, 0, sizeof(mess_out));
   mess_out.mess_out_type = C_LOC_MSG_OUT;
   mess_out.u.loc_msg_out.other_task_t_trans_num = MESSYS__NULL_T;
   mess_out.u.loc_msg_out.this_task_t_trans_num = MESSYS__NULL_T;
   mess_out.u.loc_msg_out.gsoc_flag = OBEY;
   strcpy ( mess_out.u.loc_msg_out.gsoc_name, name );
   if ( length > MSG_VAL_LEN )
   {
      length = MSG_VAL_LEN;
   }
   mess_out.u.loc_msg_out.gsoc_len             = length;
   mess_out.u.loc_msg_out.gsoc_status             = SAI__OK;
   memcpy ( mess_out.u.loc_msg_out.gsoc_value, value, length );
   msp_send_message ( (char *) &mess_out,
     C_LOC_MSG_OUT_LEN - MSG_VAL_LEN + length,
     targetq, command_q, status );
}



static void ams_settimeout
(
int time,        /* timeout in milliseconds (given) */
int *status      /* global status (given and returned) */
)
/*
*+
*  Name:
*     AMS_SETTIMEOUT

*  Language:
*     Starlink C

*  Algorithm:
*     Sets up a timeout event to occur in 'time' millseconds

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   atimer_settimr ( time, MESSYS__TIMEOUTID, ams_timeout, status );
}




static void ams_timeout
(
int parm                          /* the timeout parameter (given) */
)

/*
*+
*  Name:
*     AMS_TIMEOUT

*  Purpose:
*     Cause AMS_RECEIVE or GETREPLY to timeout

*  Language:
*     Starlink C

*  Algorithm:
*     This routine is assumed to be called from the timer's signal handler.
*     It writes an empty message onto this task's timeout queue, thereby
*     causing AMS_RECEIVE or AMS_GETREPLY to detect the condition.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     10-MAR-1994 (BDK):
*        Original
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   struct a_mess_out mess_out;    /* the message sent */
   int numbytes;                  /* number of bytes in message */
   int istat;                     /* local istat */
   receiveq_type mcq;             /* reply queue */

   mcq = MSP__NULL_RECEIVEQ;

/*   Build a timeout message */

   memset( &mess_out, 0, sizeof(mess_out));
   mess_out.mess_out_type = C_LOC_MSG_OUT;
   mess_out.u.loc_msg_out.other_task_t_trans_num = MESSYS__NULL_T;
   mess_out.u.loc_msg_out.this_task_t_trans_num = MESSYS__NULL_T;
   mess_out.u.loc_msg_out.gsoc_flag = OBEY;
   mess_out.u.loc_msg_out.gsoc_name[0] = '\0';
   mess_out.u.loc_msg_out.gsoc_len = 1;
   mess_out.u.loc_msg_out.gsoc_status = SAI__OK;
   numbytes = C_LOC_MSG_OUT_LEN - MSG_VAL_LEN;
   mess_out.u.loc_msg_out.gsoc_value[0] = '\0';

/*   and send it to the timeout queue */

   istat = SAI__OK;
   msp_send_message ( (char *) &mess_out, numbytes, sigtimeout_q, mcq,
     &istat );

}



static void ams_translate
(
const struct a_loc_msg_in *loc_msg_in,  /* the message in internal format
                                           (given) */
sendq_type reply_q,               /* reply queue (given) */
int *path,                        /* the communications path to the other
                                     task (returned) */
int *messid,                      /* the message identifier for this
                                     transaction (returned) */
int message_name_s,               /* space for name (given) */
int message_value_s,              /* space for value (given) */
int *message_status,              /* message status (returned) */
int *message_context,             /* message context (returned) */
char *message_name,               /* message name (returned) */
int *message_length,              /* length of value (returned) */
char *message_value,              /* message value (returned) */
int *status                       /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_TRANSLATE

*  Language:
*     Starlink C

*  Algorithm:
*     The system has received a local message as part of the transaction
*     'loc_msg_in->this_task_t_trans_num' (set into *messid) on path
*     t_trans[*messid].t_path_num (set into *path).
*
*     This routine updates the transaction entry for this end (in
*     particular adding the other task's acknowledge queue and
*     transaction end index if the existing transaction end this end is
*     missing this information) and then uses ams_unpacklocmsg() to
*     unpack the message.
*
*     If the path associated with the transaction has been closed or
*     changed this routine sets *status to MESSYS__BADPATH but still
*     unpacks the message.

*  Copyright:
*     Copyright (C) 1992, 1994 Science & Engineering Research Council.
*      All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     15-JUN-1992 (IRJ):
*        Created
*     18-JUN-1992 (IRJ,SKR):
*        Tidied
*     11-APR-1994 (BDK):
*        Return message components as separate arguments
*     12-APR-1994 (BDK):
*        Make function static
*     17-AUG-1994 (BKM):
*        Use local status on call to ams_unpacklocmsg
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int locmessid;         /* incoming transaction number */
   int istat;             /* local status */

   if (*status != SAI__OK) return;


   *path = MESSYS__NULL_P;
   *messid = MESSYS__NULL_T;
   locmessid = loc_msg_in->this_task_t_trans_num;
   AMS_checktrans ( locmessid, status );
   if ( *status != MESSYS__BADMESS )
   {
      *messid = locmessid;
   }
   if ( *status == SAI__OK )
   {
      if ( (*path = t_trans[*messid].t_path_num) != MESSYS__NULL_P )
      {
         t_trans[*messid].other_task_ack_q =
           reply_q;
         t_trans[*messid].other_transnum =
           loc_msg_in->other_task_t_trans_num;
      }
      else
      {
          *status = MESSYS__BADPATH;
      }
   }
   istat = SAI__OK;
   ams_unpacklocmsg ( loc_msg_in, message_name_s, message_value_s,
     message_status, message_context, message_name,
     message_length, message_value, &istat );
}


static void ams_unpacklocgsoc
(
const struct a_loc_gsoc_start_in *localmess, /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_UNPACKLOCGSOC

*  Language:
*     Starlink C

*  Algorithm:
*     Unpack a local gsoc start message.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994: return values as separate arguments, trap for
*                 overfilling buffers (BDK)
*     12-APR-1994 (BDK):
*        Make function static
*     23-JUN-1994 (BDK):
*        Fix one-off in trapping message_length
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   int nlen;        /* length of name */

   if ( *status != SAI__OK ) return;

   *message_status = SAI__OK;
   *message_context = localmess->gsoc_flag;
   nlen = strlen ( localmess->gsoc_name );
   if ( nlen < message_name_s )
   {

      strcpy ( message_name, localmess->gsoc_name );
      *message_length = localmess->gsoc_len;
      if ( *message_length <= message_value_s )
      {
         memcpy ( message_value, localmess->gsoc_value, localmess->gsoc_len );
      }
      else
      {
         memcpy ( message_value, localmess->gsoc_value, message_value_s );
         *status = MESSYS__BUFOV;
      }
   }
   else
   {
      *status = MESSYS__BUFOV;
   }
}



static void ams_unpacklocmsg
(
const struct a_loc_msg_in *localmess,  /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_UNPACKLOCMSG

*  Language:
*     Starlink C

*  Algorithm:
*     Unpack a local message.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994: return values as separate arguments, trap for
*                 overfilling buffers (BDK)
*     12-APR-1994 (BDK):
*        Make function static
*     23-JUN-1994 (BDK):
*        Fix one-off in trapping message_length
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
{
   int nlen;        /* length of name */

   if ( *status != SAI__OK ) return;

   *message_status = localmess->gsoc_status;
   *message_context = localmess->gsoc_flag;
   nlen = strlen ( localmess->gsoc_name );
   if ( nlen < message_name_s )
   {

      strcpy ( message_name, localmess->gsoc_name );
      *message_length = localmess->gsoc_len;
      if ( *message_length <= message_value_s )
      {
         memcpy ( message_value, localmess->gsoc_value, localmess->gsoc_len );
      }
      else
      {
         memcpy ( message_value, localmess->gsoc_value, message_value_s );
         *status = MESSYS__BUFOV;
      }
   }
   else
   {
      *status = MESSYS__BUFOV;
   }
}


static void ams_unpackremgsoc
(
const struct a_rem_gsoc_start_in *remotemess, /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_UNPACKREMGSOC

*  Language:
*     Starlink C

*  Algorithm:
*     Unpack a remote gsoc start message.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994: return values as separate arguments, trap for
*                 overfilling buffers (BDK)
*     12-APR-1994 (BDK):
*        Make function static
*     23-JUN-1994 (BDK):
*        Fix one-off in trapping message_length
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int nlen;        /* length of name */


   if ( *status != SAI__OK ) return;

   *message_status = SAI__OK;
   *message_context = remotemess->gsoc_flag;
   nlen = strlen ( remotemess->gsoc_name );
   if ( nlen < message_name_s )
   {

      strcpy ( message_name, remotemess->gsoc_name );
      *message_length = remotemess->gsoc_len;
      if ( *message_length <= message_value_s )
      {
         memcpy ( message_value, remotemess->gsoc_value,
           remotemess->gsoc_len );
      }
      else
      {
         memcpy ( message_value, remotemess->gsoc_value, message_value_s );
         *status = MESSYS__BUFOV;
      }
   }
   else
   {
      *status = MESSYS__BUFOV;
   }
}


static void ams_unpackremmsg
(
const struct a_rem_msg_in *remotemess, /* internal format (given) */
int message_name_s,                    /* space for name (given) */
int message_value_s,                   /* space for value (given) */
int *message_status,                   /* message status (returned) */
int *message_context,                  /* message context (returned) */
char *message_name,                    /* message name (returned) */
int *message_length,                   /* length of value (returned) */
char *message_value,                   /* message value (returned) */
int *status                            /* global status (given and returned) */
)

/*
*+
*  Name:
*     AMS_UNPACKREMMSG

*  Language:
*     Starlink C

*  Algorithm:
*     Unpack a remote message.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.   All
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     {original_author_entry}

*  History:
*     11-APR-1994: return values as separate arguments, trap for
*                 overfilling buffers (BDK)
*     12-APR-1994 (BDK):
*        Make function static
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

{
   int nlen;        /* length of name */


   if ( *status != SAI__OK ) return;

   *message_status = remotemess->gsoc_status;
   *message_context = remotemess->gsoc_flag;

   nlen = strlen ( remotemess->gsoc_name );
   if ( nlen < message_name_s )
   {

      strcpy ( message_name, remotemess->gsoc_name );
      *message_length = remotemess->gsoc_len;
      if ( *message_length <= message_value_s )
      {
         memcpy ( message_value, remotemess->gsoc_value,
           remotemess->gsoc_len );
      }
      else
      {
         memcpy ( message_value, remotemess->gsoc_value, message_value_s );
         *status = MESSYS__BUFOV;
      }
   }
   else
   {
   printf ( "%s - message name too large\n", taskname );
      *status = MESSYS__BUFOV;
   }
}
