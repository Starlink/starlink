/*
*+
*  Name:
*     ANT_CMN

*  Purpose:
*     Global variables for ANT library

*  Language:
*     {routine_language}

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     24-FEB-1988 (REVAD::BDK):
*        Original
*     25-MAR-1988 (REVAD::BDK):
*        Add LOCAL_TASK_ACCEPT_Q
*     25-MAR-1988 (REVAD::BDK):
*        Add COMMAND_Q
*     07-APR-1988: insert common blocks for structures and add
*                  THIS_MACHINE (REVAD::BDK)
*     07-APR-1988 (REVAD::BDK):
*        Rationalise N_MACH structure
*     18-APR-1994 (REVAD::BDK):
*        TCP/IP version
*     26-APR-1994 (REVAD::BDK):
*        C-unix version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

struct a_ntrans {       /* holds details of a net transaction */

   int local_nettask_n_path_num;   /* index to N_PATHS structure */
   sendq_type local_task_ack_q;    /* acknowledgement queue */
   int local_task_t_num;           /* index for local task */
   int remote_nettask_n_trans_num; /* index for remote ANT */
   int trans_state;                /* ANT__NULL_T => unused
                                      ANT__THIS_T => partly inserted from
                                      this end
                                      ANT__OTHER_T => partly inserted from
                                      other end
                                      ANT__FULL_T => fully inserted */
};

/*   The currently active transactions are held in an array of A_NTRANS
     structures, indexed by LOCAL_NETTASK_N_TRANS_NUM */

static struct a_ntrans n_trans[MESSYS__MXTRANS];


struct a_npath {       /* holds details of a network path */

   char local_taskname[MESSYS__TNAME]; /* taskname */
   char remote_taskname[MESSYS__TNAME]; /* taskname */
   sendq_type local_task_q;            /* command queue for local task */
   int local_task_t_path_num;          /* path index in local task */
   int remote_nettask_n_path_num;      /* path index in remote nettask */
   int local_machine_num;              /* index to list of machines in
                                          this task */
   int path_state;                     /* flag for partially-inserted path */
   sendq_type local_task_reminit_ack_q;/* local task's queue for receiving
                                          ACK_INIT */

};

/*   The currently active paths are held in an array of A_NPATH
     structures, indexed by LOCAL_NETTASK_N_PATH_NUM */

static struct a_npath n_paths[MESSYS__MXPATH];


struct a_nmach {    /* machine details */

char machine_names[MESSYS__MNAME]; /* machine names */
int local_channel;                 /* network channel */
int remote_machine_num;            /* machine number in remote nettask */

int mach_state;                /* ANT__NULL_MACH => unused entry
                                  ANT__THIS_START => partial connection,
                                  started from this end
                                  ANT__THIS_INIT => completed connection,
                                  started from this end
                                  ANT__OTHER_INIT => completed
                                  connection, started from other end */

sendq_type local_task_accept_q;    /* local task's queue for receiving
                                      REM_ACCEPT */

};

/*   The names of machines and network channel numbers which are being
     accessed are held in an array of A_NMACH structures indexed by
     N_PATHS(N).LOCAL_MACHINE_NUM. */

static struct a_nmach n_mach[MESSYS__MXMACH];



/*   Local machine name */

static char this_machine[MESSYS__MNAME];



/*   The following are the variables required for accessing TCP/IP */

static int listen_channel;        /* socket for incoming calls */

/*   Queues for communications from signal handler */

static sendq_type networks_q;     /* send queue used by signal handler */
static receiveq_type networkr_q;  /* receive queue used by main-line code */


/*   Queue for messages from tasks on this machine */

static receiveq_type command_q;


/*   buffer for network i/o */

static char netbuffer[C_NET_MAXMSG_LEN];

/*   pointers into network buffer
      _r refers to receiving end
      _s refers to sending end */

/*   INIT messages */

static char *i_stask;
static char *i_rtask;
static char *i_smach;
static char *i_rmach;
static char *i_snum;
static char *i_rnum;
static char *i_spath;

/*   GSOC messages */

static char *g_strans;
static char *g_rpath;
static char *g_flag;
static char *g_name;
static char *g_len;
static char *g_value;

/*   MSG messages */

static char *m_strans;
static char *m_rtrans;
static char *m_flag;
static char *m_name;
static char *m_len;
static char *m_status;
static char *m_value;

/*   DEINIT messages */

static char *d_rpath;

/*   ACK messages */

static char *k_rpath;
static char *k_spath;

/*   CALL messages */

static char *c_rmach;
static char *c_smach;
static char *c_snum;

/*   ACCEPT messages */

static char *a_rmach;
static char *a_smach;
static char *a_rnum;
static char *a_snum;
