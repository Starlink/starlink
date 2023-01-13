/******************************************************************************
 *                        A D A M . C                                         *
 ******************************************************************************
 *
 * This file contains implementations of most of the adam-related procedures
 * which are part of ICL.
 *
 * History:
 *   13-OCT-1993 (AJC):
 *      Added GETPAR and SETPAR
 *            GETGLOBAL, SETGLOBAL and CREATEGLOBAL
 *   22-NOV-1993 (BKM):
 *	Restructured include file layout
 *   02-DEC-1993 (BKM):
 *      Moved GETPAR and SETPAR
 *            GETGLOBAL, SETGLOBAL and CREATEGLOBAL to new module hds.c
 *   10-DEC-1993 (AJC):
 *      Remove UFACE routines to uface.c
 *    7-JAN-1993 (AJC):
 *      Act on `messages' flag when loading tasks
 *   13-JAN-1994 (AJC):
 *      Removed concat_args to interp.c
 *   13-OCT-1994 (AJC):
 *      Mods required on Solaris
 *       Initialize message_name_s, message_value_s in adam_send (required
 *         anyway?).
 *       Cast strlen
 *       Strtrim from rvalue_len not MSG_VAL_LEN in proc_send GET
 *   19-OCT-1994 (AJC):
 *      Comment out setup_task_hds from OBEYW command
 *    5-JUN-1996 (AJC):
 *      Consistent TOOFEWPARS and TOOMANYPARS messages
 *   28-NOV-1996 (AJC):
 *      Use restore_adamstring in proc_send and proc_obeyw
 *    4-FEB-2000 (AJC):
 *      Correct error message if SEND action not GET/SET/OBEY/CANCEL(/CONTROL)
 *      Display return from SEND CONTROL DEFAULT ""
 *    1-JUL-2004 (TIMJ)
 *      Add autoconf test for strsignal
 *
 ******************************************************************************
 */

#if HAVE_CONFIG_H
# include <config.h>
#endif

/* strsignal only appears if we turn on _GNU_SOURCE */
#if HAVE_STRSIGNAL
# ifndef _GNU_SOURCE
#  define _GNU_SOURCE
# endif
#endif

#include <stdio.h>
#include <signal.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <string.h>
#include "icl.h"
#include "expr.h"
#include "interp.h"
#include "symtab.h"
#include "dtask_err.h"
#include "sae_par.h"
#include "messys_len.h"
#include "messys_par.h"
#include "messys_err.h"
#include "adam_defns.h"
#include "ams.h"
#include "nbs_par.h"
#include "nbs_typ.h"
#include "nbs.h"
#include "iclhds.h"
#include "procs.h"
#include "adam.h"
#include "uface_functions.h"

/* Getting the text name of signals should be easier than this! */

#if !HAVE_STRSIGNAL
#  if( defined(SOL_SIGLIST) )	/* Solaris */
#    define sys_siglist _sys_siglistp
#  elif !defined(__linux)
    extern char *sys_siglist[];
#  endif
#endif

static value define_interpret ( node * n, int op);

extern void flshbuf(void);					 /* output.c */
extern void bufnewline(void);					 /* output.c */
extern void bufstring(char *mess);				 /* output.c */
extern void outstring(char *mess);				 /* output.c */

extern node *node_value(value v);				 /* node.c   */
extern node *node_builtin(value (*fn)() );			 /* node.c   */
extern node *node1(value (*interpreter)(), value val, node *n0); /* node.c   */

/******************************************************************************
 *
 * The following data structure, defined in adam.h, is used to record
 * information about tasks associated with LOADW, LOADD etc commands in ICL
 * This structure is specific to ICL's ADAM functions
 *
 * struct task
 * {
 *   struct task *next, *prev;	Doubley linked list, where an empty
 * 				list has an empty entry pointing to itself
 *   char taskname [MESSYS__TNAME]; The task's task name
 *   char *filename;    The Unix file name used to load the task
 *   int unix_pid;	The task's Unix pid
 *  };
 *
 ******************************************************************************
 */

#if HAVE_PUTENV && !HAVE_SETENV
static char env[22];		/* environment variable - see init_adam() */
#endif
static char buf[255];		/* General purpose buffer */

/* Circular buffer recording details of ADAM tasks known to ICL */
static struct task tasks = {&tasks, &tasks};
static struct task *task_list = &tasks;

#define TASKNULL ((struct task *)0)
#define TASKPTRNULL ((struct task **)0)
#define PIDNULL (-1)

/* Linked list recording details of ADAM NBS (Notice Boards) accessed by ICL */
static struct nbslist {
    struct nbslist *next;
    char name[17];
    int id;
    } *nbs_list = NULL;

/******************************************************************************
 *
 *	I N T _ C O N T E X T
 *
 * int_context(s) tests the string 's' for one of the possible GET, SET, OBEY,
 * CANCEL or CONTROL context and returns the appropriate integer internal code
 * for this context.
 *
 * Otherwise returns zero
 *
 ******************************************************************************
 */
static int
int_context
(
char *s			/* string pointing to context type name (given) */
)
{
    if (strcmp(s, "GET") == 0)
	return GET;
    if (strcmp(s, "SET") == 0)
	return SET;
    if (strcmp(s, "OBEY") == 0)
	return OBEY;
    if (strcmp(s, "CANCEL") == 0)
	return CANCEL;
    if (strcmp(s, "CONTROL") == 0)
	return CONTROL;
    return 0;
}

/******************************************************************************
 *
 *	T A S K _ C R E A T E
 *
 * Creates a new (empty) task entry linked into the internal task tables and
 * returns either a pointer to this structure or TASKNULL if memory allocation
 * fails
 *
 ******************************************************************************
 */
static struct task *
task_create
(
void
)
{
    struct task *task = (struct task *) malloc(sizeof(struct task));

    if (task == TASKNULL)
	return task;
    task->next = task->prev = task;
    task->unix_pid = PIDNULL;
    task->detached = -1;
    task->next = task_list->next;		/* link into task_list */
    task->prev = task_list;
    task_list->next->prev = task;
    task_list->next = task;
    return task;
}

/******************************************************************************
 *
 *	T A S K _ E X I T H A N D L E R
 *
 * This function is normally declared be the signal handler for SIGCHLD
 * signals (except when we wish to handle these differently).
 *
 * We determine the process id of the terminated task and, if an ADAM task, in
 * the internal tables we delete it from there. An ADAM task terminated by
 * a SIGHUP (or the depricated SIGTERM) signal is the normal way ICL itself
 * terminates tasks and this case is processed silently.
 *
 * NB The DTASK library converts a SIGHUP (or SIGTERM) signal sent to a task
 * into an exit(0) and ensures exit handlers are called.
 * Other types of unexpected termination are reported to the user.
 *
 * The io_subsystem process failing by any means is a serious error and is
 * reported directly to the user via stderr.
 *
 ******************************************************************************
 */
void
task_exithandler
(
int signo		/* system supplied integer signal number (given) */
)
{
    int pid, status;
    extern int iopid;						/* main.c */
    char text[80], text1[80];
    struct task *task;

/* Get the exit status of the terminated child process */
    if ( (pid = wait(&status)) < 0) {
	perror("task_exithandler - wait");
	return;
    }

/* Check if this is a known ADAM task, the io_subsystem or other child process
 * such as a shell
 */
    if (pid == iopid)
	sprintf(text, "\nio_subsystem task (pid = %d) terminated\n", iopid);
    else {
	for (task = task_list->next; task != task_list; task = task->next)
	    if (task->unix_pid == pid) {
		sprintf(text, "\nADAM task %s (pid = %d) ", task->taskname,
							    pid);
		task->next->prev = task->prev;
		task->prev->next = task->next;
		free((char *) task);
		break;
	    }
        if (task == task_list)
	    sprintf(text, "Unknown ICL child process (pid = %d) ", pid);
    }

    if (WIFSIGNALED(status))
	sprintf(text1, "abnormal termination -\nsignal %s\n",
#if HAVE_STRSIGNAL
		strsignal(WTERMSIG(status))
#else
		sys_siglist[WTERMSIG(status)]
#endif
		);
    else if (WIFEXITED(status))
	if ((WEXITSTATUS(status) == 0 || WEXITSTATUS(status) == SIGTERM) &&
	    task != task_list)  /* Expected task termination */
	    return;
        else
	    sprintf(text1, "unexpected exit -\nstatus code %d\n",
		    WEXITSTATUS(status) );

    if (pid == iopid) {
	fprintf(stderr, "%s%s", text, text1);
	_exit(1);
    } else {
	outstring(text);
	outstring(text1);
    }
}

/******************************************************************************
 *
 *	T A S K _ D E S T R O Y
 *
 * Unlinks and frees the memory associated with the given task table entry.
 *
 ******************************************************************************
 */
static void
task_destroy
(
struct task * task
)
{
    task->next->prev = task->prev;
    task->prev->next = task->next;
    free((char *) task);
    return;
}

/******************************************************************************
 *
 *	T A S K _ F R O M _ N A M E
 *
 * Returns a pointer to the task structure for the named task OR NULL if
 * the task is not found.
 *
 * 'name' can be supplied in one of two forms depending on 'istask':-
 *
 *	1) The task name as used by the message system
 *	2) The file name used to load the process
 *
 ******************************************************************************
 */
static struct task *
task_from_name
(
char *name,	/* name to search the internal tables for (given) */
int  istask	/* is name a taskname (boolean) (given) */
)
{
    struct task *p;
    char buff[MSG_NAME_LEN+10];
    int i, path, status = SAI__OK;

    for (p = task_list->next; p != task_list; p = p->next)
	if (istask) {
	    if (strcasecmp(p->taskname, name) == 0)
		break;
	} else
	    if (strcasecmp(p->filename, name) == 0)
		break;

/* Task may already be known to this ICL or started by a previous ICL or
 * another user interface */
    if (p != task_list) {
	ams_path(p->taskname, &path, &status);
	if( status == SAI__OK)
	    return p;
	else {
	    sprintf(buf, "ICL task %s not in message system - deleted\n",
		    p->taskname);
	    outstring(buf);
	    task_destroy(p);/* started task must have died or something
			     * as no path available */
	}
    } else if (istask) {
        ams_path(name, &path, &status);
        if( status == SAI__OK) {
	    p = task_create();
	    strcpy(p->taskname, name);
            p->filename = CHARNIL;
	    p->detached = TRUE;
	    p->unix_pid = 0;
	    return p;
	}
    }
    return TASKNULL;
}

/******************************************************************************
 *
 *	A D A M _ S T O P
 *
 * Called as part of ICL rundown to kill all attached tasks.
 *
 ******************************************************************************
 */
void
adam_stop
(
void
)
{
    struct task *p;
    struct utsname *info=NULL;
    char *s, *file=NULL, *host;
    FILE *fd;
/*
 * Kill attached tasks - write the PIDs of any detached ones into a file
 * so we can find them again.
 */
    uname(info);	/* Get system information structure */

    for (p = task_list->next; p != task_list; p = p->next) {
	if (p->unix_pid)
	    if (!p->detached) {
		kill(p->unix_pid, SIGHUP);
		kill(p->unix_pid, SIGTERM);
	    } else {
		if (file == NULL) {
		    if ( (s = getenv("ADAM_USER")) == NULL)
			s = strconcat( getenv("HOME"), "/adam");
		    file = strconcat(s, "/.iclpids");
		    fd = fopen(file, "w");
		}
		fprintf(fd, "%s - %d\n", info->nodename, p->unix_pid);
	    }
    }
    if (file != NULL)
	fclose(fd);

    return;
}

/******************************************************************************
 *
 *	A D A M _ P A T H
 *
 * Returns the message system path assocated with a given task.  Does not
 * concern itself if the path was already open and so, if ams_path()
 * returns MESSYS__PATHOPEN this routine returns SAI__OK.
 *
 ******************************************************************************
 */
static void
adam_path
(
char *taskname,		/* ADAM task name (given) */
int *path,		/* message system path index to task (returned) */
int *status		/* global status (given and returned) */
)
{
    if (*status != SAI__OK)
	return;
    ams_path(taskname, path, status);
    if (*status == MESSYS__PATHOPEN)
	*status = SAI__OK;
    return;
}

/******************************************************************************
 *
 *	A D A M _ S E N D
 *
 * Sends a message to start a transaction on a given path and returns
 * the 'value' field of the reply in and the newly established transaction
 * identifier to allow the caller to continue with the transacton.
 *
 * This routine will only return when a reply is received from this transaction
 * (ie other mesages will not be confused with the expected reply)
 *
 ******************************************************************************
 */
static void adam_send
(
int path,		/* path number (given) */
int message_context,	/* message context (given) */
char *message_name,	/* message name (given) */
char *message_sval,	/* message value to send (given) */
int message_slen,	/* message send value length (given) */
char *message_rval,	/* message value from reply (returned) */
int *message_rlen,	/* message reply value length (returned) */
int *messid,		/* message number (returned) */
int *status		/* global status (given and returned) */
)
{
    int message_name_s, message_value_s; /* space for name and value (given) */
    int message_status;			 /* message status (returned) */

    message_name_s = sizeof(message_name);
    message_value_s = sizeof(message_rval);
    if (*status != SAI__OK)
	return;
    ams_send(path, MESSYS__MESSAGE, SAI__OK, message_context, message_name,
	     message_slen, message_sval, messid, status);
    ams_getreply(MESSYS__INFINITE, path, *messid, message_name_s,
		 message_value_s, &message_status, &message_context,
		 message_name, message_rlen, message_rval, status);
    if (*status == SAI__OK)
	*status = message_status;
    return;
}

/******************************************************************************
 *
 *	A D A M _ S E N D T
 *
 * This routine functions like adam_send() (above) except that a timeout (in
 * seconds) can be specified for the reply.
 *
 * Note that (after a timeout) a reply may subsequently arrive and will queue
 * until a message system call which accepts messages from all transactions
 * (either adam_receive() or adam_getreply() ) is called.
 *
 ******************************************************************************
 */
static void adam_sendt
(
int timeout,		/* reply timeout in seconds (given) */
int path,		/* path number (given) */
int message_context,	/* message context (given) */
char *message_name,	/* message name (given) */
char *message_sval,	/* message value to send (given) */
int message_slen,	/* message send value length (given) */
char *message_rval,	/* message value from reply (returned) */
int *message_rlen,	/* message reply value length (returned) */
int *messid,		/* message number (returned) */
int *status		/* global status (given and returned) */
)
{
    int message_status;			 /* message status (returned) */

    if (*status != SAI__OK)
	return;
    ams_send(path, MESSYS__MESSAGE, SAI__OK, message_context, message_name,
	     message_slen, message_sval, messid, status);
    ams_getreply(timeout, path, *messid, MSG_NAME_LEN, MSG_VAL_LEN,
		 &message_status, &message_context, message_name,
		 message_rlen, message_rval, status);
    if (*status == SAI__OK)
	*status = message_status;
    return;
}

/******************************************************************************
 *
 *	A D A M _ C O N T R O L
 *
 * Sends a task CONTROL context message to a task ot tasks. At present this
 * new facility is only used to send "DEFAULT" messages to ADAM tasks to
 * allow them to change their home directory.
 *
 * 'name' can be a valid taskname or 'CACHED' which will cause the message
 * to be sent to all attached tasks
 *
 ******************************************************************************
 */
value
adam_control
(
char *name,			/* task name or "CACHED" (given) */
char *action,			/* action for control context (given) */
char *message			/* message to send (given) */
)
{
    struct task *p=TASKNULL;
    char taskname[MESSYS__TNAME],
	 messname[MSG_NAME_LEN],
	 rvalue[MSG_VAL_LEN] = " ";
    node *lookup_node;
    char *vp;
    int status, path, messid, message_status, message_context, rvalue_len;

/* Check for the only valid action at present */
    if (strcmp(action, "DEFAULT") != 0)
	return exception("ADAMERR - Invalid CONTROL context action");
    else
	strcpy(messname, action);

    taskname[0] = '\0';
    if (strcmp(name, "CACHED") == 0) {
	for (p=task_list->next; p != task_list; p = p->next)
	    if ( !p->detached ) {
		strcpy(taskname, p->taskname);
		break;
	    }
    } else
	if (task_from_name(name, TRUE) == TASKNULL)
/*
 * Task is not loaded. However user is also allowed to refer to a task via
 * an ICL DEFINEd command name if present.
 */
	    if ((lookup_node = lookup_symbol(name, SYM_DEFINED)) != NULL &&
		 lookup_node->interpret == define_interpret)
	    {
		vp = string_part(lookup_node->val);
		for (p = task_list->next; p != task_list; p = p->next)
		    if( strcmp(vp, p->filename) == 0)
		    {
			strcpy(taskname, p->taskname);
			p = TASKNULL;
			break;
		     }
	    } else
		return exception1("ADAMERR - task \"%s\" not found", name);
	else
	    strcpy(taskname, name);
/*
 * Send the control command to the task(s)
 */
     while (taskname[0] != '\0') {
	status = SAI__OK;
	adam_path(taskname, &path, &status);
	adam_sendt(20000, path, CONTROL, messname, message,
		   strlen(message), rvalue, &rvalue_len, &messid, &status);
	message_status = status;
	status = SAI__OK;
	while (status == SAI__OK) {
	    if (message_status == DTASK__ACTSTART ||
		message_status == SAI__OK)
		break;
	    else if (message_status == MESSYS__INFORM)
		uface_inform(path, rvalue, rvalue_len, &status);
	    else
		status = message_status;
	    ams_getreply(MESSYS__INFINITE,path, messid, MSG_NAME_LEN,
			MSG_VAL_LEN, &message_status, &message_context,
			messname, &rvalue_len, rvalue, &status);
	}
	if ( status == SAI__OK ) {
	    bufstring("Default for task ");
	    bufstring(taskname);
	    bufstring(" - ");
	    bufstring(rvalue);
	    bufnewline();
	} else {
	    bufstring("Error setting default directory for task \"");
	    bufstring(taskname);
            bufstring("\"");
	    bufnewline();
	    bufstring("Task return status - ");
	    bufstring(string_part(adam_exception("",status)));
	    bufnewline();
	}
        taskname[0] = '\0';
	if (p != TASKNULL && p != task_list)
	    for (p = p->next; p != task_list; p = p->next)
		if ( !p->detached ) {
		    strcpy(taskname, p->taskname);
		    break;
		}
    }
    flshbuf();

    return trueval;
}

/******************************************************************************
 *
 * 	A D A M _ W A I T
 *
 * Return after a specified millisecond wait ( without causing a SIG_ALARM)
 *
 * Note: Any other signal will interrupt the select system call but this is
 * desirable behaviour in this case.
 *
 ******************************************************************************
 */
static void adam_wait(int millisecs, int *status)
{
    struct timeval waittime;

    if( *status != SAI__OK )
	return;

    waittime.tv_sec = ((long) millisecs) / 1000;
	/* Convert milliseconds argument -> microseconds */
    waittime.tv_usec = (((long) millisecs) % 1000) * 1000;
    if (select(0, NULL, NULL, NULL, &waittime) != 0)
	if (errno != EINTR) perror("adam_wait() - select");
    return;
}

/******************************************************************************
 *
 *	L O A D _ A N D _  R U N _ C O M M A N D
 *
 * This is a UNIX specific routine to start a new ADAM task.
 *
 * UNIX signals (such as CTRL-C (SIGINT)) are delivered to all processes
 * which are not ignoring SIGINT and which are in the same process group as
 * the controlling terminal. load_and_run_command() can thus 'detach' a
 * process from ICL by placing it in a new process group.
 *
 ******************************************************************************
 */
static int		/* pid of loaded process or -1 on error */
load_and_run_command
(
char *filename,		/* Unix filename to load (given) */
char *adam_taskname,	/* ADAM taskname and message system name (given) */
int detached		/* Flag to inidicate 'detached' status (given) */
)
{
    int pid, nfds, lcount, path, i, status;
    struct sigaction oact;

/* Output loading message depending upon 'messages' flag (altered by SET) */
    if (messages) {
       sprintf(buf,"Loading %s into %s ", filename, adam_taskname);
       bufstring(buf);
       bufstring(detached == 0 ? "(attached)" : "(detached)");
       bufnewline();
       flshbuf();
    }
/*
 * Set up an environment variable to be inherited by the child task giving
 * it the name it should register with for the ADAM message system
 */

#if HAVE_SETENV
    setenv("ICL_TASK_NAME", adam_taskname, 1);
#elif HAVE_PUTENV
    if (getenv("ICL_TASK_NAME") == CHARNIL) {
      /* Note that the string "env" becomes part of the environment
         directly so modifying it modifies the environment */
	putenv(env);
        sprintf(env, "ICL_TASK_NAME=%s", adam_taskname);
    }
#else
# error "Unable to set an environment variable"
#endif

/* Reset SIGCHLD handling to its default */
    sigaction(SIGCHLD, NULL, &oact);
    signal(SIGCHLD, SIG_DFL);
/*
 * Load task image file
 */
    switch (pid = fork()) {
      case 0:			/* now running as the child */
/*
 * Close file descriptors, reset exit handler and signals
 */
	if ( (nfds = sysconf(_SC_OPEN_MAX)) < 0 )
	    nfds = 256;
	for (i = 3; i < nfds; i++) {
	    close(i);
	    if (errno == EBADF)
		break;
	}
	if (detached)
	    (void) setsid();	/* Detach from controlling terminal */

    /* Child loads the ADAM task from filename using execl() */
	execl(filename, filename, (char *) 0);

    /* only executed if execl fails - NB MSP is unavailable in child process */
	status = errno;
	sleep(2);		/* allow any ICL messages to output */
	errno = status;		/* restore execl errno */
	perror("execl() failed to load file");
	_exit(errno);		/* Bypass exit handlers and exit immediately */
	break;

      case -1:			/* Fork failed */
	sprintf(buf,"fork() process failed: %s", strerror(errno));
        bufstring(buf);
	bufnewline();
	flshbuf();
	return -1;

      default:				/* Still the parent process */
	status = SAI__OK;
	adam_wait(1000, &status); 	/* give child process a chance
					 * to get started and registered
					 * with the message system */
	if (pid != -1) {
	    lcount = 30;		/* 15 seconds */
	    do {
		if (waitpid(pid, &status, WNOHANG)) {
		    bufstring("Child process ");
		    if( WIFSIGNALED(status) )
		      sprintf(buf, "died - signal %s\n",
#if HAVE_STRSIGNAL
			      strsignal(WTERMSIG(status))
#else
			      sys_siglist[WTERMSIG(status)]
#endif
				);
		    else
			sprintf(buf, "unexpected exit - status code %d\n",
				WEXITSTATUS(status));
		    bufstring(buf);
		    flshbuf();
		    lcount = 0;
		    break;
		}
		status = SAI__OK;
		ams_path(adam_taskname, &path, &status);
		if( status == SAI__OK)
		    break;
		else {
		    status = SAI__OK;
		    adam_wait(500, &status);
		}
	    }
	    while (--lcount);


	    if (lcount)
/* Reset SIGCHLD handling */
		sigaction(SIGCHLD, &oact, NULL);
	    else {
		if (status == 0) {
		    bufstring("TASKERR : Failed to contact task using");
		    bufstring(" message system\n");
		    flshbuf();
	            kill(pid, SIGKILL);
		    waitpid(pid, &status, 0);
		    sigaction(SIGCHLD, &oact, NULL);
		}
		return -1;
	    }
	}
    }
    return pid;
}

/******************************************************************************
 *
 *	T A S K _ F I N D _ O R _ L O A D
 *
 * This routine first checks the task structure to see if the given task has
 * already been loaded and if so returns a pointer to the associated data
 * structure.
 *
 * If it is not loaded and the global variable (see the SET command) 'autold'
 * is true, we call load_and_run_command() to fork and run the command
 * (possibly as a detached process dependent on the boolean 'detached').
 *
 * We then enter the taskname and process id in the internal task table.
 *
 * If the task is not loaded and autold is false, a message is output and
 * TASKNULL is returned.
 *
 * We then establish a path to the task using adam_path() and record the path
 * end index in the task table.
 *
 ******************************************************************************
 */
static struct task *
task_find_or_load(
char *comname,		/* File name to load (given) */
char *taskname, 	/* Task name (can be NULL) (given) */
int detached,		/* Should loaded task be detached - boolean (given) */
int autold		/* Automatic load of task - boolean (given) */
)
{
    extern int uface_interrupt(void);				/* uface.c */
    struct task *taskptr;
    char adam_taskname[MESSYS__TNAME];
    char *filename, *s;
    int status, path;
/*
 * The taskname can be explicitly specified or it can be derived from the
 * filename used to load the task.
 *
 * First check if the task is already loaded
 */
    if(taskname != NULL)
	taskptr = task_from_name(taskname, TRUE);
    else
	taskptr = task_from_name(comname,  FALSE);

    if (taskptr != TASKNULL)  /* Task already loaded */
	return taskptr;

    if (taskname == NULL) {
	if(!detached) 	/* Base task name on PID */
	    sprintf(buf, "%s%d", strip_path(comname),getpid());
	else
	    strcpy(buf, strip_path(comname));
    } else
	strcpy(buf, taskname);

    if( strlen(buf) >= (size_t) MESSYS__TNAME){
	bufstring("TASKERR Task name ");
	bufstring(buf);
	bufstring(" too long\n");
	flshbuf();
	return TASKNULL;
     } else
	strcpy(adam_taskname, buf);

    if (!expand_name(&filename, comname)) {
        bufstring("Unable to obtain filename from ");
        bufstring(comname);
	bufstring(" - ");
	bufstring(filename);
	flshbuf();
	bufnewline();

        return TASKNULL;
    }
/* Check if automatic loading required
 * The autold argument is set to 1 when this routine is called from the
 * explicit load commands, and to the value of the autoload flag when
 * called from other commands which require the task to be loaded. */
    if ( !autold ) {
 	bufstring("task_find_or_load: NOAUTOLOAD set, task ");
	bufstring(filename);
 	bufstring(" must be loaded explicitly\n");
	flshbuf();
	return TASKNULL;
    }

/* Load the task */
    if( access(filename, X_OK) != 0) {
	if( filename[0] == '/' ||
	    (s = which_exec(filename)) == CHARNIL )
	{
	    bufstring("task_find_or_load: No task image file ");
	    bufstring(filename);
	    bufnewline();
	    flshbuf();
	    return TASKNULL;
	} else
	    filename = s;
    }
    taskptr = task_create();
    strcpy(taskptr->taskname, adam_taskname);
    taskptr->filename = comname;
    taskptr->detached = detached;
    if ((taskptr->unix_pid = load_and_run_command(filename, adam_taskname,
						  detached)) <= 0) {
	task_destroy(taskptr);
	return TASKNULL;
    }
    status = SAI__OK;
    adam_path(adam_taskname, &path, &status);
    if (status == MESSYS__TIMEOUT)
    {
	bufstring("Timeout getting path to task ");
        bufstring(adam_taskname);
        bufnewline();
        flshbuf();
    }
    if (status != SAI__OK || uface_interrupt()) {
	task_destroy(taskptr);
	return TASKNULL;
    }
    return taskptr;
}

/******************************************************************************
 *
 *	A L O A D _ H E L P E R
 *
 * Common code called to complete the processing of the ICL LOADW, ALOAD and
 * LOADD commands.
 *
 * These ICL commands all have, as their first parameter, the name of the
 * executable task image file and an optional second parameter specifying
 * the name of the loaded task. As usual, if the second parameter is omitted,
 * the taskname is derived from the filename.
 *
 * On entry 'ptask' points to a task entry to be used to record details of
 * the loaded task (or NULL if task entry not required). 'detached' is a
 * boolean and determines if the task is to be detached from this process.
 *
 * The routine first interprets the parameters to the command line using
 * interpret_to_string() and then calls task_find_or_load() with the evaluated
 * command name and task name. An exception is returned if this function fails
 * to run and to establish a path with the new task.
 *
 * Prior to this routine being being called, the following global variables
 * will already have been initialised:
 *
 *   The parameter list of the ICL command line is in the 'arglist[]' array.
 *
 *   'nargs' is the number of command arguments.
 *
 ******************************************************************************
 */
static value
aload_helper
(
struct task **ptask,		/* pointer to the task table entry to contain
				 * details of the loaded task (returned) */
int detached			/* Boolean to indicated if 'detached' (given) */
)
{
    node *n1;
    struct task *task;
    char *command, *taskname;
    value val;

/* Perform basic argument checking and obtain a string from the first
 * ICL command parameter */
    if (nargs < 1)
	return exception("TOOFEWPARS  Insufficient parameters for ALOAD");
    else if (nargs > 2)
	return exception("TOOMANYPARS  Too many parameters for ALOAD");
    if( isexc(val = interpret_to_string(arglist[0])) )
	return val;
    command = string_part(val);

/* Check if 'command' has been DEFSTRINGed - mirrors VMS behaviour */
    if ( (n1 = lookup_symbol(command, SYM_DEFINED)) != NODENIL &&
	 n1->interpret == defstring_interpret)
    {
	command = string_part(n1->val);
    }

    if (nargs == 1)	/* Base the taskname on the final filename component */
	taskname = strip_path(command);
    else {		/* use the optional argument */
	if (isexc(val = interpret_to_string(arglist[1])))
	    return val;
	taskname = string_part(val);
    }
    if (strlen(taskname) >= (size_t)  MESSYS__TNAME)
	return exception("TASKERR task name too long");
    else if ((task= task_find_or_load(command, taskname, detached, 1))
		== TASKNULL)
	return exception("TASKERR failed to load task");
    else if (task->unix_pid == 0) {
	bufstring("Detached task \"");
	bufstring(taskname);
	bufstring("\" already loaded and known to message system");
	bufnewline();
	flshbuf();
    }

    if (ptask != TASKPTRNULL) /* Return task data structure */
	*ptask = task;

    return trueval;
}

/******************************************************************************
 *
 *	D E F T A S K _ I N T E R P R E T
 *
 * Interprets an ICL command
 *   command context [arguments]
 * where 'command' has previously been defined by a DEFTASK command
 *
 * Obeying a DEFTASKed command 'command' is equivalent to the ICL command
 * SEND taskname context [arguments] with the arguments of 'command'
 * forming the input value string sent to the task.
 *
 * The earlier proc_deftask() routine which processed the original DEFTASK
 * command will have checked for abbreviations of the form COMM(AND) and
 * will have set up 'PROC_HIDDEN' symbol table entries whose value components
 * point to a node structure pointing to this function and having a
 * value_string containing the taskname.
 *
 * Method:
 *
 * We first obtain the taskname from the value_string part of node 'n' and
 * either locate an existing task or set a new one running using
 * task_find_or_load().
 *
 * The first parameter is the task context and we check it is one of
 * GET/SET/OBEY/CANCEL/CONTROL.
 *
 * Then we obtain the parameter to be SET/GET or the action to be OBEYed or
 * CANCELed plus any further parameters.
 *
 * We then establish a path to the task use adam_sendt() to send the message
 * allowing a maximum delay of 20 seconds for a response. Depending on the
 * context we either display the parameter value (for a GET), return trueval
 * or raise an exception (if the final status is not SAI__OK)
 *
 * Bugs:
 *
 * Improve exception condition infromation messages.
 *
 ******************************************************************************
 */
static value
deftask_interpret
(
node * n,		/* ICL node structure (given) */
int op			/* op code for this function (given) */
)
{
    value val;
    char *taskname, *vp, actionorparametername[MSG_NAME_LEN],
	valuetobesent[MSG_VAL_LEN], valuereceived[MSG_VAL_LEN];
    int context, vpl, path, messid, status, vlen, vtbslen;
    struct task *task;

    switch (op) {
      case OP_INTERPRET:
	taskname = uppercase(string_part(n->val));
        if( strlen(taskname) >= (size_t) MESSYS__TNAME)
	    return exception("TASKERR  Task name too long");
	if (nargs == 0)
	    return (exception(
		  "DEFTASK/SEND First parameter must be a context string"));
	if (isexc(val = interpret(arglist[0])))
	    return val;
	if (!isstringtype(val))
	    return (exception(
		  "DEFTASK/SEND First parameter must be a context string"));
	if ((context = int_context(uppercase(string_part(val)))) == 0)
	    return (exception(
	   "DEFTASK/SEND First parameter must be GET/SET/OBEY/CANCEL/CONTROL"));
	if (nargs <= 1)
	    return exception(
	   "DEFTASK/SEND Second parameter must be parameter name or action");
	if (isexc(val = interpret(arglist[1])))
	    return exception(
	   "DEFTASK/SEND Second parameter must be parameter name or action");
	vp = uppercase(string_part(val));
	vpl = strlen(vp) + 1;
	memcpy(actionorparametername, vp,
	      (vpl > MSG_NAME_LEN ? MSG_NAME_LEN : vpl));
	if (context != GET) {
	    if (nargs <= 2)
		return exception(
        "DEFTASK/SEND Third parameter must be set value or action parameters");
	    if (isexc(val = concat_args(2)))
		return exception(
	"DEFTASK/SEND Third parameter must be set value or action parameters");
	    vp = string_part(val);
	    vpl = strlen(vp) + 1;
	    memcpy(valuetobesent, vp,
		  (vtbslen = (vpl > MSG_VAL_LEN ? MSG_VAL_LEN : vpl)));
	} else {
	    vtbslen = 1;
	    valuetobesent[0] = '\0';
	}
	if ((task = task_find_or_load(taskname, taskname, 0, autoload))
             == TASKNULL)
	    return exception("TASKERR  could not load task");
	status = SAI__OK;
	adam_path(taskname, &path, &status);
	if (status == SAI__OK) {
	    adam_sendt( 20000, path, context, actionorparametername,
			valuetobesent, vtbslen, valuereceived, &vlen,
			&messid, &status);
	    if (status == SAI__OK) {
		if (context == GET)
		    outstring(valuereceived);
	    } else if (status == DTASK__ACTCANCEL)
		status = SAI__OK;
	} else
	    valuereceived[0] = '\0';
	if (status != SAI__OK)
	    return exception(" DEFTASK/SEND Failure during sending of message");
			    /* Could qualify with valuereceived */
	break;

      case OP_PRINT:
	break;

      default:
	break;
    }
    return noval;
}

/******************************************************************************
 *
 *	P R O C _ D E F T A S K
 *
 * Interprets an ICL command
 *   DEFTASK command [taskname]
 *
 * The builtin ICL DEFTASK command defines a temporary ICL command 'command'
 * which will (using defitask_interpret()) issue a SEND to an ADAM task
 *
 * The first argument 'command' may specify allowable abbreviations if in the
 * form COM(MAND) where COM, COMM, COMMA, COMMAN and COMMAND would be
 * subsequently recognised.
 *
 * The second (optional) parameter 'taskname' is the name of the task to
 * receive the SEND and defaults to 'command' if absent.
 *
 * When 'command' is processed its own parameters must contain the context
 * (GET, SET, OBEY, CANCEL) and the parameters or action name to be sent to
 * the task.
 *
 * This routine creates a PROC_HIDDEN symbol table entry for COM(MAND)
 * whose value component points to a deftask_interpret() node with a
 * value_string containing the taskname.
 *
 * Thus when 'command' is subsequently issued as an ICL command
 * deftask_interpret() will be executed.
 *
 * Method:
 *
 * Prior to this routine being being called, the following global variables
 * will already have been initialised:
 *
 *   The parameter list of the ICL command line is in the 'arglist[]' array.
 *
 *   'nargs' is the number of command arguments.
 *
 * We obtain the string value of the first argument (returning an exception on
 * failure or not a string) and the optional 'taskname' argument. We call
 * remove_parens() to provide us with a parenthesis free copy of the command
 * name and then use install_abbrevs() to install this (and any abbreviations)
 * into the ICL symbol table.
 *
 * On execution, the PROC_HIDDEN symbol table entry for COM(MAND) will be
 * found and its value member will point to a deftask_interpret() node whose
 * string part member contains the taskname.
 *
 ******************************************************************************
 */
static value
proc_deftask
(
node * n		/* ICL internal 'node' structure (given) */
)
{
    value val;
    char *taskname, *command;
    node *nw1;

    if (nargs < 1)
	return exception("TOOFEWPARS  Insufficient parameters for DEFTASK");
    else if (nargs > 2)
	return exception("TOOMANYPARS  Too many parameters for DEFTASK");

    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return (exception("DEFTASK First parameter must be a command name"));

    command = string_part(val);
    if (nargs == 2) {
	if (isexc(val = interpret(arglist[1])))
	    return val;
	taskname = string_part(val);
    } else if ((taskname = remove_parens(command)) == CHARNIL)
	return exception(
	    "SYSERR  System Error : Memory exhausted in DEFTASK");
    if( strlen(taskname) >= (size_t) MESSYS__TNAME )
	return exception("TASKERR  Taskname too long");

    if ((nw1 = node_value(value_string(taskname))) == NODENIL)
	return exception(
	    "SYSERR  System Error : Memory exhausted in DEFTASK");

    return (install_abbrevs(command, node1(deftask_interpret, val, nw1)));
}

/******************************************************************************
 *
 *	D E F I N E _ I N T E R P R E T
 *
 * Interprets an ICL command
 *   command [arguments]
 * where 'command' has previously been defined by a DEFINE command
 *
 * This routine interprets a temporary ICL command 'command' and is equivalent
 * to "OBEYW taskname action ..." with the arguments of 'command' being
 * appended to the OBEYW command forming the value string sent to the task.
 *
 * The earlier proc_define() routine which processed the DEFINE command will
 * have checked for abbreviations of the form COMM(AND) and set up PROC_HIDDEN
 * symbol table entries whose value components point to a define_interpret()
 * node with a value_string containing the taskname.
 *
 * Method:
 *
 * Prior to this routine being being called, the following global variables
 * will already have been initialised:
 *
 *   The parameter list of the ICL command line is in the 'arglist[]' array.
 *
 *   'nargs' is the number of command arguments.
 *
 * On execution, the PROC_HIDDEN symbol table entry for COM(MAND) set up by
 * proc_define() will be found. This is a 'node' structure pointing to this
 * routine having a 'value' component whose string part contains the 'taskname'
 * and whose sub[0] member points to a node with a value holding the 'action'
 * name.
 *
 * We obtain the optional [arguments] part of the command line (the global
 * 'all_args' will be NULL if none specified) as a string to be passed as the
 * input value string of the OBEY message. uface_obeyw() sends the OBEY
 * message on the path to the task 'taskname' to invoke the action recorded
 * from the DEFINE command.
 *
 * Bugs:
 *
 ******************************************************************************
 */
static value
define_interpret
(
node * n, 		/* ICL internal 'node' structure (given) */
int op			/* op code for this routine (given) */
)
{
    extern int sigint_flag;					/* main.c */
    value val;
    int vpl, tlen, vallen, status, hdsvars = 0;
    struct task *task;
    char actname[MSG_NAME_LEN], svalue[MSG_VAL_LEN], *vp;

    switch (op) {

      case OP_INTERPRET:
	if ((task =
             task_find_or_load(string_part(n->val), CHARNIL, 0, autoload))
	     == TASKNULL)
	    return exception("TASKERR  could not load task");

	vp = string_part(n->sub[0]->val);	/* the action */
	vpl = strlen(vp) + 1;
	memcpy(actname, vp, (vpl > MSG_NAME_LEN ? MSG_NAME_LEN : vpl));
	if (all_args != NODENULL) {

/* Check for strings and ICL variables being passed to the task and adjust
 * the parse tree to cope with either case
 */
            setup_task_hds(all_args, &hdsvars);

/* Interpret the argument structure - gives a string to pass to the task */
	    if( isexc(val = interpret_to_string(all_args)) )
		return val;
/* Free HDS file so that changes will be flushed to disk*/
	    if (hdsvars)
		hds_flush();

	    vp = string_part(val);
	    vpl = strlen(vp) + 1;
	    memcpy(svalue, vp,
		  (vallen = (vpl > MSG_VAL_LEN ? MSG_VAL_LEN : vpl)));
	} else {
	    svalue[0] = '\0';
	    vallen = 1;
	}
        status = SAI__OK;
	uface_obeyw(task->taskname, actname, svalue, vallen, &status);
	if (status == MESSYS__EXTINT) {
	    sigint_flag = 0;
	    if (task->unix_pid > 0 &&
	        (kill(task->unix_pid, SIGHUP) == 0 ||
		 kill(task->unix_pid, SIGTERM) == 0) ) {
		    val = exception1("CNTLC - task %s killed during OBEYW",
					task->taskname);
		    return val;
	    } else
		return exception1(
			"CNTLC  Could not kill %s - not started by this ICL",
			task->taskname);
	} else if (status != SAI__OK) {
	    bufstring("OBEYW unexpected status returned from task \"");
	    bufstring(task->taskname);
	    bufstring("\", action - \"");
	    bufstring(actname);
	    bufstring("\"");
	    bufnewline();
	    flshbuf();
	    return adam_exception("ADAMERR",  status);
	} else {
	    if (hdsvars)
		/* Reload ICL variable from the HDS file */
		if( isexc(val = reload_vars_hds()) )
		    return val;
	    return noval;
        }
	break;

      case OP_PRINT:
	break;

      default:
	break;
    }
    return noval;
}

/******************************************************************************
 *
 *	P R O C _ D E F I N E
 *
 * Interprets the builtin ICL command
 *    DEFINE command taskname [action]
 *
 * which sets up a new temporary ICL command 'command' which, when issued, will
 * send an OBEYW to an ADAM task.
 *
 * 'command' is the new ICL command being defined, and may be an abbreviation
 * in the form COM(MAND) where COM, COMM, COMMA, COMMAN and COMMAND would be
 * acceptable.
 *
 * 'taskname' is the name of the task to receive the OBEYW.
 *
 * 'action' (optional) is the action to be executed and defaults to the
 * same as 'command' if omitted.
 *
 * Once defined the ICL command "'command' ..." is equivalent to
 * OBEYW taskname action ... with the parameters of 'command' being appended
 * to the OBEYW as the value string sent to the task.
 *
 * Method:
 *
 * Prior to this routine being being called, the following global variables
 * will already have been initialised:
 *
 *   The parameter list of the ICL command line is in the 'arglist[]' array.
 *
 *   'nargs' is the number of command arguments.
 *
 * We use interpret() to obtain the string value of 'command' (return an
 * exception on failure or if not a string) and also to obtain 'taskname'
 * and the 'action' (which absent defaults to a parenthesis free copy of
 *'command').
 *
 * We then use install_abbrevs() to install the command (and all its
 * abbreviations) into the ICL symbol table linking the execution of 'command'
 * with the execution of define_interpret().
 *
 * On execution, the PROC_HIDDEN symbol table entry for COM(MAND) will be
 * found and its value member will point to a define_interpret() node whose
 * string part member contains the taskname and whose sub[0] member points
 * to a node whose string part member will hold the action.
 *
 * Bugs:
 *
 ******************************************************************************
 */
static value
proc_define
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    node *nw, *nw1;
    char *command, *taskname, *action;

    if (nargs < 2)
	return exception("TOOFEWPARS  Insufficient parameters for DEFINE");
    else if (nargs > 3)
	return exception("TOOMANYPARS  Too many parameters for DEFINE");

    if (isexc(val = interpret(arglist[0])))
	return val;

    if (!isstringtype(val))
	return (exception("DEFINE First parameter must form a command name"));

    command = uppercase(string_part(val));
    if (isexc(val = interpret(arglist[1])))
	return val;

    if (!isstringtype(val))
	return (exception("DEFINE Second parameter must form a task name"));

    taskname = string_part(val);

    if (nargs == 3) {				/* Explicit action given */
	if (isexc(val = interpret(arglist[2])))
	    return val;
	action = uppercase(string_part(val));
    } else                                      /* Default action = command */
	action = remove_parens(command);
/*
 * Build the nodes
 */
    if  ((nw1 = node_value(value_string(action))) == NODENIL ||
	 (nw  = node1(define_interpret, value_string(taskname), nw1))
	    == NODENIL)
	return exception("SYSERR  Memory exhausted during DEFINE");

    return (install_abbrevs(command, nw));
}

/******************************************************************************
 *
 *      P R O C _ A L O A D
 *
 * Interprets the builtin ICL command
 *   ALOAD executable [taskname]
 *
 *
 ******************************************************************************
 */
static value
proc_aload
(
node * n		/* ICL 'node' structure (given) */
)
{
    return aload_helper(TASKPTRNULL, 0);
}

/******************************************************************************
 *
 *      P R O C _  L O A D W
 *
 * Interprets the builtin ICL command
 *   LOADW executable [taskname]
 *
 ******************************************************************************
 */
static value
proc_loadw
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;

    if (isexc(val = aload_helper(TASKPTRNULL, 0)))
	return val;
    return trueval;
}

/******************************************************************************
 *
 *      P R O C _  L O A D D
 *
 * Interprets the builtin ICL command
 *   LOADD executable [taskname]
 *
 ******************************************************************************
 */
static value
proc_loadd
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;

    if (isexc(val = aload_helper(TASKPTRNULL, 1)))
	return val;
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ S E N D
 *
 * Interprets the builtin ICL command
 *   SEND taskname context ...
 *
 * which sends a control message to the ADAM task 'taskname'.
 *
 * Method:
 * Prior to this routine being being called, the following global variables
 * will already have been initialised:
 *
 *   The parameter list of the ICL command line is in the 'arglist[]' array.
 *
 *   'nargs' is the number of command arguments.
 *
 * We first obtain the taskname as a string from the first agrument and then
 * either locate or load it using task_find_or_load().
 *
 * We then obtain the 'context' (second argument) and check it is one of
 * GET/SET/OBEY/CANCEL/CONTROL.
 *
 * The remaining command arguments are obtained and built into a string to
 * send to the task.
 *
 * We then establish a path to the task use adam_sendt() to send the message
 * allowing a maximum delay of 20 seconds for a response. Depending on the
 * context we either display the parameter value (for a GET), return trueval
 * or raise an exception (if the final status is not SAI__OK)
 *
 * Bugs:
 *
 * Improve exception condition infromation messages.
 *
 ******************************************************************************
 */
static value
proc_send
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    char actionorparam[MSG_NAME_LEN],
	 svalue[MSG_VAL_LEN]=" ",
	 rvalue[MSG_VAL_LEN]=" ";
    char *taskname, *vp;
    int status, path, messid, message_context, message_status,
	svalue_len, rvalue_len,
	vpl, i;
    struct task *task,
		*p;
    node *lookup_node,
	 *argp,
	 *argp1;

    if (nargs < 3)
	return exception("TOOFEWPARS  Too few arguments for SEND");
/*
 * Get task name (first parameter)
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return (exception("SEND First parameter not a taskname"));

    taskname = string_part(val);
    if( strlen(taskname) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Taskname too long");
/*
 * Check task exists
 */
    if ((task = task_from_name(taskname, TRUE)) == TASKNULL)
    {
/*
 * Task is not loaded. However user is also allowed to refer to a task via
 * an ICL DEFINEd command name if present.
 */
	if ((lookup_node = lookup_symbol(taskname, SYM_DEFINED)) != NULL &&
	     lookup_node->interpret == define_interpret)
	{
	    vp = string_part(lookup_node->val);
	    for (p = task_list->next; p != task_list; p = p->next)
		if( strcmp(vp, p->filename) == 0)
		{
		    task = task_from_name(p->taskname, TRUE);
		    break;
		}
	}
    }
    if(task == TASKNULL)
	return (exception("SEND - task is not loaded"));
/*
 * Task located - proceed with processing the SEND command arguments. Next
 * argument is the SEND context (GET/SET/OBEY/CANCEL/CONTROL)
 */
    if (isexc(val = interpret(arglist[1])))
	return val;
    if (!isstringtype(val) ||
       (message_context = int_context(uppercase(string_part(val)))) == 0)
	return (exception(
	       "INVSET Second SEND parameter not GET/SET/OBEY/CANCEL/CONTROL"));

    if (isexc(val = interpret(arglist[2])) || !isstringtype(val) )
	return exception(
		"INVSET Third parameter not parameter name or action");
    vp = uppercase(string_part(val));
    vpl = strlen(vp) + 1;
    memcpy(actionorparam, vp,
	  (vpl > MSG_NAME_LEN ? MSG_NAME_LEN : vpl));
    if (message_context != GET) {
	if (nargs <= 3)
	    val = value_string(" ");
	else {
/*
 * Setup the argument tree such that string arguments are passed to the
 * task with quotes restored and, as ICL and the task are running
 * asynchronously and the use of HDS is not appropriate (cf DEFINE), ICL
 * variables should be passed by value
 */
	    for (i=0, argp=all_args; i<3; i++)
		argp = argp->sub[1];
		/* argp now points to the third argument */
	    if(argp->interpret != abbut_interpret)	/* one argument */
		if(argp->interpret == string_interpret)
		    val = value_string(
			restore_adamstring(string_part(argp->val)) );
		else {
		    if(isexc(val = interpret(argp)) )
			return val;
		    if(!isstringtype(val))
			val = string_val(val);
             } else {
		for(argp1 = argp;  argp1->interpret == abbut_interpret;
			argp1 = argp1->sub[1])
		    argp1->interpret = abbut_qstring_interpret;
		if (isexc(val = interpret(argp)) )
		    return val;
	    }
	}
	vp = string_part(val);
	vpl = strlen(vp) + 1;
	memcpy(svalue, vp,
	      (svalue_len = (vpl > MSG_VAL_LEN ? MSG_VAL_LEN : vpl)));
    } else {
	svalue_len = 1;
	svalue[0] = '\0';
    }
/*
 * Initiate communication with task
 */
    status = SAI__OK;
    adam_path(task->taskname, &path, &status);
    adam_sendt(20000, path, message_context, actionorparam, svalue,
	       svalue_len-1, rvalue, &rvalue_len, &messid, &status);
    message_status = status;
    status = SAI__OK;
    while (status == SAI__OK) {
	if (message_status == DTASK__ACTSTART ||
	    message_status == SAI__OK)
	    break;
	else if (message_status == MESSYS__INFORM)
	    uface_inform(path, rvalue, rvalue_len, &status);
	else if (message_status == MESSYS__PARAMREQ)
	    uface_askparam(path, rvalue, messid, &status);
	else
	    status = message_status;
        rvalue[0] = 0;
	ams_getreply(MESSYS__INFINITE,path, messid, MSG_NAME_LEN, MSG_VAL_LEN,
		     &message_status, &message_context, actionorparam,
		     &rvalue_len, rvalue, &status);
    }

/* Now for GET context or CONTROL DEFAULT "" display the return value */
   strtrim(svalue,svalue_len);
   if ((message_context == GET) ||
       ((message_context == CONTROL) &&
       !strcmp(actionorparam,"DEFAULT") &&
       (svalue[0]=='\0')) )
	if (status == SAI__OK) {
/*
 * Trim blank terminated string - DTASK FORTRAN artifact
 */
	    strtrim(rvalue, rvalue_len);
	    bufstring(rvalue);
	    bufnewline();
	    flshbuf();
	}
	else
	    rvalue[0] = '\0';

    if (status != SAI__OK ) {
	if (status != MESSYS__TIMEOUT) {
	    bufstring("SEND - unexpected return status from task \"");
	    bufstring(task->taskname);
	    bufstring("\"");
	    strtrim(rvalue, MSG_VAL_LEN);
	    if (rvalue[0] != '\0') {
		bufnewline();
		bufstring(" - return value string = \"");
		bufstring(rvalue);
		bufstring("\"");
		bufnewline();
	    }
	    bufnewline();
	    flshbuf();
	}
	return adam_exception("ADAMERR",  status);
    }
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ G E T
 *
 * Interprets the builtin ICL command
 *   GET taskname parameter (ICL variable)
 *
 * to obtain a task parameter of an ADAM I-task and to then put the value of
 * this parameter into an ICL variable
 *
 * Method:
 * similar to proc_send() - see above
 *
 * Bugs:
 * Could merge much of the method with proc_send
 *
 ******************************************************************************
 */
static value
proc_get
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    char parname[MSG_NAME_LEN],
	 rvalue[MSG_VAL_LEN];
    char *taskname, *vp;
    int status, path, messid, message_context, message_status;
    int	rvalue_len, vpl;
    struct task *task,
		*p;
    node *lookup_node;

    if (nargs < 3)
	return exception("TOOFEWPARS  Insufficient parameters for GET");
    if (nargs > 3)
	return exception("TOOMANYPARS  too many arguments to GET");
/*
 * Get task name (first parameter)
 */
    if (isexc(val = interpret(arglist[0])))
	return val;
    if (!isstringtype(val))
	return exception("GET  First parameter not a taskname");

    taskname = string_part(val);
    if( strlen(taskname) >= (size_t) MESSYS__TNAME)
	return exception("TASKERR: Taskname too long");
/*
 * Check task exists
 */
    if ((task = task_from_name(taskname, TRUE)) == TASKNULL)
    {
/*
 * Task is not loaded. However user is also allowed to refer to a task via
 * an ICL DEFINEd command name if present.
 */
	if ((lookup_node = lookup_proc(taskname)) != NULL &&
	     lookup_node->interpret == define_interpret)
	{
	    vp = string_part(lookup_node->val);
	    for (p = task_list->next; p != task_list; p = p->next)
		if( strcmp(vp, p->filename) == 0)
		{
		    task = task_from_name(p->taskname, TRUE);
		    break;
		}
	}
    }
    if(task == TASKNULL)
	return exception("ADAMERR  GET - task is not loaded");
/*
 * Second parameter is parameter
 */
    if (isexc(val = interpret(arglist[1])))
	return exception(
	    "INVARG second parameter not parameter name or action");

    vp = uppercase(string_part(val));
    vpl = strlen(vp) + 1;
    memcpy(parname, vp, (vpl > MSG_NAME_LEN ? MSG_NAME_LEN : vpl));
/*
 * Third parameter is ICL variable
 */
   if( arglist[2]->interpret != paren_interpret ||
       arglist[2]->sub[0]->interpret != name_interpret )
	return exception("INVARG  GET - third parameter not ICL variable");
/*
 * GET the value from the ADAM task
 */
    status = SAI__OK;
    message_context = int_context("GET");
    adam_path(task->taskname, &path, &status);
    if (status == SAI__OK) {
	adam_sendt(20000, path, message_context, parname, "", 1 /*svalue_len*/,
		   rvalue, &rvalue_len, &messid, &status);
	message_status = status,
	status = SAI__OK;
	while(status == SAI__OK && message_status == MESSYS__INFORM) {
	    uface_inform(path, rvalue, rvalue_len, &status);
	    ams_getreply(MESSYS__INFINITE,path, messid,
			 MSG_NAME_LEN, MSG_VAL_LEN,
			 &message_status, &message_context,
			 parname, &rvalue_len, rvalue, &status);
	}
	if( message_status == SAI__OK) {
/*
 * Trim blank terminated string - DTASK FORTRAN artifact
 */
	    for(vp = &rvalue[rvalue_len-1]; *vp <= ' '; vp--)
		/* Null statement */;
	    *(++vp) = '\0';		/* Terminate string */
            if(isexc(val = assign_helper(arglist[2]->sub[0],
					 value_string(rvalue))))
		return val;

	} else if (message_status == DTASK__ACTCANCEL ||
		   message_status == DTASK__ACTSTART)
	    status = SAI__OK;
    } else
	rvalue[0] = '\0';
    if (status != SAI__OK)
	return exception("ADAMERR GET Failure during message exchange");

    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ O B E Y W
 *
 * Interprets the builtin ICL command
 *   OBEYW taskname action [value]
 *
 * to send an OBEY control message to an ADAM task and wait for it to complete.
 * Also automatically loads the task as a attached task if necessary.
 *
 * 'taskname' is the name of the task to be run, 'action' is the action name
 * to be obeyed and 'value' is the optional parameters for the action
 * (this is sent to the task as the value part of the obey message).
 *
 * Prior to this routine being being called, the following global variables
 * will already have been initialised:
 *
 *   The parameter list of the ICL command line is in the 'arglist[]' array.
 *
 *   'nargs' is the number of command arguments.
 *
 * We evaluate the parameters as strings and use uface_obeyw() to send to
 * OBEY message and to handle and requests from the task. Either an error
 * or the final DTASK__ACTCOMPLETE message from the task will return to here.
 * If uface_obeyw() returns with an error code output informative messages
 * and generate an exception
 *
 * Bugs:
 *
 ******************************************************************************
 */
static value
proc_obeyw
(
node * n		/* ICL 'node' structure (given) */
)
{
    char *taskname, *command, *vp, name[MSG_NAME_LEN], svalue[MSG_VAL_LEN];
    struct task *task;
    node *n1, *argp, *argp1;
    value val;
    int vallen;
    int vpl, status = 0, hdsvars = 0, i;
    extern int sigint_flag;					/* main.c */

    if (nargs < 2)
	return exception("TOOFEWPARS  Insufficient parameters for OBEYW");

    if (isexc(val = interpret_to_string(arglist[0])))
	return exception(
	"OBEYW : The first parameter of OBEYW must evaluate to a taskname");

    taskname = string_part(val);

/* Check if 'taskname' has been DEFSTRINGed - this mirrors VMS behaviour */
    if ( (n1 = lookup_symbol(taskname, SYM_DEFINED)) != NODENIL &&
	 n1->interpret == defstring_interpret)
    {
	command = string_part(n1->val);
	taskname = CHARNIL;
    } else
	command = taskname;
    if ((task = task_find_or_load(command, taskname, 0, autoload ))
         == TASKNULL)
	return exception("TASKERR  could not load task");

    if (isexc(val = interpret_to_string(arglist[1])))
	return exception("OBEYW : second parameter must be an action string");
    vp = uppercase(string_part(val));
    vpl = strlen(vp) + 1;
    memcpy(name, vp, (vpl > MSG_NAME_LEN ? MSG_NAME_LEN : vpl));
    if (nargs > 2) {		/* optional parameters for the action */
	argp = all_args;
	for(i = 0; i<2 ;i++)	/* argp = interpreter node for 2nd argument */
	    argp = argp->sub[1];
/*
 * Setup the argument tree such that string arguments are passed to the
 * task with quotes restored and, purely for compatability with the VMS
 * version, ICL variables are passed by value
 */
	if(argp->interpret != abbut_interpret)	/* one argument */
	    if(argp->interpret == string_interpret)
		val = value_string(restore_adamstring(string_part(argp->val)) );
	else {
	    if(isexc(val = interpret(argp)) )
		return val;
	    if(!isstringtype(val))
		val = string_val(val);
	} else {
	    for(argp1 = argp;
		argp1->interpret == abbut_interpret;
		argp1 = argp1->sub[1])
		{
		argp1->interpret = abbut_qstring_interpret;
	    }
	    if (isexc(val = interpret(argp)) )
		return val;
	}
	vp = string_part(val);
	vpl = strlen(vp) + 1;
	memcpy(svalue, vp, (vallen = (vpl > MSG_VAL_LEN ? MSG_VAL_LEN : vpl)));
    } else {
	svalue[0] = '\0';
	vallen = 1;
    }

    status = SAI__OK;
    uface_obeyw(task->taskname, name, svalue, vallen, &status);
    if (status == MESSYS__EXTINT) {
	sigint_flag = 0;
	if (task->unix_pid > 0 &&
	    (kill(task->unix_pid, SIGHUP)  == 0 ||
	     kill(task->unix_pid, SIGTERM) == 0) )
	    {
	    val = exception1("CNTLC - task %s killed during OBEYW",
			     task->taskname);
	    return val;
	} else
	    return exception1("Could not kill %s - not started by this ICL",
				task->taskname);
    } else if (status != SAI__OK) {
 	bufstring("OBEYW - unexpected return status from task \"");
	bufstring(task->taskname);
	bufstring("\"");
	bufnewline();
	flshbuf();
	return adam_exception("ADAMERR",  status);
    }

    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ S T A R T O B E Y
 *
 * Interprets the builtin ICL command
 *   STARTOBEY (path) (messid) task action value...
 *
 * which sends an OBEY control message to an ADAM task and then stores the
 * path and transaction end index into ICL variables. When used in conjunction
 * with ENDOBEY this command can be used to set up multiple concurrent actions
 * in an ADAM I-task.
 *
 * 	path	An ICL variable to receive the path associated with the action
 *	messid	An ICL variable to receive the message transaction end index
 *		associated with the action
 *	task	The taskname of the task to receive the OBEY
 *	action	The action to be executed
 *	value	The parameters associated with the action (possibly none)
 *
 * Prior to this routine being being called, the following global variables
 * will have been initialised:
 *	The parameter list of the ICL command line will have been processed
 *	into the global 'arglist[]' array.
 *	'nargs' will have been set to the number of arguments.
 *
 * Method:
 * We first check that the first two arguments are parenthesised ICL variables.
 * We then extract the taskname and, using task_find_or_load(), either locate
 * it or set it running.
 * We then get actionname and value (if any) and check they are strings.
 *
 * Using adam_path() we establish a path to the task and then adam_send() is
 * used to send the message.  If the returned status is DTASK__ACTSTART the
 * action statrt has succeeded and we assign the ICL variables.  Otherwise we
 * return an exception.
 *
 * Bugs:
 * Improve error reporting
 *
 ******************************************************************************
 */
static value
proc_startobey
(
node * n		/* ICL 'node' structure (given) */
)
{
    char *taskname, *vp, name[MSG_NAME_LEN], valu[MSG_VAL_LEN],
	receivedvalue[MSG_VAL_LEN];
    struct task *task;
    value val;
    int tlen, vlen, vvlen;
    node *argp, *argm;
    int vpl, status, path, messid;

    if (nargs < 4)
	return exception("TOOFEWPARS  Insufficient parameters for STARTOBEY");

    argp = arglist[0];
    if (argp->interpret != paren_interpret)
	goto error_return;
    while (argp->interpret == paren_interpret)
	argp = argp->sub[0];
    if (argp->interpret != name_interpret)
	goto error_return;
    argm = arglist[1];
    if (argm->interpret != paren_interpret)
	goto error_return;
    while (argm->interpret == paren_interpret)
	argm = argm->sub[0];
    if (argm->interpret != name_interpret)
error_return:
	return exception("STARTOBEY : first two parameters not ICL variables");
    else
	 /* Null statement */ ;
    if (isexc(val = interpret_to_string(arglist[2])))
	return exception("STARTOBEY : third parameter not a task name");
    taskname = string_part(val);
    if( strlen(taskname) >= (size_t) MESSYS__TNAME)
	return exception("STARTOBEY: Taskname too long");
    if ((task = task_find_or_load(taskname, taskname, 0, autoload))
         == TASKNULL)
	return exception("TASKERR  could not load task for STARTOBEY");

    if (isexc(val = interpret_to_string(arglist[3])))
	return exception("STARTOBEY : fourth parameter not an action");

    vp = uppercase(string_part(val));
    vpl = strlen(vp) + 1;
    memcpy(name, vp, (vpl > MSG_NAME_LEN ? MSG_NAME_LEN : vpl));
    if (nargs > 4) {
	if (isexc(val = concat_args(4)))
	    return val;
	vp = string_part(val);
	vpl = strlen(vp) + 1;
	memcpy(valu, vp, (vvlen = (vpl > MSG_VAL_LEN ? MSG_VAL_LEN : vpl)));
    } else {
	vvlen = 1;
	valu[0] = '\0';
    }
    status = SAI__OK;
    uface_startobey(taskname, name, valu, vvlen, &path, &messid, &status);
    if (status == SAI__OK) {
	if (isexc(val = assign_helper(argp, value_integer(path))))
	    return val;

	if (isexc(val = assign_helper(argm, value_integer(messid))))
	    return val;

    } else {
	    bufstring("STARTOBEY failed to start transaction in task \"");
	    bufstring(taskname);
	    bufstring("\", action - \"");
	    bufstring(name);
	    bufstring("\"");
	    bufnewline();
	    flshbuf();
	    return adam_exception("ADAMERR",  status);
    }
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ E N D O B E Y
 *
 * Interprets the builtin ICL command
 *   ENDOBEY  (path)  (messid)
 *
 * which is used to wait for completion of an ADAM I-task initiated by
 * STARTOBEY
 *
 *	path	The path associated with the action
 *	messid	The transaction end index associated with the action
 *
 * Method:
 * We obtain the path and messid values as integers then call uface_endobey()
 * to handle the remainer of the transaction with the task.  uface_endobey()
 * should return with SAI__OK status. If not we report this and return an
 * exception
 *
 * Bugs:
 * Improve error reporting
 *
 ******************************************************************************
 */
static value
proc_endobey
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    int path, messid, status;
    char valuereceived[MSG_VAL_LEN];

    if (nargs < 2)
	return exception("TOOFEWPARS  Insufficient parameters for ENDOBEY");
    else if (nargs > 2)
	return exception("TOOMANYPARS  Too many parameters for ENDOBEY");

    if (isexc(val = interpret_to_integer(arglist[0])))
	return val;

    path = integer_part(val);
    if (isexc(val = interpret_to_integer(arglist[1])))
	return val;

    messid = integer_part(val);
    uface_endobey(path, messid, valuereceived, &status);
    if (status != SAI__OK) {
	bufstring("ENDOBEY - unexpected return status");
	bufnewline();
	flshbuf();
	return adam_exception("ADAMERR",  status);
    }

    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ K I L L
 *
 * Interprets the builtin ICL commands
 *   KILL    taskname
 *   KILLW    taskname
 *
 * which should either terminate an ADAM task asychronously or synchronously
 * with respect to ICl iself. At present both these commands do the same
 * thing!
 *
 * Tasks are terminated by sending them a SIGHUP signal which will cause exit
 * handlers to be obeyed which will properly clear down the path, transaction
 * tables and the message system associated with that task.
 *
 * NB SIGHUP is now used instead of SIGTERM (310795) bu we probably should
 * try both methods until the next major release of the SSC (BKM)
 *
 * The eventual death of the child process is signalled to ICL by a system
 * generated SIGCHLD signal. This is processed in the task_exithandler()
 * routine.
 *
 ******************************************************************************
 */
static value
proc_kill
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    struct task *task;
    node *n1;
    char *name;
    int status;

    if (nargs < 1)
	return exception("TOOFEWPARS  Insufficient parameters for KILL");
    else if (nargs > 1)
	return exception("TOOMANYPARS  Too many parameters for KILL");

    if (isexc(val = interpret_to_string(arglist[0])))
	return val;

    name = string_part(val);
/* Check for some form of DEFSTASK, DEFSTRING etc */
    if ( (n1 = lookup_symbol(name, SYM_DEFINED)) != NODENIL &&
	 n1->interpret == define_interpret)
	name = string_part(n1->val);
    if ((task = task_from_name(name, TRUE))  == TASKNULL &&
        (task = task_from_name(name, FALSE)) == TASKNULL )
	return exception("TASKERR  Task not found");

    if (task->unix_pid > 0)
	if (kill(task->unix_pid, SIGHUP) || kill(task->unix_pid, SIGTERM))
	    return sys_exception("TASKERR  Couldn't kill task");
	else
	    return trueval;
    else
	return exception(
	    "TASKERR  Couldn't kill task - not started by this ICL");
}

/******************************************************************************
 *
 *	N B S _ A C C E S S
 *
 * This function contains common code for proc_getnbs() and proc_putnbs()
 *
 * Given a notice board component specification of the form
 * nbsname.{level1...}.component_name this routine accesses the NBS (and adds
 * it to a ICL internal list) and works down to the last specified component.
 *
 * It returns either the component NBS identifier (as the integer part of the
 * returned value) or some form of exception.
 *
 ******************************************************************************
 */
static value
nbs_access
(
char *name	/* NBS specification (given) */
)
{
    char *nbsname, *comp_name;
    struct nbslist *nbs_ll, *nbs_new;
    int nbs_id, new_id;
    int found = FALSE, status = SAI__OK;

    if ( (nbsname = strtok(name, ".")) == NULL ||
	 (int)strlen(nbsname) > NBS_K_MAXNAME)
	return exception("NBSERR Incorrect Noticeboard Item Name");

/* Search ICL internal list to see if already mapped */
    if (nbs_list != NULL)
	for(nbs_ll = nbs_list; nbs_ll != NULL; nbs_ll = nbs_ll->next)
	    if( strcmp(nbs_ll->name, nbsname) == 0) {
		found = TRUE;
		break;
	    }
    if (found)
	nbs_id = nbs_ll->id;
    else {		/* Add to list */
	nbc_find_noticeboard(nbsname, &nbs_id, &status);
	if (status != SAI__OK)
	    return adam_exception("NBSERR",  status);
	nbs_new = (struct nbslist *) malloc(sizeof(struct nbslist));
	strcpy(nbs_new->name, nbsname);
	nbs_new->id = nbs_id;
	nbs_new->next = NULL;
	if (nbs_list == NULL)
	    nbs_list = nbs_new;
	else {
	    nbs_ll = nbs_list;
	    while (nbs_ll->next != NULL)
		nbs_ll = nbs_ll->next;
	    nbs_ll->next = nbs_new;
	}
    }

/* Work down through components to final one */
    if( (comp_name = strtok(CHARNIL, ".")) == NULL ||
	(int)strlen(comp_name) > NBS_K_MAXNAME)
	return exception1("NBSERR Incorrect Noticeboard Item Name %s",
			  comp_name);
    else
	do {
	    nbc_find_item(nbs_id, comp_name, &new_id, &status);
	    if (status != SAI__OK)
		return adam_exception("NBSERR",  status);
	    nbs_id = new_id;
	} while( (comp_name = strtok(CHARNIL, ".")) != NULL);

    return value_integer(nbs_id);
}

/******************************************************************************
 *
 *	P R O C _ P U T N B S
 *
 * Interprets the builtin ICL command
 *   PUTNBS nbsname value
 *
 * which puts the specified 'value' (which may be an ICL variable) into
 * the specified component of an EXISTING NBS. ICL will not create the
 * NBS and it is also the responsibility of the task  performing NBS creation
 * to ensure that the space allowed is suitable to hold the required ICL
 * values.
 *
 ******************************************************************************
 */
static value
proc_putnbs
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val, newval;
    char *name, stype[NBS_K_MAXTYPE];
    int slen, nbslen;
    int nbs_id;
    int ival;
    int oval;
    char *sval;
    float rval;
    double dval;

    int status = SAI__OK;

    if (nargs < 2)
	return exception("TOOFEWPARS  Insufficient parameters for PUTNBS");
    else if (nargs > 2)
	return exception("TOOMANYPARS  Too many parameters for PUTNBS");

/* Get the noticeboard specification (1st argument) */
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    name = string_part(val);

/* Get the new value for the NBS component (2nd argument) */
    if (isexc(newval = interpret(arglist[1])))
	return newval;

/* Set the default for NBS access to be "WORLD_WRITE */
    nbc_tune("WORLD_WRITE", 1, &oval, &status);
    if (status != SAI__OK)
	return adam_exception("NBSERR",  status);

/* Obtain the NBS identifier for required component */
    if (isexc(val = nbs_access(name)))
	return val;
    else
	nbs_id = integer_part(val);
/*
 * Get the component type and put the value into it if a recognised primitive
 * type
 */
    nbc_get_type(nbs_id, stype, &status);
    if (status != SAI__OK)
	return adam_exception("NBSERR",  status);
    if (strcmp(stype, "_INTEGER") == 0 ) {
	ival = integer_part(integer_val(newval));
	nbc_put_value(nbs_id, 0, sizeof(ival), &ival, &status);
    } else if (strcmp(stype, "_LOGICAL") == 0 ) {
	ival = as_logical(newval);
	nbc_put_value(nbs_id, 0, sizeof(ival), &ival, &status);
    } else if (strcmp(stype, "_REAL") == 0) {
	rval = real_part(real_val(newval));
	nbc_put_value(nbs_id, 0, sizeof(rval), &rval, &status);
    } else if (strcmp(stype, "_DOUBLE") == 0) {
	dval = real_part(real_val(newval));
	nbc_put_value(nbs_id, 0, sizeof(dval), &dval, &status);
    } else if (strncmp(stype, "_CHAR", 5) == 0) {
	char buff[200];
	nbc_get_size(nbs_id, &nbslen, &slen, &status);
	sval = string_part(string_val(newval));
        buff[0] = '\0';
	if ( (slen = strlen(sval)) > nbslen)
	    sprintf(buff,
		    "NBSERR string \"%s\" too long - NBS size %d bytes",
		    sval, nbslen);
        else if (slen > 200)
	    sprintf(buff, "NBSERR string length %d exceeds ICL 200 byte limit",
		    slen);
	if (buff[0] != '\0')
	    return exception((char *) buff);
        pad(buff, nbslen, ' ');
	memcpy(buff, sval, slen);
	nbc_put_value(nbs_id, 0, nbslen, buff, &status);
    } else
	return exception1("NBSERR Item does not have a suitable type (%s)",
			  stype);
    if ( status == SAI__OK)
	return trueval;
    else
	return adam_exception("NBSERR",  status);
}

/******************************************************************************
 *
 *	P R O C _ G E T N B S
 *
 * Interprets the builtin ICL command
 *   GETNBS nbsname (value)
 *
 * which accesses the EXISTING NBS 'nbsname' and reads the specified component
 * into the ICL varaible 'value'.
 *
 ******************************************************************************
 */
static value
proc_getnbs
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    node *variable;
    char *name, stype[NBS_K_MAXTYPE];
    int actbytes, clen;
    int nbs_id;
    int ival;
    char cval[200], *sval;
    float rval;
    double dval;
    int status = SAI__OK;

    if (nargs < 2)
	return exception("TOOFEWPARS  Insufficient parameters for GETNBS");
    else if (nargs > 2)
	return exception("TOOMANYPARS  Too many parameters for GETNBS");

/* Get the noticeboard specification */
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    name = string_part(val);

/* Get the ICL variable to hold the component */
    variable = arglist[1];
    if (variable->interpret != paren_interpret)
	return exception(
		"GETNBS : Assignment to a parameter which is not a variable");
/*
 * Find the name interpret node
 */
    while (variable->interpret == paren_interpret)
	variable = variable->sub[0];
    if (variable->interpret != name_interpret)
	return exception(
		"GETNBS : Assignment to a parameter which is not a variable");

/* Obtain NBS identifier for required component */
    if (isexc(val = nbs_access(name)))
	return val;
    else
	nbs_id = integer_part(val);

    nbc_get_type(nbs_id, stype, &status);
	if (status != SAI__OK)
	    return adam_exception("NBSERR",  status);
    if (strcmp(stype, "_INTEGER") == 0) {
	nbc_get_value(nbs_id, 0, sizeof(ival), &ival, &actbytes, &status);
	val = value_integer(ival);
    } else if (strcmp(stype, "_LOGICAL") == 0 ) {
	nbc_get_value(nbs_id, 0, sizeof(ival), &ival, &actbytes, &status);
	val = value_logical(ival);
    } else if (strcmp(stype, "_REAL") == 0) {
	nbc_get_value(nbs_id, 0, sizeof(rval), &rval, &actbytes, &status);
	val = value_real(rval);
    } else if (strcmp(stype, "_DOUBLE") == 0) {
	nbc_get_value(nbs_id, 0, sizeof(dval), &dval, &actbytes, &status);
	val = value_real(dval);
    } else if (strncmp(stype, "_CHAR", 5) == 0) {
	pad(cval, 200, ' ');
	nbc_get_value(nbs_id, 0, 200, cval, &actbytes, &status);
        sval = strtrim(cval, 200);
	val = value_string(sval);
    } else
	return exception("NBSERR Item does not have a suitable type");

    if ( status != SAI__OK)
	return adam_exception("NBSERR",  status);
/*
 * Assign ICL variable
 */
    if(isexc(val = assign_helper(variable, val)))
	return val;

    return trueval;

}

/******************************************************************************
 *
 *	P R O C _ T A S K S
 *
 * Interprets the builtin ICL command
 *   TASKS
 *
 * which lists the name of all the loaded tasks known to ICL
 *
 ******************************************************************************
 */
static value
proc_tasks
(
node * n		/* ICL 'node' structure (given) */
)
{
    struct task *p;

    sprintf(buf, "%30s\tProcess Id\n\n", "TASKNAME");
    bufstring(buf);
    for (p = task_list->next; p != task_list; p = p->next) {
	sprintf(buf, "%30s\t%d\n", p->taskname, p->unix_pid);
	bufstring(buf);
    }
    flshbuf();

    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ C H E C K T A S K
 *
 * Interprets the builtin ICL command
 *   CHECKTASK taskname (loaded)
 *
 * which will check if an ADAM task is currently loaded. 'loaded' is an ICL
 * variable which will be set to TRUE or FALSE accordingly.
 *
 ******************************************************************************
 */
static value
proc_checktask
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;
    char *taskname;
    node *variable;

    if (nargs < 2)
	return exception("TOOFEWPARS  Insufficient parameters for CHECKTASK");
    else if (nargs > 2)
	return exception("TOOMANYPARS  Too many parameters for CHECKTASK");

/* Get the taskname */
    if (isexc(val = interpret_to_string(arglist[0])))
	return val;
    taskname = string_part(val);

/* Get the ICL variable to hold the boolean */
    if (arglist[1]->interpret != paren_interpret ||
	arglist[1]->sub[0]->interpret != name_interpret)
	return exception(
	      "CHECKTASK : Assignment to a parameter which is not a variable");
     else
	variable = arglist[1]->sub[0];

/* Check if the task can be found - also check for detached tasks */
    if ( task_from_name(taskname, TRUE) != TASKNULL)
	val = assign_helper(variable, trueval);
    else
	val = assign_helper(variable, falseval);
    if (isexc(val))
	return val;
    else
	return trueval;
}

/******************************************************************************
 *
 *	P R O C _ W A I T
 *
 * Interprets the builtin ICL command
 *   WAIT
 * which waits for the specified interval expressed in seconds
 *
 * By the time this routine is called the parameter list will have been
 * linearised into arglist[] such that arglist[0] corresponds to the wait
 * period.  We evaluate this using interpret_to_integer() and then sleep()
 * that number of seconds.
 *
 ******************************************************************************
 */
static value
proc_wait
(
node * n		/* ICL 'node' structure (given) */
)
{
    value val;

    if (nargs < 1)
	return exception("TOOFEWPARS  Insufficient parameters for WAIT");
    else if (nargs > 1)
	return exception("TOOMANYPARS  Too many parameters for WAIT");

    if (isexc(val = interpret_to_integer(arglist[0])))
	return val;
    sleep(integer_part(val));
    return trueval;
}

/******************************************************************************
 *
 *	P R O C _ U N I M P L
 *
 * Simply produces an exception with a suitable message for not yet implemented
 * features of Unix ICL.
 *
 ******************************************************************************
 */
static value
proc_unimpl
(
void
)
{
    return exception("UNIMPLERR  command not yet implemented");
}

/******************************************************************************
 *
 *	I N I T _ A D A M
 *
 * ADAM module initialisation
 *
 ******************************************************************************
 */
value
init_adam
(
void
)
{
    value val;
    char ownname[MESSYS__TNAME];
    int status;

    sprintf(ownname, "%s%d", ICLSYSTEMNAME, getpid());
    status = SAI__OK;
    ams_init(ownname, &status);
    if (status != 0) {
	return exception("ADAM : failed to initialise messys");
    }
    if ((isexc(val = store_symbol(
	  "DEFINE", SYM_BUILTIN, node_builtin(proc_define))))	||
	(isexc(val = store_symbol(
	  "DEFTASK", SYM_BUILTIN, node_builtin(proc_unimpl))))	||
	(isexc(val = store_symbol(
	  "KILL", SYM_BUILTIN, node_builtin(proc_kill))))	||
	(isexc(val = store_symbol(
	  "KILLW", SYM_BUILTIN, node_builtin(proc_kill))))	||
	(isexc(val = store_symbol(
	  "ALOAD", SYM_BUILTIN, node_builtin(proc_aload))))	||
	(isexc(val = store_symbol(
	  "LOADD", SYM_BUILTIN, node_builtin(proc_loadd))))	||
	(isexc(val = store_symbol(
	  "LOADW", SYM_BUILTIN, node_builtin(proc_loadw))))	||
	(isexc(val = store_symbol(
	  "OBEYW", SYM_BUILTIN, node_builtin(proc_obeyw))))	||
	(isexc(val = store_symbol(
	  "STARTOBEY", SYM_BUILTIN, node_builtin(proc_startobey)))) ||
	(isexc(val = store_symbol(
	  "ENDOBEY", SYM_BUILTIN, node_builtin(proc_endobey))))	||
	(isexc(val = store_symbol(
	  "CHECKTASK", SYM_BUILTIN, node_builtin(proc_checktask))))	||
	(isexc(val = store_symbol(
	  "DUMPTASK", SYM_BUILTIN, node_builtin(proc_unimpl))))	||
	(isexc(val = store_symbol(
	  "SEND", SYM_BUILTIN, node_builtin(proc_send))))	||
	(isexc(val = store_symbol(
	  "GET", SYM_BUILTIN, node_builtin(proc_get))))		||
	(isexc(val = store_symbol(
	  "TASKS", SYM_BUILTIN, node_builtin(proc_tasks))))	||
	(isexc(val = store_symbol(
	  "WAIT", SYM_BUILTIN, node_builtin(proc_wait))))	||
	(isexc(val = store_symbol(
	  "GETNBS", SYM_BUILTIN, node_builtin(proc_getnbs))))	||
	(isexc(val = store_symbol(
	  "PUTNBS", SYM_BUILTIN, node_builtin(proc_putnbs))))	||
	(isexc(val = store_symbol(
	  "DEFSHARE", SYM_BUILTIN, node_builtin(proc_unimpl))))	||
	(isexc(val = store_symbol(
	  "DEFUSER", SYM_BUILTIN, node_builtin(proc_unimpl)))))
	return (val);
    else
	return (trueval);
}
