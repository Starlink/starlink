#define PRINT 0

#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <time.h>
#include <fcntl.h>
#include <unistd.h>
#include <ctype.h>

#include "adam_defns.h"
#include "ams.h"
#include "cnf.h"
#include "dtask_err.h"
#include "ems.h"
#include "ems_par.h"
#include "f77.h"
#include "messys_err.h"
#include "messys_len.h"
#include "messys_par.h"
#include "sae_par.h"

#define PAR__ABORT 146703171
#define PAR__NULL 146703163
#define PAR__SZNAM 15
#define SUBPAR__NOPAR 145392443

#include "slv.h"

typedef struct task_data {
   pid_t pid;
   int detached;
   int connected;
   int killed;
   int terminated;
   int exit_status;
   volatile struct task_data *prev;
   volatile struct task_data *next;
} task_data;

typedef struct param_data {
   char *last_value;
   char name[ PAR__SZNAM + 1 ];
   int nreq;
   struct param_data *next;
} param_data;

/* External variables. */
extern char **environ;           /* Environment variable pointer */

/* Static variables. */
static int closedown_mode = 0;   /* In task closedown mode? */
static struct sigaction new_SIGCHLD; /* New SIGCHLD signal action */
static struct sigaction old_SIGCHLD; /* Old SIGCHLD signal action */
static volatile task_data *active_list = NULL; /* Active task list */

/* Local function prototypes. */
static pid_t LoadW( const char *, const char *, int, int, int * );
static void Closedown( void );
static void HandleInform( const char *, int, const char *, int, int, int * );
static void HandleSIGCHLD( int );
static void InitAMS( int * );
static void InitClosedown( int * );
static void Kill( pid_t, int * );
static void KillW( pid_t, int * );
static void RemoveTask( volatile task_data * );
static void WaitK( pid_t, int, int * );
static volatile task_data *LookupTask( pid_t );
static volatile task_data *NewTask( pid_t, int );

F77_SUBROUTINE(msg_out)( CHARACTER(MSG_NAME), CHARACTER(MSG_TEXT),
                         INTEGER(STATUS)
                         TRAIL(MSG_NAME) TRAIL(MSG_TEXT) );

F77_SUBROUTINE(msg_sync)( INTEGER(status) );

F77_SUBROUTINE(subpar_findpar)( CHARACTER(MASTER),
                                INTEGER(IPAR),
                                INTEGER(STATUS)
                                TRAIL(MASTER) );

F77_SUBROUTINE(subpar_cancl)( INTEGER(IPAR),
                              INTEGER(STATUS) );

F77_SUBROUTINE(subpar_def0c)( INTEGER(IPAR),
                              CHARACTER(SUGGESTED),
                              INTEGER(STATUS)
                              TRAIL(SUGGESTED) );

F77_SUBROUTINE(subpar_getname)( INTEGER(IPAR),
                               CHARACTER(PARVALUE),
                               INTEGER(STATUS)
                               TRAIL(PARVALUE) );


/* Fortran interface implementations. */
F77_INTEGER_FUNCTION(slv_loadw)( CHARACTER(TASK),
                                 CHARACTER(FILE),
                                 LOGICAL(DETACH),
                                 INTEGER(TMOUT),
                                 INTEGER(STATUS)
                                 TRAIL(TASK)
                                 TRAIL(FILE) ) {

/*
*+
*  Name:
*     SLV_LOADW

*  Purpose:
*     Load an ADAM task as a slave.

*  Language:
*     ANSI C.

*  Invocation:
*     RESULT = SLV_LOADW( TASK, FILE, TMOUT, STATUS )

*  Description:
*     This function loads a specified ADAM task as a slave (or ensures
*     that a specified task has already been loaded). Before returning,
*     it waits until loading is complete and ensures that the task is
*     ready to receive messages.
*
*     Typically, this function is called to load an ADAM monolith to
*     which messages will subsequently be sent instructing it to carry
*     out specified actions.

*  Arguments:
*     TASK = CHARACTER * ( * ) (Given)
*        The name by which the task will be known to the ADAM message
*        system. You may also give the name of a previously loaded
*        task, in which case the function will ensure that it has been
*        loaded.  Note that slave tasks may be shared by several
*        masters, so if you want a slave for private use, you should
*        make this name unique.
*     FILE = CHARACTER * ( * ) (Given)
*        The name of the executable file containing the task to be
*        loaded. This is only used if a task with the specified name is
*        not already loaded. The standard methods for locating this
*        file are followed if necessary (e.g. using the UNIX PATH).
*
*        If you simply wish to check whether a specified task is
*        already loaded, then FILE may be blank. In this case an error
*        will result if the task is not loaded.
*     DETACH = LOGICAL (Given)
*        If this value is .TRUE., the task will be loaded as a
*        "detached" process that is not bound specifically to its
*        parent and whose lifetime extends beyond the termination of
*        its parent process.  If it is .FALSE., the task will be
*        "attached" to its parent and will terminate when its parent
*        terminates (if not killed sooner).
*     TMOUT = INTEGER (Given)
*        A timeout period (in seconds). An error will result if it
*        takes longer than the specified time to load the task and
*        establish communication with it. A zero or negative value
*        results in the function waiting indefinitely (i.e. no
*        timeout).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     SLV_LOADW = INTEGER
*        If a new task is loaded by this function, then the ID of the
*        process into which it was loaded is returned. If the task was
*        already loaded, a value of zero is returned.

*  Notes:
*     - If an error occurs (or the function is called with the STATUS
*     argument set), a value of -1 is returned.
*     - To remove the task after you have finished with it, you should
*     use the SLV_KILLW routine and pass the process ID returned by
*     SLV_LOADW.  Note, however, that this has no effect if the process
*     ID is zero (meaning that the corresponding call to SLV_LOADW
*     found the task already loaded).

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     8-MAY-1997 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   DECLARE_INTEGER(PID);
   char *file;
   char *task;
   int detach;
   int status;
   int tmout;

/* Generate pointers to Fortran arguments. */
   GENPTR_CHARACTER(TASK)
   GENPTR_CHARACTER(FILE)
   GENPTR_LOGICAL(DETACH)
   GENPTR_INTEGER(TMOUT)
   GENPTR_INTEGER(STATUS)

/* Initialise. */
   PID = (F77_INTEGER_TYPE) -1;

/* Check the inherited status. */
   status = (int) *STATUS;
   if( status != SAI__OK ) return PID;

/* Copy input arguments to local storage. */
   task = cnf_creim( (char *) TASK, TASK_length );
   file = cnf_creim( (char *) FILE, FILE_length );
   detach = F77_ISTRUE( *DETACH );
   tmout = (int) *TMOUT;

/* Load the task. */
   PID = (F77_INTEGER_TYPE) LoadW( task, file, detach, tmout, &status );

/* Free local storage and return output arguments. */
   cnf_free( task );
   cnf_free( file );

/* If an error occurred, report a contextual error message and clear
   the process ID. */
   if ( status != SAI__OK ) {
      emsRep( "SLV_LOADW_ERR",
                 "SLV_LOADW: Error while loading a slave task.", &status );
      PID = (F77_INTEGER_TYPE) -1;
   }

/* Return the status. */
   *STATUS = (F77_INTEGER_TYPE) status;

/* Return the process ID. */
   return PID;
}

/* Handle SIGCHLD signals. */
/* ----------------------- */
/* N.B. SIGCHLD signals must be blocked during execution of this function. */
static void HandleSIGCHLD( int signo ) {
   int stat_val;                 /* Child status information */
   pid_t pid;                    /* Child process ID */
   sigset_t set;                 /* Process signal mask */
   volatile task_data *task;     /* Pointer to task data */

/* Return immediately if this function is invoked for the wrong signal. */
   if ( signo != SIGCHLD ) return;

/* Search the list of active tasks to see if the SIGCHLD signal
   corresponds to any of them terminating. */
   task = active_list;
   while( task ) {

/* Wait (without hanging) for each PID we recognise, but omitting
   those for tasks that have already terminated. Do not wait for all
   child processes, or we may reap the exit status required by some
   other piece of code. */
      if ( !task->terminated ) {
         pid = waitpid( task->pid, &stat_val, WNOHANG | WUNTRACED );

/* If a child process has terminated (but not if it has simply
   stopped), flag its entry in the active task list as "terminated"
   and store the associated status information. */
         if ( ( pid > (pid_t) 0 ) && !WIFSTOPPED( stat_val ) ) {
            task->exit_status = stat_val;
            task->terminated = 1;
#if PRINT
            (void) fprintf(stderr,  "HandleSIGCHLD: %s process %ld terminated%s.\n",
                           task->detached ? "Detached" : "Attached",
                           (unsigned long) pid,
                           closedown_mode ?
                           "" : "; flagged in active task list" );
            (void) fflush( NULL );
#endif

/* If we are in "closedown mode", no further code will execute to wait
   for the process to terminate, so we can remove the task from the
   active task list right now. */
            if ( closedown_mode ) RemoveTask( task );
            break;
         }
      }

/* If necessary, examine the next task in the list. */
      task = task->next;

/* If we reach the end of the list without obtaining any status
   information, then the SIGCHLD signal was for a child process
   started by some other code, about which we know nothing. Since that
   code may want to wait for the exit status (or whatever), we now
   arrange for the signal to be handled by the original (old) signal
   handler, as if we hadn't interfered. */
      if ( task == active_list ) {
#if PRINT
      (void) fprintf(stderr,  "HandleSIGCHLD: Unrecognised process terminated; passing "
                     "SIGCHLD to previous signal handler.\n" );
      (void) fflush( NULL );
#endif

/* There are two code options here, depending on whether portability
   or reliability is a priority... */
#if 0                            /* Portability option */

/* Re-establish the original signal action and un-block the SIGCHLD
   signal (which is blocked because we are in a SIGCHLD signal
   handler). */
         (void) sigaction( SIGCHLD, &old_SIGCHLD, NULL );
         (void) sigemptyset( &set );
         (void) sigaddset( &set, SIGCHLD );
         (void) sigprocmask( SIG_UNBLOCK, &set, NULL );

/* Re-generate the SIGCHLD signal. (Note this is not 100% secure.  We
   are guaranteed the delivery of at least one unblocked signal before
   "kill" returns. If it is not ours, there is no problem - we simply
   end up back here again when it is delivered. However, if more than
   one SIGCHLD signal is delivered here, we could miss handling one
   that we should know about.) */
         (void) kill( getpid(), SIGCHLD );

/* Re-block SIGCHLD signals and put our signal action back in place. */
         (void) sigprocmask( SIG_BLOCK, &set, NULL );
         (void) sigaction( SIGCHLD, &new_SIGCHLD, NULL );

#else                            /* Reliability option */
/* This alternative code works by simply setting up the original
   signal mask (with SIGCHLD included) and executing the associated
   handler function directly. It avoids the reliability problem above,
   but if the original signal handling function was established with
   the C "signal" function, then there is no guarantee (from POSIX)
   that the old_SIGCHLD structure contents are meaningful. */
          if ( ( old_SIGCHLD.sa_handler != SIG_DFL ) &&
               ( old_SIGCHLD.sa_handler != SIG_IGN ) ) {
             (void) sigprocmask( SIG_SETMASK, &old_SIGCHLD.sa_mask, NULL );
             ( *old_SIGCHLD.sa_handler )( SIGCHLD );
          }
#endif
         break;
      }
   }
}

/* Look up a task in the active task list. */
/* --------------------------------------- */
static volatile task_data *LookupTask( pid_t pid ) {
   volatile task_data *result = NULL; /* Result to return */
   volatile task_data *task;     /* Pointer to task data */

/* Examine the active task list to identify the PID. */
   task = active_list;
   while( task ) {

/* If found, return a pointer to the task data and quit searching. */
      if ( task->pid == pid ) {
         result = task;
         break;
      }

/* If necessary, examine the next task. Quit when the end of list is
   reached. */
      task = task->next;
      if ( task == active_list ) break;
   }

/* Return the result. */
   return result;
}

/* Create a new task entry in the active task list. */
/* ------------------------------------------------ */
static volatile task_data *NewTask( pid_t pid, int detached ) {
   sigset_t oset;                /* Old process signal mask */
   sigset_t set;                 /* New process signal mask */
   volatile task_data *task;     /* Pointer to task data */

/* Allocate memory for the task data. */
   task = (volatile task_data *) malloc( sizeof( volatile task_data ) );
   if ( task ) {

/* Store the PID and initialise the other task data. */
      task->pid = pid;
      task->detached = ( detached != 0 );
      task->connected = 0;
      task->killed = 0;
      task->terminated = 0;
      task->exit_status = 0;

/* Block SIGCHLD signals so the active task list can be accessed. */
      (void) sigemptyset( &set );
      (void) sigaddset( &set, SIGCHLD );
      (void) sigprocmask( SIG_BLOCK, &set, &oset );

/* Link the task into the active task list. */
      if ( active_list ) {
         task->prev = active_list->prev;
         task->next = active_list;
         task->prev->next = task;
         task->next->prev = task;
      } else {
         task->next = task;
         task->prev = task;
      }
      active_list = task;

/* Restore the original process signal mask. */
      (void) sigprocmask( SIG_SETMASK, &oset, NULL );
   }

/* Return the task data pointer. */
   return task;
}

/* Remove a task from the active task list. */
/* ---------------------------------------- */
static void RemoveTask( volatile task_data *task ) {
   sigset_t oset;                /* Old process signal mask */
   sigset_t set;                 /* New process signal mask */

#if PRINT
   (void) fprintf(stderr,  "RemoveTask: Removing %s process %ld from active task "
                  "list.\n",
                  task->detached ? "detached" : "attached",
                  (unsigned long) task->pid );
   (void) fflush( NULL );
#endif

/* Block SIGCHLD signals so the active task list can be accessed. */
   (void) sigemptyset( &set );
   (void) sigaddset( &set, SIGCHLD );
   (void) sigprocmask( SIG_BLOCK, &set, &oset );

/* Unlink the list entry. */
   task->next->prev = task->prev;
   task->prev->next = task->next;

/* Update the head of list pointer, setting it to NULL if the list is
   empty. */
   if ( active_list == task ) active_list = task->next;
   if ( active_list == task ) active_list = NULL;

/* Restore the original process signal mask. */
   (void) sigprocmask( SIG_SETMASK, &oset, NULL );

/* Free the memory used. */
/*   free( (void *) task ); -- not correct according to Solaris debug
                               also line 385 (alloc) is leaked */
}

/* Kill a task and return immediately (without waiting). */
/* ----------------------------------------------------- */
static void Kill( pid_t pid, int *status ) {
   volatile task_data *task;     /* Pointer to task data */

/* Check that the PID supplied is positive, otherwise do nothing. */
   if ( pid > (pid_t) 0 ) {

/* Begin a new error reporting context. */
      emsBegin( status );

/* Look up the PID in the active task list. */
      task = LookupTask( pid );

/* If the PID was not found, then report an error. */
      if ( !task ) {
         *status = SAI__ERROR;
         emsSeti( "PID", (int) pid );
         emsRep( "SLV_KILL_PID",
                    "Process ^PID is not a valid slave process.", status );

/* If the task has already been killed, or has terminated, then just
   set the "killed" flag and do nothing more. */
      } else if ( task->killed || task->terminated ) {
         task->killed = 1;

/* Otherwise, kill it. Use a gentle signal (SIGHUP) if an ADAM message
   system connection has been made (so that ADAM exit handlers will
   execute), otherwise do it brutally (SIGKILL). */
      } else {
#if PRINT
         (void) fprintf(stderr,  "Kill: Killing %s process %ld with signal %s.\n",
                        task->detached ? "detached" : "attached",
                        (unsigned long) pid,
                        task->connected ? "SIGHUP" : "SIGKILL" );
         (void) fflush( NULL );
#endif
         (void) kill( pid, task->connected ? SIGHUP : SIGKILL );

/* Note that the task has been killed, so that we won't kill it
   again. It may not actually terminate until later (that will be
   handled by the HandleSIGCHLD function). */
         task->killed = 1;
      }
   }

/* End the error reporting context. */
   emsEnd( status );
}

void SlvWaitK( pid_t pid, int timeout, int *status ) {
   emsBegin( status );
   WaitK( pid, timeout, status );
   if ( *status != SAI__OK ) {
      emsRep( "SLV_WAITK_ERR",
                 "SlvWaitK: Error while waiting for a slave task to "
                 "terminate.", status );
   }
   emsEnd( status );
}

/* Wait for a killed task to terminate. */
/* ------------------------------------ */
static void WaitK( pid_t pid, int timeout, int *status ) {
   double delay;                 /* Time elapsed */
   time_t time1;                 /* Start time */
   time_t time2;                 /* End time */
   volatile task_data *task;     /* Pointer to task data */

/* Check that the PID supplied is positive, otherwise do nothing. */
   if ( pid > (pid_t) 0 ) {

/* Begin a new error reporting context. */
      emsBegin( status );

/* Look up the PID in the active task list. */
      task = LookupTask( pid );

/* If the PID was not found, then report an error. */
      if ( !task ) {
         *status = SAI__ERROR;
         emsSeti( "PID", (int) pid );
         emsRep( "SLV_WAITK_PID",
                    "Process ^PID is not a valid slave process.", status );

/* If the task has not previously been killed, then report an error. */
      } else if ( !task->killed ) {
         *status = SAI__ERROR;
         emsSeti( "PID", (int) pid );
         emsRep( "SLV_WAITK_NO",
                    "Process ^PID has not previously been killed.", status );

/* Otherwise, note the current time and loop until the process
   terminates (as notified by the HandleSIGCHLD signal handler). */
      } else {
         time1 = time( (time_t*) NULL );
#if PRINT
         (void) fprintf(stderr,  "WaitK: Waiting for %s process %ld to terminate.\n",
                        task->detached ? "detached" : "attached",
                        (unsigned long) pid );
         (void) fflush( NULL );
#endif
         while ( 1 ) {

/* Determine how long we have been waiting, if time information is
   available. */
            time2 = time( (time_t*) NULL );
            delay = 0.0;
            if ( ( time1 != (time_t) -1 ) && ( time2 != (time_t) -1 ) ) {
               delay = difftime( time2, time1 );
            }

/* If the task has terminated, quit waiting. */
            if ( task->terminated ) break;

/* If the timeout has been exceeded, report an error and quit waiting. */
            if ( ( timeout > 0 ) && ( delay > (double) timeout ) ) {
               *status = SAI__ERROR;
               emsSeti( "PID", (int) pid );
               emsSeti( "SEC", timeout );
               emsRep( "SLV_WAITK_TIME",
                          "Timed out after ^SEC seconds while waiting for "
                          "process ^PID to terminate.", status );

               break;

/* If we have been waiting for more than 2 seconds, sleep for a second
   before testing again (up to this point we test continuously to
   avoid any unnecessary delays). Always sleep if time information
   isn't available. */
            } else if ( ( delay > 2.0 ) ||
                        ( time1 == (time_t) -1 ) ||
                        ( time2 == (time_t) -1 ) ) {
               (void) sleep( (unsigned) 1 );
            }
         }

/* If the wait succeeded, remove the task from the active task list. */
         if ( *status == SAI__OK ) {
#if PRINT
            (void) fprintf(stderr,  "WaitK: %s process %ld terminated after %g "
                           "seconds.\n",
                           task->detached ? "Detached" : "Attached",
                           (unsigned long) pid, delay );
            (void) fflush( NULL );
#endif
            RemoveTask( task );
         }
      }

/* End the error reporting context. */
      emsEnd( status );
   }
}

/* Initialise facilities to handle task termination and closedown. */
/* --------------------------------------------------------------- */
static void InitClosedown( int *status ) {
   static int init = 0;          /* Initialisation done? */

/* Check the inherited status. */
   if ( *status != SAI__OK ) return;

/* Do nothing if initialisation has already been done. */
   if ( !init ) {

/* Obtain the existing SIGCHLD signal action and store it in a static
   variable.  Add SIGCHLD to the signal mask associated with it (this
   is required by the HandleSIGCHLD function). */
      (void) sigaction( SIGCHLD, NULL, &old_SIGCHLD );
      (void) sigaddset( &old_SIGCHLD.sa_mask, SIGCHLD );

/* Establish a new SIGCHLD action (also stored in a static variable),
   preventing generation of SIGCHLD signals for stopped processes only
   if the original action did so. This new action will handle
   termination of slave processes. */
      new_SIGCHLD.sa_handler = HandleSIGCHLD;
      (void) sigemptyset( &new_SIGCHLD.sa_mask );
      new_SIGCHLD.sa_flags = ( old_SIGCHLD.sa_flags & SA_NOCLDSTOP );
      (void) sigaction( SIGCHLD, &new_SIGCHLD, NULL );

/* Establish an exit function to terminate slave tasks when this
   (master) process terminates (we assume the standard ADAM exit
   handling facilities have been set up, so that "atexit" functions
   are executed even if the termination is not "normal"). */
      atexit( Closedown );

/* Note initialisation has been done. */
      init = 1;
#if PRINT
      (void) fprintf(stderr,  "InitClosedown: Task termination handling "
                     "initialised.\n" );
      (void) fflush( NULL );
#endif
   }
}

/* Initialise the ADAM message system. */
/* ----------------------------------- */
static void InitAMS( int *status ) {
   char master[ 50 ];            /* Buffer for master task name */
   static int init = 0;          /* Initialisation done? */

/* Check the inherited status. */
   if ( *status != SAI__OK ) return;

/* Do nothing if initialisation has already been done. */
   if ( !init ) {

/* If this task is running from a user interface, the ICL_TASK_NAME
   environment variable will be set to indicate this, and the ADAM
   message system will have been initialised to make this task's name
   known to the message system. Otherwise, we must initialise the
   message system here. */
      if ( !getenv( "ICL_TASK_NAME" ) ) {

/* Create a unique name for this task and initialise the ADAM message
   system. */
         (void) sprintf(master, "MASTER_%ld", (unsigned long) getpid() );
         ams_init( master, status );

/* Report any error. */
         if ( *status != SAI__OK ) {
            emsFacer( "MESSAGE", *status );
            emsRep( "SLV_AMS_MSG", "^MESSAGE", status );
            emsSetnc( "NAME", master, EMS__SZTOK );
            emsRep( "SLV_AMS_INIT",
                       "Error initialising message system with task name "
                       "\"^NAME\".", status );
#if PRINT
         } else {
            (void) fprintf(stderr,  "InitAMS: Initialised message system with master "
                           "task name \"%s\".\n", master );
            (void) fflush( NULL );
#endif
         }
#if PRINT
      } else {                   /* ICL_TASK_NAME is set */
         (void) fprintf(stderr,  "InitAMS: Master task name defined by "
                         "\"ICL_TASK_NAME=%s\".\n",
                         getenv( "ICL_TASK_NAME" ) );
         (void) fflush( NULL );
#endif
      }

/* Note if initialisation was carried out successfully. */
      init = ( *status == SAI__OK );
   }
}

/* Load a slave task and wait for loading to complete. */
/* --------------------------------------------------- */
static pid_t LoadW( const char *name, const char *file, int detach,
                    int timeout, int *status ) {

/* Local Variables: */
   char **new_environ;
   char env_buff[ 1024 ];
   double delay;
   int istat;
   int loaded;
   int n;
   int nn;
   int path;
   int wait_stat;
   pid_t pid;
   sigset_t oset;
   sigset_t set;
   time_t time1;
   time_t time2;
   volatile task_data *task;
   int nfd;
   int fd;
   int killed;
   int detached;

/* Initialise. */
   pid = (pid_t) -1;

/* Check the inherited error status. */
   if ( *status != SAI__OK ) return pid;

/* Ensure that task closedown facilities are initialised. */
   InitClosedown( status );

/* Ensure the ADAM message system is initialised. */
   InitAMS( status );

/* If OK, try to establish a path to the slave task, to see if it is
   already loaded. Set "pid" to zero if it is. */
   if ( *status == SAI__OK ) {
      istat = SAI__OK;
      ams_path( (char *) name, &path, &istat );
      loaded = ( istat == SAI__OK );
      if ( loaded ) {
         pid = (pid_t) 0;
#if PRINT
         (void) fprintf(stderr,  "LoadW: Task \"%s\" is already loaded.\n", name );
         (void) fflush( NULL );
      } else {
         (void) fprintf(stderr,  "LoadW: Task \"%s\" is not already loaded.\n", name );
         (void) fflush( NULL );
#endif
      }
   }

/* If it is not loaded, then check if a file name was given. If not,
   report an error. */
   if ( !loaded && ( *status == SAI__OK ) ) {
      if ( !file[ 0 ] ) {
         *status = SAI__ERROR;
         emsSetnc( "NAME", name, EMS__SZTOK );
         emsRep( "SLV_LOAD_NOFL",
                    "Unable to load task \"^NAME\": no executable file name "
                    "given.", status );

/* We will now attempt to load the slave task. To do this, we must set
   some environment variables. However, if we change the environment,
   we will upset the behaviour of the message path which the current
   task may already be using to talk to its master. Therfore we must
   construct a replacement environment for use by the slave (only). */
      } else {

/* Loop to count the number of environment variables set, omitting
   those we are going to set ourselves. */
         for ( nn = n = 0; environ[ n ]; n++ ) {
            if ( strncmp( environ[ n ], "ICL_TASK_NAME=", 14 ) &&
                 strncmp( environ[ n ], "ADAM_TASK_TYPE=", 14 ) ) nn++;
         }

/* Allocate an array of string pointers to accommodate the existing
   environment, plus the new variables, plus a final NULL pointer. */
         new_environ = (char **) malloc( (size_t) ( nn + 3 ) *
                                         sizeof( char * ) );

/* Report an error if we failed to allocate the memory. */
         if ( !new_environ ) {
            *status = SAI__ERROR;
            emsErrno( "MESSAGE", errno );
            emsSeti( "SIZE", ( nn + 3 ) * (int) sizeof( char * ) );
            emsRep( "SLV_LOAD_MEM",
                       "Failed to allocate a block of ^SIZE byte(s) of "
                       "memory: ^MESSAGE.", status );

/* If OK, copy the pointers to the existing environment strings to the
   new array, again omitting the variables we want to set. */
         } else {
            for ( nn = n = 0; environ[ n ]; n++ ) {
               if ( strncmp( environ[ n ], "ICL_TASK_NAME=", 14 ) &&
                    strncmp( environ[ n ], "ADAM_TASK_TYPE=", 14 ) ) {
                  new_environ[ nn++ ] = environ[ n ];
               }
            }

/* Construct the environment variable assignment giving the slave task
   name and add its pointer to the end of the new environment. */
            (void) sprintf( env_buff, "ICL_TASK_NAME=%s", name );
            new_environ[ nn++ ] = env_buff;

/* Add an assignment which causes an ADAM "I-task" to be created and
   terminate the new environment array. */
            new_environ[ nn++ ] = "ADAM_TASK_TYPE=I";
            new_environ[ nn++ ] = NULL;

/* Block SIGCHLD signals from terminating child processes. */
            (void) sigemptyset( &set );
            (void) sigaddset( &set, SIGCHLD );
            (void) sigprocmask( SIG_BLOCK, &set, &oset );

/* Flush all output streams. */
            (void) fflush( NULL );

/* Fork a new process and check for errors. */
            pid = fork();
            if ( pid == (pid_t) -1 ) {
               *status = SAI__ERROR;
               emsErrno( "MESSAGE", errno );
               emsRep( "SLV_LOAD_FORK",
                          "Error while forking a new process: ^MESSAGE.",
                          status );

/* This is the child process. */
            } else if ( !pid ) {

/* If a detached task is required, start a new session (this
   relinquishes the controlling terminal, so we do not subsequently
   receive signals from it). */
               if ( detach ) (void) setsid();

/* Determine the maximum number of open file descriptors and loop to
   set the "close-on-exec" flag for them (except for the standard
   channels, which remain open in the slave task). */
               nfd = sysconf( _SC_OPEN_MAX );
               for ( fd = 0; fd < nfd; fd++ ) {
                  switch ( fd ) {
                  case STDIN_FILENO: case STDOUT_FILENO: case STDERR_FILENO:
                     break;
                  default:
                     (void) fcntl( fd, F_SETFD, FD_CLOEXEC );
                     break;
                  }
               }

/* Replace the "environ" pointer with a pointer to the new environment
   we constructed above. */
               environ = new_environ;

/* Now exec the file whose name is supplied (using the PATH to locate
   it). The new process will inherit the new environment. */
               execlp( file, file, NULL );

/* If the exec fails, quit the process quietly with a distinctive exit
   status. */
               _exit( errno );

/* If this is the parent process, record the current time. */
            } else {
               time1 = time( (time_t*) NULL );

/* Insert the child PID into the active task list. */
               task = NewTask( pid, detach );

/* Unblock SIGCHLD signals. */
               (void) sigprocmask( SIG_UNBLOCK, &set, NULL );

/* Loop while the child process starts, waiting to establish a message
   path to it. */
#if PRINT
               (void) fprintf(stderr,  "LoadW: Waiting for %s task \"%s\" "
                              "(file \"%s\") to load into process %ld.\n",
                              detach ? "detached" : "attached",
                              name, file, (unsigned long) pid );
               (void) fflush( NULL );
#endif

               while ( 1 ) {

/* Attempt to obtain an ADAM message system path to the slave process
   and quit looping if successful. */
                  istat = SAI__OK;
                  ams_path( (char *) name, &path, &istat );
                  if ( istat == SAI__OK ) {
                     task->connected = 1;
#if PRINT
                     (void) fprintf(stderr,  "LoadW: Message connection to %s task "
                                    "\"%s\" (process %ld) established.\n",
                                     detach ? "detached" : "attached",
                                     name, (unsigned long) pid );
                     (void) fflush( NULL );
#endif
                     break;
                  }

/* If not successful, block SIGCHLD signals and then check if the
   child process has terminated without completing the
   connection. Report an appropriate error message if it has. */
                  if ( task->terminated ) {
#if PRINT
                     (void) fprintf(stderr,  "LoadW: %s task \"%s\" (process %d) died "
                                    "while waiting for message connection.\n",
                                    detach ? "Detached" : "Attached",
                                    name, pid );
                     (void) fflush( NULL );
#endif
                     *status = SAI__ERROR;
                     if ( WIFSIGNALED( task->exit_status ) ) {
                        emsSetnc( "FILE", file, EMS__SZTOK );
                        emsSeti( "SIGNAL", WTERMSIG( task->exit_status ) );
                        emsRep( "SLV_LOADW_SIG",
                                   "Child process to execute the file "
                                   "\"^FILE\" was unexpectedly terminated "
                                   "by a signal (no. ^SIGNAL).", status );
                     } else {
                        emsSetnc( "FILE", file, EMS__SZTOK );
                        emsSeti( "EXIT",
                                     WEXITSTATUS( task->exit_status ) );
                        emsErrno( "MESSAGE",
                                     WEXITSTATUS( task->exit_status ) );
                        emsRep( "SLV_LOADW_EXIT",
                                   "Child process to execute the file "
                                   "\"^FILE\" terminated unexpectedly with "
                                   "exit status ^EXIT (^MESSAGE).", status );
                     }

/* Remove the task from the active task list. */
                     RemoveTask( task );

/* Clear the process ID and quit looping. */
                     pid = (pid_t) -1;
                     break;
                  }

/* Check how long we have been waiting to make the connection. If this
   exceeds the timeout period, then report an error. */
                  time2 = time( (time_t*) NULL );
                  delay = 0.0;
                  if ( ( time1 != (time_t) -1 ) && ( time2 != (time_t) -1 ) ) {
                     delay = difftime( time2, time1 );
                  }
                  if ( ( timeout > 0 ) && ( delay > (double) timeout ) ) {
#if PRINT
                     (void) fprintf(stderr,  "LoadW: %s task \"%s\" (process %ld) "
                                    "timed out after %g seconds while waiting "
                                    "for message connection.\n",
                                    detach ? "Detached" : "Attached",
                                    name, (unsigned long) pid, delay );
                     (void) fflush( NULL );
#endif
                     *status = SAI__ERROR;
                     emsSeti( "SEC", timeout );
                     emsSetnc( "FILE", file, EMS__SZTOK );
                     emsSeti( "PID", (int) pid );
                     emsRep( "SLC_LOAD_TIME1",
                                "Timed out ^SEC seconds after loading "
                                "file \"^FILE\" into process ^PID.",
                                status );
                     emsSetnc( "NAME", name, EMS__SZTOK );
                     emsRep( "SLV_LOAD_TIME2",
                                "Failed to establish a message path to new "
                                "task \"^NAME\".", status );

/* Kill the child process and wait for it to terminate. */
                     Kill( pid, status );
                     WaitK( pid, timeout, status );

/* Clear the process ID and quit looping. */
                     pid = (pid_t) -1;
                     break;

/* If we have been waiting more than 2 seconds, sleep for a second
   before trying again (up until then, we loop continuously to avoid
   introducing any unnecessary delay). Always sleep if time
   information is unavailable. */
                  } else if ( ( delay > 2.0 ) ||
                              ( time1 == (time_t) -1 ) ||
                              ( time2 == (time_t) -1 ) ) {
                     (void) sleep( (unsigned) 1 );
                  }
               }
            }

/* Restore the original process signal mask. */
            (void) sigprocmask( SIG_SETMASK, &oset, NULL );

/* Free the memory used for the new environment. */
            free( new_environ );
         }
      }
   }

/* Return the process ID. */
   return pid;
}

/* Close down attached tasks as part of master process termination. */
/* ---------------------------------------------------------------- */
static void Closedown( void ) {
   int head;                     /* Removing head of active task list? */
   int i;                        /* Loop counter for tasks */
   int nkill;                    /* Number of tasks to kill */
   int status = SAI__OK;         /* Inherited status */
   pid_t *pid = NULL;            /* Pointer to PID array */
   sigset_t oset;                /* Old process signal mask */
   sigset_t set;                 /* New process signal mask */
   volatile task_data *next;     /* Pointer to next task's data */
   volatile task_data *task;     /* Pointer to task data */

/* Block SIGCHLD signals so that the active task list may be
   accessed. */
   (void) sigemptyset( &set );
   (void) sigaddset( &set, SIGCHLD );
   (void) sigprocmask( SIG_BLOCK, &set, &oset );

/* Examine each task in the active task list. */
   nkill = 0;
   task = active_list;
   while( task ) {

/* If there are terminated tasks that have not been waited for
   (zombies), we must remove their entries from the list. */
      if ( task->terminated ) {
#if PRINT
         (void) fprintf(stderr,  "Closedown: %s process %ld is a zombie slave.\n",
                        task->detached ? "Detached" : "Attached",
                        (unsigned long) task->pid );
         (void) fflush( NULL );
#endif

/* Before removing a zombie, note if we are removing the task at the
   head of the list and identify the following task in the list. */
         head = ( task == active_list );
         next = task->next;
         RemoveTask( task );

/* Move on to examine the next task in the list. Quit if the list is
   empty or we have arrived back at the head of the list (but exclude
   the case where we have just removed the head of the list, because
   the new head-of-list will be the next task). */
         task = next;
         if ( !active_list ||
              ( ( task == active_list ) && !head ) ) break;

/* Count the number of tasks remaining in the active task list which
   are not detached and have not yet been killed. */
      } else {
         if ( !task->detached && !task->killed ) nkill++;
         task = task->next;
         if ( task == active_list ) break;
      }
   }
#if PRINT
   (void) fprintf(stderr,  "Closedown: %d active task(s) must be closed down.\n",
                  nkill );
   (void) fflush( NULL );
#endif

/* Allocate space for an array of PIDs for the tasks to be closed down. */
   if ( nkill ) {
      pid = (pid_t *) malloc( sizeof( pid_t ) * (size_t) nkill );
      if ( pid ) {

/* Loop through the active task list again and extract and store the
   PIDs of the tasks which are to be closed down. */
         task = active_list;
         nkill = 0;
         while( task ) {
            if ( !task->detached && !task->killed ) pid[ nkill++ ] = task->pid;
            task = task->next;
            if ( task == active_list ) break;
         }
      }
   }

/* Enable "closedown mode", which causes the SIGCHLD signal handler
   (HandleSIGCHLD) to remove tasks from the active task list
   immediately when they terminate, rather than leaving them around as
   zombies. */
   closedown_mode = 1;

/* Un-block handling of SIGCHLD signals. */
   (void) sigprocmask( SIG_UNBLOCK, &set, NULL );

/* Mark the error stack to catch any errors. */
   emsMark();

/* Loop to kill each task. */
   for ( i = 0; i < nkill; i++ ) {
#if PRINT
         (void) fprintf(stderr,  "Closedown: Closing down attached process %ld.\n",
                        (unsigned long) task->pid );
         (void) fflush( NULL );
#endif
      Kill( pid[ i ], &status );
   }

/* Annul any errors and release the error stack. */
   emsAnnul( &status );
   emsRlse();

/* Free the PID array. */
   free( pid );

/* Restore the original process signal mask. */
   (void) sigprocmask( SIG_SETMASK, &oset, NULL );
}

void SlvKillW( pid_t pid, int *status ) {
   emsBegin( status );
   KillW( pid, status );
   if ( *status != SAI__OK ) {
      emsRep( "SLV_KILLW_ERR",
                 "SlvKillW: Error while killing and waiting for a slave "
                 "task to terminate.", status );
   }
   emsEnd( status );
}

/* Kill a slave task and wait (indefinitely) for it to terminate. */
/* -------------------------------------------------------------- */
static void KillW( pid_t pid, int *status ) {

/* Check that the PID supplied is positive, otherwise do nothing. */
   if ( pid > (pid_t) 0 ) {

/* Begin a new error reporting context. */
      emsBegin( status );

/* Kill the task. */
      Kill( pid, status );

/* If successful, wait for it to terminate. */
      if ( *status == SAI__OK ) WaitK( pid, 0, status );

/* End the error reporting context. */
      emsEnd( status );
   }
}

int UserFn( int msg_status, const char *msg_name, int msg_length,
            const char *msg_value, int path, int messid, int *status ) {
   int handled = 0;

   return handled;
}

static int StrCmp( const char *str1, const char *str2 ) {
   char c1;
   char c2;
   int result = 0;
   while ( *str1 && *str2 ) {
      c1 = toupper( *str1 );
      c2 = toupper( *str2 );
      if ( c1 > c2 ) {
         result = 1;
      } else if ( c1 < c2 ) {
         result = -1;
      }
      str1++;
      str2++;
      if ( result ) break;
   }
   if ( !result ) {
      if ( *str1 ) {
         result = 1;
      } else if ( *str2 ) {
         result = -1;
      }
   }
   return result;
}

/* Handle an "inform" message from a task (or simulated by this process). */
/* ---------------------------------------------------------------------- */
static void HandleInform( const char *msg_name,
                          int msg_length, const char *msg_value,
                          int path, int messid,
                          int *status ) {
   int handled;                  /* Message handled by user function? */

/* Variables for Fortran 77 arguments. */
   DECLARE_CHARACTER(MSG_NAME,7); /* Message name for MSG_OUT call */
   DECLARE_CHARACTER(MSG_TEXT,8); /* Message text for MSG_OUT call */
   DECLARE_INTEGER(STATUS);      /* Fortran status */

/* Check the inherited status. */
   if ( *status != SAI__OK ) return;

/* Give the user function a chance to handle the message. */
   handled = UserFn( MESSYS__INFORM, msg_name, msg_length, msg_value,
                     path, messid, status );

/* If it was not handled, set up suitable Fortran 77 arguments and
   output the message via the MSG_OUT routine. */
   if ( !handled && ( *status == SAI__OK ) ) {
      cnf_exprt( "SLV_MSG", MSG_NAME, MSG_NAME_length );
      cnf_exprt( "^MESSAGE", MSG_TEXT, MSG_TEXT_length );
      emsSetnc( "MESSAGE", msg_value, msg_length );
      STATUS = *status;
      F77_CALL(msg_out)( CHARACTER_ARG(MSG_NAME), CHARACTER_ARG(MSG_TEXT),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(MSG_NAME) TRAIL_ARG(MSG_TEXT) );

/* Save the returned status. */
      *status = STATUS;
   }
}


/* Flush pending error messages. */
/* ----------------------------- */
static void ErrFlush( int path, int messid, int *status ) {
   char msg_name[ MSG_NAME_LEN ];
   char name[ EMS__SZPAR + 1 ];
   char text[ 3 + EMS__SZMSG + 1 ];
   int first = 1;
   int name_len;
   int new_status = SAI__OK;
   int text_len;

#if PRINT
   (void) fprintf(stderr,  "ErrFlush: Flushing error messages...\n" );
   (void) fflush( NULL );
#endif

/* Loop to output each error message. */
   while ( new_status == SAI__OK ) {

/* Retrieve the message from EMS. Quit if there are none left. */
      emsEload( name, &name_len, text + 3, &text_len, status );
      if ( *status == SAI__OK ) break;

/* Prefix "!! " to the first message, and "!  " to subsequent ones. */
      (void) strncpy( text, first? "!! " : "!  ", 3 );
      first = 0;

/* Output the message text as if it had been received as part of an
   "inform" message from a slave task. */
      (void) strncpy( msg_name, name, MSG_NAME_LEN );
      HandleInform( msg_name, text_len + 3, text, path, messid, &new_status );
   }
#if PRINT
   (void) fprintf(stderr,  "ErrFlush: Error messages flushed.\n" );
   (void) fflush( NULL );
#endif

/* Return the new status. */
   *status = new_status;
}

static const char *LookupParamIO( const char *parname, const char *assoc ) {

   const char *master = NULL;
   char io[ 2 ];
   int i1 = 0;
   int i2;
   int i;
   int len;
   int nc;
   static char masterpar[ PAR__SZNAM + 1 ];
   char slavepar[ PAR__SZNAM + 1 ];

   len = (int) strlen( assoc );
   while ( i1 < len ) {
      nc = 0;
      (void) sscanf( assoc + i1, "%*[^,]%n", &nc );
      i2 = i1 + nc - 1;

      nc = 0;
      if ( ( 3 == sscanf( assoc + i1, " %15[ABCDEFGHIJKLMNOPQRSTUVWXYZ] "
                                      "%1[<>]"
                                      " %15[ABCDEFGHIJKLMNOPQRSTUVWXYZ] %n",
                                      slavepar, io, masterpar, &nc ) ) &&
           ( nc >= i2 - i1 + 1 ) &&
           ( *io == '<' ) &&
           !StrCmp( parname, slavepar ) ) {

         for ( i = 0; masterpar[ i ]; i++ ) {
            masterpar[ i ] = toupper( masterpar[ i ] );
         }
         master = masterpar;
         break;
      }
      i1 = i2 + 2;
   }
   return master;
}

static param_data *LookupParam( const char *parname, param_data **param_list,
                                int *status ) {
   param_data *p;

   if ( *status != SAI__OK ) return NULL;

   p = *param_list;
   while ( p ) {
      if ( !strcmp( p->name, parname ) ) break;
      p = p->next;
   }

   if ( !p ) {
      p = (param_data *) malloc( sizeof( param_data ) );
      (void) strncpy( p->name, parname, PAR__SZNAM );
      p->name[ PAR__SZNAM ] = '\0';
      p->nreq = 0;
      p->last_value = NULL;
      p->next = *param_list ? ( *param_list )->next : NULL;
      *param_list = p;
   }
   return p;
}

static void HandleParamReq( int path, int messid,
                            int msg_length, const char *msg_value,
                            param_data **param_list, const char *assoc,
                            int *act_status, int *status ) {

   char msg_name[ MSG_NAME_LEN ];
   char parname[ PAR__SZNAM + 1 ];
   char parvalue_copy[ MSG_VAL_LEN + 1 ];
   char suggested[ MSG_VAL_LEN + 1 ];
   const char *master;
   const char *parvalue;
   int handled;
   int i;
   int j;
   int jend;
   param_data *pardata;

   DECLARE_CHARACTER(MASTER,PAR__SZNAM);
   DECLARE_CHARACTER(PARVALUE,MSG_VAL_LEN);
   DECLARE_CHARACTER(SUGGESTED,MSG_VAL_LEN);
   DECLARE_INTEGER(IPAR);
   DECLARE_INTEGER(STATUS);

/* Give the user function a chance to handle the parameter request
   message. */
   handled = UserFn( MESSYS__PARAMREQ, msg_name, msg_length, msg_value,
                     path, messid, status );

/* Otherwise, we handle it ourselves... */
   if ( !handled ) {
      if ( *status == SAI__OK ) {

/* Copy the (null terminated) parameter name from the start of the
   message data and convert it to upper case. */
         for ( i = j = 0; ( i < msg_length ) && msg_value[ i ]; i++ ) {
            if ( j < PAR__SZNAM ) parname[ j++ ] = toupper( msg_value[ i ] );
         }
         parname[ j ] = '\0';

/* Skip over the prompt string, which follows in the message data. */
         i++;
         while ( ( i < msg_length ) && msg_value[ i ] ) i++;

/* Extract the suggested value, which follows, trimmimg off any
   trailing white space. */
         i++;
         for ( jend = j = 0; ( i < msg_length ) && msg_value[ i ]; i++ ) {
            if ( j < MSG_VAL_LEN ) {
               suggested[ j++ ] = msg_value[ i ];
               if ( !isspace( msg_value[ i ] ) ) jend = j;
            }
         }
         suggested[ jend ] = '\0';

/* Look the parameter name up in the parameter list for this action
   and increment the number of requests received for it. */
         pardata = LookupParam( parname, param_list, status );
         if ( *status == SAI__OK ) {
            pardata->nreq++;

/* Look up the slave parameter name in the parameter I/O list to
   obtain the name of the master parameter it is associated with (if
   any). */
            master = LookupParamIO( msg_value, assoc );

/* If there is no association with a master parameter and this is the
   first request for a value, then set the returned value equal to the
   suggested value (equivalent to the user pressing the RETURN key or
   using the "accept" keyword). */
            if ( !master ) {
#if PRINT
               (void) fprintf(stderr,  "HandleParamReq: Slave parameter \"%s\" "
                              "is not associated with a master parameter.\n",
                              parname );
               (void) fflush( NULL );
#endif
               if ( pardata->nreq == 1 ) {
                  parvalue = *suggested ? suggested : "!";

/* If there are error messages pending from the slave task, then this
   is probably a request for a replacement parameter value
   (e.g. because the initial "command line" value was unacceptable),
   so report an additional contextual error message showing the new
   parameter value. */
                  if ( *act_status != SAI__OK ) {
                     emsSetnc( "PARAM", parname, EMS__SZTOK );
                     emsSetnc( "VALUE", parvalue, EMS__SZTOK );
                     emsRep( "SLV_OBEYW_NEW",
                                "Sending parameter value '^PARAM=^VALUE' "
                                "to slave task.",act_status );
                  }

/* If this is the second attempt to obtain a value for this parameter,
   then the last value was probably unacceptable (and we are in danger
   of getting into an infinite loop). Since we have no other value to
   use, return a "null" parameter value and record this as an error in
   the slave task. */
               } else {
                  if ( *act_status == SAI__OK ) *act_status = SAI__ERROR;
                  emsSetnc( "PARAM", parname, EMS__SZTOK );
                  if ( pardata->nreq == 2 ) {
                     emsRep( "SLV_OBEYW_NULL",
                                "Out of values - sending null parameter "
                                "value '^PARAM=!' to slave task.",
                                act_status );
                     parvalue = "!";

/* If we are asked a third time, the slave has rejected our "null"
   value and asked for a replacement. This time, supply an "abort"
   response as a final attempt to avoid an infinite loop. Only really
   naughty tasks will ignore this. */
                  } else {
                     emsRep( "SLV_OBEYW_NULL",
                                "Out of values - sending abort parameter "
                                "value '^PARAM=!!' to slave task.",
                                act_status );
                     parvalue = "!!";
                  }
               }

/* If there is a parameter association, call SUBPAR_FINDPAR to obtain
   the index number of the master parameter name. Check for errors. */
            } else {
#if PRINT
               (void) fprintf(stderr,  "HandleParamReq: Slave parameter \"%s\" "
                              "is associated with master parameter \"%s\".\n",
                              parname, master );
               (void) fflush( NULL );
#endif
               cnf_exprt( (char *) master, MASTER, MASTER_length );
               STATUS = *status;
               F77_CALL(subpar_findpar)( CHARACTER_ARG(MASTER),
                                         INTEGER_ARG(&IPAR),
                                         INTEGER_ARG(&STATUS)
                                         TRAIL_ARG(MASTER) );
               *status = STATUS;

/* We will now try and obtain the master parameter value. If we have
   received a parameter request from the slave for the same (slave)
   parameter previously during the current action, then cancel the
   master task's parameter value. Otherwise we will just get the same
   value as previously. */
               if ( ( *status == SAI__OK ) && ( pardata->nreq > 1 ) ) {
#if PRINT
                  (void) fprintf(stderr,  "HandleParamReq: Cancelling previous "
                                 "master \"%s\" parameter value.\n",
                                 master );
                  (void) fflush( NULL );
#endif
                  STATUS = *status;
                  F77_CALL(subpar_cancl)( INTEGER_ARG(&IPAR),
                                          INTEGER_ARG(&STATUS) );
                  *status = STATUS;
               }

/* If a "suggested" value is available from the slave task, then use
   it to set a dynamic default for the master parameter. */
               if ( ( *status == SAI__OK ) && *suggested ) {
#if PRINT
                  (void) fprintf(stderr,  "HandleParamReq: Setting a dynamic default "
                                 "for the master \"%s\" parameter.\n",
                                 master );
                  (void) fflush( NULL );
#endif
                  STATUS = *status;
                  cnf_exprt( (char *) suggested, SUGGESTED, SUGGESTED_length );
                  F77_CALL(subpar_def0c)( INTEGER_ARG(&IPAR),
                                          CHARACTER_ARG(SUGGESTED),
                                          INTEGER_ARG(&STATUS)
                                          TRAIL_ARG(SUGGESTED) );
                  *status = STATUS;
               }

/* If there are pending error messages from the slave task, flush
   them, because the slave task has obviously recovered and is
   continuing. These messages may be needed by the user in order to
   supply a parameter value. */
               if ( ( *status == SAI__OK ) && ( *act_status != SAI__OK ) ) {
#if PRINT
                  (void) fprintf(stderr,  "HandleParamReq: Flushing pending error "
                                 "messages from slave task.\n" );
                  (void) fflush( NULL );
#endif
                  ErrFlush( path, messid, act_status );
                  *status = *act_status;
               }


/* Mark the error stack and attempt to obtain a value for the master
   parameter. */
               if ( *status == SAI__OK ) {
#if PRINT
                  (void) fprintf(stderr,  "HandleParamReq: Requesting a value for the "
                                 "master \"%s\" parameter.\n", master );
                  (void) fflush( NULL );
#endif
                  emsMark();
                  STATUS = *status;
                  /*                  for ( i=0; i <= PARVALUE_length; i++ ) PARVALUE[i] = ' ';*/
                  F77_CALL(subpar_getname)( INTEGER_ARG(&IPAR),
                                            CHARACTER_ARG(PARVALUE),
                                            INTEGER_ARG(&STATUS)
                                            TRAIL_ARG(PARVALUE) );
                  *status = STATUS;

/* Classify the returned status. If it indicates a "null" state for
   the parameter, then annul any associated error messages and return
   the "!" parameter value to the slave task to indicate that no value
   was available. */
                  if ( *status == PAR__NULL ) {
#if PRINT
                     (void) fprintf(stderr,  "HandleParamReq: Master parameter state "
                                    "is \"null\".\n" );
                     (void) fflush( NULL );
#endif
                     emsAnnul( status );
                     parvalue = "!";

/* If the parameter state is "abort, annul any associated error
   messages and return a "!!" parameter value to the slave task. */
                  } else if ( *status == PAR__ABORT ) {
#if PRINT
                     (void) fprintf(stderr,  "HandleParamReq: Master parameter state "
                                    "is \"abort\".\n" );
                     (void) fflush( NULL );
#endif
                     emsAnnul( status );
                     parvalue = "!!";

/* If a value was obtained successfully, make a copy of it. */
                  } else if ( *status == SAI__OK ) {
                     cnf_imprt( PARVALUE, PARVALUE_length, parvalue_copy );
                     parvalue = parvalue_copy;
                  }

/* Release the error stack. */
                  emsRlse();
               }
            }
         }
      }

/* If there has been no error, send the parameter value to the slave
   task. */
      if ( *status == SAI__OK ) {
#if PRINT
         (void) fprintf(stderr,  "HandleParamReq: Sending parameter reply \"%s=%s\" "
                        "to slave task.\n", parname, parvalue );
            (void) fflush( NULL );
#endif
         ams_reply( path, messid, MESSYS__MESSAGE, MESSYS__PARAMREP, OBEY,
                    msg_name, strlen( parvalue ), (char *) parvalue, status );

/* If an error has occurred, attempt to clean up by sending an "abort"
   reponse to the slave task. */
      } else {
         emsBegin( status );
         parvalue = "!!";
#if PRINT
         (void) fprintf(stderr,  "HandleParamReq: Sending parameter reply \"%s=%s\" "
                        "to slave task.\n", parname, parvalue );
         (void) fflush( NULL );
#endif
         ams_reply( path, messid, MESSYS__MESSAGE, MESSYS__PARAMREP, OBEY,
                    msg_name, strlen( parvalue ), (char *) parvalue, status );
         emsEnd( status );
      }
   }
}

void SlvObeyW( const char *task, const char *action,
               const char *command, const char *assoc, int *status ) {

   int msg_status;
   int msg_context;
   int msg_length;
   char msg_name[ MSG_NAME_LEN ];
   char msg_value[ MSG_VAL_LEN ];
   int i;
   int done;
   int path;
   int messid;
   int istat;
   int started;
   int finished;
   char replyaction[ MSG_NAME_LEN ];
   int rvalue_len;
   int slave_error = 0;
   int handled;
   param_data *param_list = NULL;
   param_data *tmp = NULL;
   const char *master = NULL;
   const char *slave = NULL;
   msg_name[0] = '\0';
   msg_value[0] = '\0';

   ams_path( (char *) task, &path, status );
   if ( *status == MESSYS__PATHOPEN ) *status = SAI__OK;

/* Send the action request to the task. */
   ams_send( path, MESSYS__MESSAGE, SAI__OK, OBEY, (char *) action,
             strlen( command ), (char *) command, &messid, status );
#if PRINT
   (void) fprintf(stderr,  "ObeyW: Sent action request \"%s%s%s\" to task \"%s\".\n",
                  action, *command ? " " : "", command, task );
   (void) fflush( NULL );
#endif

   emsMark();
   istat = SAI__OK;

/* Loop to interpret messages from the task while the action is
   carried out. */
   started = 0;
   finished = 0;
   while ( ( *status == SAI__OK ) && !finished ) {

/* Get the next message from the task. */
      ams_getreply( started ? MESSYS__INFINITE : 20000, path, messid,
                    MSG_NAME_LEN, MSG_VAL_LEN,
                    &msg_status, &msg_context, msg_name, &msg_length,
                    msg_value, status );

/* Deal with any errors in obtaining the message. */
      if ( *status != SAI__OK ) {
         emsFacer( "MESSAGE", *status );
         emsRep( "SLV_OBEYW_MSG",
                    "Error in message from slave task: ^MESSAGE", status );

/* Detect confirmation of the action from the task. */
      } else if ( !started && ( msg_status == DTASK__ACTSTART ) ) {
#if PRINT
         {
            int len = msg_length;
            while ( len >= 0 && ( msg_value[ len - 1 ] == ' ' ) ) len--;
            (void) fprintf(stderr,  "ObeyW: Action \"%.*s%s%.*s\" confirmed by "
                           "task \"%s\".\n",
                           MSG_NAME_LEN, msg_name, len ? " " : "",
                           len, msg_value, task );
            (void) fflush( NULL );
         }
#endif
         if ( istat != SAI__OK ) ErrFlush( path, messid, &istat );
         started = 1;
         (void) UserFn( msg_status, msg_name, msg_length, msg_value,
                        path, messid, status );

/* Detect successful completion of the action by the task. */
      } else if ( started && ( msg_status == DTASK__ACTCOMPLETE ) ) {
#if PRINT
         {
            int len = msg_length;
            while ( len >= 0 && ( msg_value[ len - 1 ] == ' ' ) ) len--;
            (void) fprintf(stderr,  "ObeyW: Action \"%.*s %.*s\" completed by "
                           "task \"%s\".\n",
                           MSG_NAME_LEN, msg_name, len, msg_value, task );
            (void) fflush( NULL );
         }
#endif
         if ( istat != SAI__OK ) ErrFlush( path, messid, &istat );
         finished = 1;
         (void) UserFn( msg_status, msg_name, msg_length, msg_value,
                        path, messid, status );

/* Detect a request for a parameter value. */
      } else if ( msg_status == MESSYS__PARAMREQ ) {
#if PRINT
         (void) fprintf(stderr,  "ObeyW: Request for parameter \"%.*s\" from task "
                        "\"%s\".\n", msg_length, msg_value, task );
         (void) fflush( NULL );
#endif
         HandleParamReq( path, messid, msg_length, msg_value, &param_list,
                         assoc, &istat, status );

/* Handle informational messages. */
      } else if ( msg_status == MESSYS__INFORM ) {

/* Identify possible error messages. */
         if ( ( msg_length > 3 ) &&
              ( !strncmp( msg_value, "!! ", 3 ) ||
                !strncmp( msg_value, "!  ", 3 ) ) ) {
            istat = SAI__ERROR;
            (void) strncpy( msg_value, "   ", 3 );
            emsSetnc( "MESSAGE", msg_value, msg_length );
            emsRep( "SLV_ERR", "^MESSAGE", &istat );

         } else {
            if ( istat != SAI__OK ) ErrFlush( path, messid, &istat );
            HandleInform( msg_name, msg_length, msg_value,
                          path, messid, status );
         }

      } else if ( started && msg_status == MESSYS__SYNC ) {
         if ( istat != SAI__OK ) ErrFlush( path, messid, &istat );
         printf( "SYNC\n" );

         F77_CALL(msg_sync)( INTEGER_ARG(status) );
         ams_reply( path, messid, MESSYS__MESSAGE, MESSYS__SYNCREP,
                    msg_context, replyaction, msg_length, msg_value, status );
      } else if ( ( msg_status != DTASK__ACTSTART ) &&
                  ( msg_status != MESSYS__PARAMREQ ) &&
                  ( msg_status != MESSYS__INFORM ) &&
                  ( msg_status != MESSYS__SYNC ) &&
                  ( msg_status != DTASK__ACTCOMPLETE ) ) {
         *status = msg_status;
         slave_error = 1;
         finished = 1;

      } else {
         *status = SAI__ERROR;
         emsSeti( "STAT", msg_status );
         emsFacer( "MESSAGE", msg_status );
         emsRep( "SLV_OBEYW_EH",
                    "Unexpected message from slave task: message status = "
                    "^STAT (^MESSAGE).", status );
      }

/* If an error has occurred and the action was not acknowledged by the
   task, then report contextual information. */
      if ( *status != SAI__OK ) {
         emsSetnc( "ACTION", action, EMS__SZTOK );
         emsSetnc( "TASK", task, EMS__SZTOK );
         if ( !started ) {
            emsRep( "SLV_OBEYW_NO",
                       "Confirmation of action was not received from slave "
                       "task.", status );
         }
      }
   }

   emsRlse();

   while ( param_list ) {
      tmp = param_list;
      param_list = tmp->next;
      free( tmp->last_value );
      free( tmp );
   }
}

/* FORTRAN 77 Interface. */
/* ===================== */
F77_SUBROUTINE(slv_kill)( INTEGER(PID), INTEGER(STATUS) ) {
   GENPTR_INTEGER(PID)
   GENPTR_INTEGER(STATUS)

   pid_t pid;
   int status;

   status = (int) *STATUS;
   emsBegin( &status );

   pid = (pid_t) *PID;

   Kill( pid, &status );

   if ( status != SAI__OK ) {
      emsRep( "SLV_KILL_ERR",
                 "SLV_KILL: Error while killing a slave task.", &status );
   }
   emsEnd( &status );

   *STATUS = (F77_INTEGER_TYPE) status;
}


F77_SUBROUTINE(slv_waitk)( INTEGER(PID), INTEGER(TIMEOUT), INTEGER(STATUS) ) {
   GENPTR_INTEGER(PID)
   GENPTR_INTEGER(TIMEOUT)
   GENPTR_INTEGER(STATUS)

   pid_t pid;
   int timeout;
   int status;

   status = (int) *STATUS;
   emsBegin( &status );

   pid = (pid_t) *PID;
   timeout = (int) *TIMEOUT;

   WaitK( pid, timeout, &status );

   if ( status != SAI__OK ) {
      emsRep( "SLV_WAITK_ERR",
                 "SLV_WAITK: Error waiting for a slave task to terminate.",
                 &status );
   }
   emsEnd( &status );

   *STATUS = (F77_INTEGER_TYPE) status;
}



F77_SUBROUTINE(slv_killw)( INTEGER(PID), INTEGER(STATUS) ) {
   GENPTR_INTEGER(PID)
   GENPTR_INTEGER(STATUS)

   pid_t pid;
   int status;

   status = (int) *STATUS;
   emsBegin( &status );

   pid = (pid_t) *PID;

   KillW( pid, &status );
   if ( status != SAI__OK ) {
      emsRep( "SLV_KILLW_ERR",
                 "SLV_KILLW: Error while killing and waiting for a slave "
                 "task to terminate.", &status );
   }
   emsEnd( &status );

   *STATUS = (F77_INTEGER_TYPE) status;
}

F77_INTEGER_FUNCTION(slv_obeyw)( CHARACTER(TASK),
                                 CHARACTER(ACTION),
                                 CHARACTER(COMMAND),
                                 CHARACTER(ASSOC),
                                 INTEGER(STATUS)
                                 TRAIL(TASK)
                                 TRAIL(ACTION)
                                 TRAIL(COMMAND)
                                 TRAIL(ASSOC) ) {
   GENPTR_CHARACTER(TASK)
   GENPTR_CHARACTER(ACTION)
   GENPTR_CHARACTER(COMMAND)
   GENPTR_CHARACTER(ASSOC)
   GENPTR_INTEGER(STATUS)

   char *task;
   char *action;
   char *command;
   char *assoc;
   int status;

   status = (int) *STATUS;
   if ( status != SAI__OK ) return 0;

   task = cnf_creim( (char *) TASK, TASK_length );
   action = cnf_creim( (char *) ACTION, ACTION_length );
   command = cnf_creim( (char *) COMMAND, COMMAND_length );
   assoc = cnf_creim( (char *) ASSOC, ASSOC_length );

   SlvObeyW( task, action, command, assoc, &status );

   if ( status != SAI__OK ) {
      emsSetnc( "ACTION", action, ACTION_length );
      emsSetnc( "TASK", task , TASK_length );
      emsRep( "SLV_OBEYW_ERR",
                 "SLV_OBEYW: Error executing the '^ACTION' action in slave "
                 "task '^TASK'.", &status );
   }

   *STATUS = (F77_INTEGER_TYPE) status;
   cnf_free( task );
   cnf_free( action );
   cnf_free( command );
   cnf_free( assoc );
   return 0;
}

void SlvKill( pid_t pid, int *status ) {
   emsBegin( status );
   Kill( pid, status );
   if ( *status != SAI__OK ) {
      emsRep( "SLV_KILL_ERR",
                 "SlvKill: Error while killing a slave task.", status );
   }
   emsEnd( status );
}

/*
 *  PWD: added this function to allow parameters to be reset (some
 *       dynamic variable in KAPPA:DISPLAY where causing problems by
 *       remaining locked.
 */

F77_SUBROUTINE(slv_reset)( CHARACTER(TASK), INTEGER(STATUS) TRAIL(TASK) ) {
   GENPTR_CHARACTER(TASK)
   GENPTR_INTEGER(STATUS)

   char *task;
   int messid;
   int path;
   int status;

   status = (int) *STATUS;
   if ( status != SAI__OK ) return;

   task = cnf_creim( (char *) TASK, TASK_length );

   /* Get the path to the task */
   ams_path( (char *) task, &path, &status );
   if ( status == MESSYS__PATHOPEN ) status = SAI__OK;

   /* Send the CONTROL request to the task. */
   ams_send( path, MESSYS__MESSAGE, SAI__OK, CONTROL, "PAR_RESET",
             1, " ", &messid, &status );

   cnf_free( task );
}


