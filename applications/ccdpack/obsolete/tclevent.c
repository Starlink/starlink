/*
*+
*  Name:
*     tclevent

*  Purpose:
*     Event handler parallel process to handle Tcl events.

*  Usage:
*     #include "tclevent.h"
*     int tcleventstart( tclevent_t *tev);
*     int tcleventstop( tclevent_t *tev );

*  Description:
*     If time-consuming processing is being done in a Tcl process and it
*     is not convenient to call the Tcl event handler Tcl_DoOneEvent()
*     during the processing, these routines may be used to ensure that
*     Tcl events continue to be handled.  
*
*     The main process should call tcleventstart() before it anticipates 
*     doing time-consuming processing.  This will start a child process
*     which will process pending Tcl events as they come up.  When it
*     comes to the end of its processing it should call tcleventstop(),
*     at which point the child process will finish off processing 
*     any event it is in the middle of, and terminate itself.

*  Return values:
*     Under normal circumstances tcleventstart() and tcleventstop() will
*     return 0.  If there is an error they will return -1.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     2-NOV-2000 (MBT):
*        Original version.

*-
*/

#include <sys/types.h>
#include <unistd.h>
#include <sys/ipc.h>
#include <sys/sem.h>
#include "tcl.h"
#include "tclevent.h"
#include <signal.h>

#include <stdio.h>

   union semun {
      int val;
      struct semid_ds *buf;
      unsigned short int *array;
   };


   int tcleventstart( tclevent_t *tev ) {
      int semid;
      pid_t pid;
      Tcl_Time time0 = { 0L, 0L };
      union semun semarg;

/* Create and initialise the semaphore. */
      if ( ( semid = semget( IPC_PRIVATE, 1, IPC_CREAT | 0666 ) ) < 0 ) {
         return semid;
      }
      semarg.val = 0;
      semctl( semid, 0, SETVAL, semarg );
      
/* Fork the process. */
      if ( ( pid = fork() ) < 0 ) {
         return pid;
      }

/* Parent process: do the event handling. */
      if ( pid > 0 ) {

/* Set up a signal handler to ensure that we die gracefully. */
         signal( 15, SIG_DFL );

/* Loop for as long as the parent is not waiting around. */
         while ( ! semctl( semid, 0, GETZCNT ) ) {

/* Wait for an event.  The parent process is permitted to kill us during
   this call. */
            Tcl_WaitForEvent( &time0 );

/* Set the semaphore indicating that the parent must not kill us now. */
            semarg.val = 1;
            semctl( semid, 0, SETVAL, semarg );

/* Process the pending event. */
            Tcl_DoOneEvent( TCL_TIMER_EVENTS );
            putchar('.');

/* Set the semaphore indicating that the parent may kill us. */
            semarg.val = 0;
            semctl( semid, 0, SETVAL, semarg );
         }

/* The parent has been waiting for us to finish servicing events.  Go 
   to sleep, ready to be killed (we could just exit, but this ensures
   that the process always terminates in the same way, which makes it
   easier to spot any IPC bugs). */
         fprintf( stderr, "lullaby" );
         sleep( 999 );
         exit( 0 );
      }

/* Child process only: return the event handler token to the caller and
   return with success status. */
      tev->semid = semid;
      tev->pid = pid;
      return 0;
   }


   int tcleventstop( tclevent_t *tev ) {
      struct sembuf sops;

/* Wait for the child to be in a killable state. */
      sops.sem_num = 0;
      sops.sem_op = 0;
      sops.sem_flg = 0;
      semop( tev->semid, &sops, 1 );

/* Kill it. */
      kill( tev->pid, 15 );

/* Return with success status. */
      return 0;
   }

/* $Id$ */
