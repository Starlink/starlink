/*
*+
*  Name:
*     ccdwish

*  Purpose:
*     Main routine for ccdwish shell.

*  Usage:
*     ccdwish [-pipes ifd ofd]

*  Description:
*     This source file contains the main() routine for the ccdwish shell.
*     Ccdwish may be invoked in one of two ways: normally, it will run
*     as a free-standing wish shell, like a normal Tcl wish but with
*     the various extensions added by the CCDPACK system, including
*     the startcl extensions and the ndf object manipulation extensions.
*
*     However, when invoked with the -pipes flag as the first command-
*     line argument, and the subsequent two arguments giving file 
*     descriptors for input and output streams, it will run in pipes mode.
*
*     In this mode a Tcl interpreter is set up, and the executable goes
*     into an infinite loop, reading commands from the incoming (downward)
*     pipe, feeding them to the interpreter, and spitting the results 
*     back through the output (upward) pipe.
*
*     Each incoming command must be terminated by a '\0' character.
*     Once it has been received, it is fed to the Tcl interpreter and
*     executed.  The Tcl exit status is then written to the upwards pipe
*     (as sizeof( int ) consecutive bytes), followed by a string giving
*     the result of the command, terminated by a '\0' character.
*     If the command was successful, this string will be the
*     interpreter result string; however, if the command returned a
*     TCL_ERROR status then the contents of the errorInfo variable
*     will be given (this is the same as the result, but has the
*     stack trace appended to it).
*     The routine then starts listening again.  The correct way to
*     terminate this process is by sending a Tcl 'exit' command down
*     the pipe.
*
*     This -pipes mode of running is only likely to be useful when the
*     interpreter is running as an exec'd child process.

*  Authors:
*     MBT: Mark Taylor (STARLINK)

*  History:
*     10-OCT-2000 (MBT):
*        Initial version.
*-
*/

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "tcl.h"
#include "tk.h"
#include "mers.h"
#include "sae_par.h"

#define BUFLENG 4096

/* Global variables. */

   extern int errno;
   char buffer[ BUFLENG ];          /* General purpose buffer */
   Tcl_AsyncHandler ccd_async;      /* Tcl asynchronous event handler */
   int caught[] = {                 /* Signals caught to make an orderly exit */
      SIGHUP, SIGINT, SIGQUIT, SIGTERM,  /* Likely signals */
      0                                  /* List terminator */
   };



   int ccdEndinterp( ClientData clientData, Tcl_Interp *interp, int code ) {
/*    
*+
*  Name:
*     ccdEndinterp
*
*  Purpose:
*     Asynchronous event handler for Tcl interpreter process.
*
*  Description:
*     This routine should be registered as the asynchronous event
*     handler for use with Tcl interpreters.  If it is invoked
*     (by Tcl_AsyncInvoke, as caused by a call to Tcl_AsyncMark),
*     it will cause the interpreter with which it is associated to
*     exit by executing an 'exit' command.  This will allow the
*     interpreter to shut down in an orderly fashion.
*
*     This routine should not be invoked directly by user code.
*
*  Note: 
*     This doesn't seem to work at all.  Writing an exit instruction
*     ought to cause clean up, but when executed here it doesn't seem
*     to get rid of the adam message relay and its associated
*     monoliths.  It does exit the process, but then the signal alone
*     would do that anyway.  Leave this in place for the moment in
*     case I can work out how to make it work.
*
*  Arguments:
*     interp = Tcl_Interp *
*        A pointer to the Tcl interpreter structure, if a Tcl command has
*        just completed.
*     clientData = ClientData
*        A pointer to the Tcl interpreter structure for which this handler
*        was registered.
*     code = int
*        Return code of some command which has recently been executed,
*        if a Tcl has just completed.
*     
*  Notes:
*     As noted on the Tcl_Async man page, the interp and code arguments
*     will not necessarily have anything useful in them (this will be
*     the case if the hander is invoked while waiting for an X event,
*     which is quite likely).  For this reason, the clientData argument
*     is used to identify the interpreter to be shut down.
*-    
*/

/* Local Variables. */
      int status[ 1 ] = { SAI__OK };
      Tcl_Interp *interpreter = (Tcl_Interp *) clientData;

/* Indicate that we are about to bail out. */
      msgOut( " ", "Process received signal; attempting to shut down cleanly.",
              status );

/* Execute an 'exit' instruction. */
      Tcl_Eval( interpreter, "exit" );
      fprintf( stderr, "Interpreter returned >%s<\n", interpreter->result );

/* The above ought to terminate this process, but if it doesn't, then this
   is a sensible thing to return. */
      exit( 1 );
   }


   void ccdSigtcl( int sig ) {
/*
*+
*  Name:
*     ccdSigtcl
*
*  Purpose:
*     Signal handler for Tcl interpreter process.
*
*  Description:
*     This routine should be registered as the signal handler for
*     signals which will cause the Tcl interpreter process to terminate.
*     It makes sure that the Tcl Asynchronous handler routine
*     registered in the global variable ccd_async (which routine ought
*     to effect a clean exit) will be called at a safe point in the
*     near future.
*
*  Arguments:
*     sig = int
*        The number of the signal which has been caught.
*-
*/
      int status[ 1 ] = { SAI__OK };
      msgSeti( "SIG", sig );
      msgOut( " ", "Tcl interpreter process received signal ^SIG", status );
      Tcl_AsyncMark( ccd_async );
   }


   void ccdPipestcl( int ifd, int ofd ) {
/*
*+
*  Name:
*     ccdPipestcl
*
*  Purpose:
*     Set up a Tcl interpreter to read and write through pipes.
*
*  Description:
*     This routine is designed to be run in a subprocess, and communication
*     is done between it and the parent process using pipes.  It begins
*     by setting up a Tcl interpreter, and then goes into an infinite
*     loop, reading commands from the incoming (downward) pipe, feeding
*     them to the interpreter, and spitting the results back through
*     the outward (upward) pipe.
*
*     Each incoming command must be terminated by a '\0' character.
*     Once it has been received, it is fed to the Tcl interpreter and
*     executed.  The Tcl exit status is then written to the upwards pipe
*     (as sizeof( int ) consecutive bytes), followed by a string giving
*     the result of the command, terminated by a '\0' character.
*     If the command was successful, this string will be the
*     interpreter result string; however, if the command returned a
*     TCL_ERROR status then the contents of the errorInfo variable
*     will be given (this is the same as the result, but has the
*     stack trace appended to it).
*     The routine then starts listening again.  The correct way to
*     terminate this process is by sending a Tcl 'exit' command down
*     the pipe.
*
*  Arguments:
*     ifd = int
*        The file descriptor for the input (downwards) pipe on which
*        Tcl commands are to be read.
*     ofd = int
*        The file descriptor for the output (upwards) pipe to which
*        Tcl exit status and result strings are to be written.
*-
*/

/* Global variables. */
      extern Tcl_AsyncHandler ccd_async;

/* Local variables. */
      Tcl_Interp *interp;
      char *c;
      int len;
      int tclrtn;
      int *sp;
      int status[ 1 ] = { SAI__OK };

/* Create a new Tcl interpreter structure. */
      interp = Tcl_CreateInterp();

/* Initialise the interpreter. */
      Tcl_AppInit( interp );

/* Create an asynchronous event handler which can be used for handling
   signals. */
      ccd_async = Tcl_AsyncCreate( ccdEndinterp, (ClientData) interp );

/* Set up signal handlers so that for the given signals the child process
   will be killed cleanly. */
      for ( sp = caught; *sp != 0; sp++ ) {

/* Only reset the signal handler if signals are not currently being ignored. */
         if ( signal( *sp, SIG_IGN ) != SIG_IGN ) {
            signal( *sp, ccdSigtcl );
         }
      }

/* Go into an indefinite loop reading commands from the downward pipe
   and writing results back to the upward pipe.  The loop will only
   be terminated by sending an 'exit' command down the pipe from the
   parent, which will cause the process to kill itself, so no special
   measures have to be taken to spot this. */
      while ( 1 ) {

/* Get the command to execute from the downward pipe. */
         c = buffer - 1;
         do {
            int bytes;
            if ( ++c >= buffer + BUFLENG ) {
               strcpy( buffer, "Buffer overflow in Tcl process\n" );
               write( ofd, buffer, strlen( buffer ) );
               exit( 1 );
            }
            bytes = read( ifd, c, 1 );

/* If the read failed then we probably caught a signal or the parent process
   stopped writing in the middle of a command.  Either way, bail out. */
            if ( bytes != 1 ) {
               Tcl_Eval( interp, "exit" );
               return;
            }
         } while ( *c != '\0' );

/* Ask the Tcl interpreter to execute the command we have received. */
         tclrtn = Tcl_Eval( interp, buffer );

/* Write the Tcl exit status to the upward pipe. */
         write( ofd, &tclrtn, sizeof( int ) );

/* Get the string to write back - either the interpreter result string,
   or the value of the errorInfo variable as appropriate. */
         c = ( tclrtn == TCL_ERROR )
            ? Tcl_GetVar( interp, "errorInfo", TCL_GLOBAL_ONLY )
            : Tcl_GetStringResult( interp );

/* Write the result of the Tcl command to the upward pipe. */
         write( ofd, c, strlen( c ) + 1 );
      }
   }


   int main( int argc, char **argv ) {
/*
*+
*  Name:
*     main

*  Purpose:
*     Main routine for the CCDWISH shell.
*-
*/
      int ifd;                       /* File descriptor of downward pipe */
      int ofd;                       /* File descriptor of upward pipe */
      char *pslash;                  /* Pointer to last '/' in command name */

/* Set the TCLLIBPATH environment variable to hold the name of the directory
   in which the wish binary is resident.  This will cause the autoloader
   to look there for various library files. */
      strcpy( buffer, argv[ 0 ] );
      pslash = rindex( buffer, '/' );
      if ( pslash == NULL ) {
         getcwd( buffer, BUFLENG );
      }
      else {
         *pslash = '\0';
      }
      setenv( "TCLLIBPATH", buffer, 0 );
      setenv( "ITCL_LIBRARY", buffer, 0 );
      setenv( "ITK_LIBRARY", buffer, 0 );

/* Check whether there are flags.  The only valid flag is '-pipes ifd ofd'.
   If we have something which looks like a flag but is not of this form,
   signal an error and bail out. */
      if ( argc > 1 && *argv[ 1 ] == '-' ) {
         if ( strcmp( argv[ 1 ], "-pipes" ) != 0 ||
              argc != 4 ||
              sscanf( argv[ 2 ], "%d", &ifd ) != 1 ||
              sscanf( argv[ 3 ], "%d", &ofd ) != 1 ) {
            fprintf( stderr, "Usage: %s [-pipes ifd ofd]\n", argv[ 0 ] );
            exit( 1 );
         }

/* We have values for both file descriptors.  Start up a Tcl interpreter
   and loop indefinitely. */
         ccdPipestcl( ifd, ofd );
      }

/* We are not running in pipe mode.  Call Tk_main and run as a normal
   freestanding wish. */
      Tk_Main( argc, argv, Tcl_AppInit );
      return 0;
   }



/* $Id$ */
