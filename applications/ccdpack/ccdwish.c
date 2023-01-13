/*
*+
*  Name:
*     ccdwish

*  Purpose:
*     Main routines for ccdwish shell.

*  Language:
*     Starlink C

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

*  Usage:
*     ccdwish [-pipes ifd ofd]

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     MBT: Mark Taylor (STARLINK)
*     PDRAPER: Peter W. Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-2000 (MBT):
*        Initial version.
*     1-FEB-2006 (PDRAPER):
*        Removed the use of CCDPACK_DIR as home for Tcl/Tk/Itcl.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <config.h>   /* Local version */

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <stddef.h>

#include "tcl.h"
#include "tk.h"
#include "mers.h"
#include "sae_par.h"
#include "cnf.h"

#define BUFLENG 4096

/* Global variables. */

extern int errno;
static char buffer[ BUFLENG + 1 ];          /* General purpose buffer */
static Tcl_AsyncHandler ccd_async;      /* Tcl asynchronous event handler */
static int caught[] = {                 /* Signals caught to make an orderly exit */
    SIGHUP, SIGINT, SIGQUIT, SIGTERM,  /* Likely signals */
    0                                  /* List terminator */
};
int ccdifd = -1;                 /* Downward pipe file descriptor */
int ccdofd = -1;                 /* Upward pipe file descriptor */


/* Prototypes for local functions. */

static Tcl_AsyncProc ccdEndinterp;
static void ccdSigtcl( int sig );
static void ccdPipestcl( char *path, int ifd, int ofd );

/* Fortran functions for querying the command line */
extern F77_INTEGER_FUNCTION(ccd1_iargc)();
extern F77_SUBROUTINE(ccd1_getarg)
     (INTEGER(k), CHARACTER(argstring) TRAIL(argstring));

extern F77_SUBROUTINE(ccd1_linflt)();

/* Release the memory allocated to my_argv */
#define RELEASE_ARGV                                    \
    if (my_argv != NULL) {                              \
        int n;                                          \
        for (n=0; n<my_argc; n++) free (my_argv[n]);    \
        free (my_argv);                                 \
    }


/* Declare the maximum size of the string which will receive the string
   arguments.  1024 should be large enough, I hope. */
#define MAX_ARG_LEN 1024

/* FC_MAIN can be a macro expanding to the entry point used by the
 * Fortran RTL. When that is true assume all Fortran initialisations are
 * done, otherwise we need to take more care.
 */
#if HAVE_FC_MAIN
   int FC_MAIN() {
#else
   int main( int argc, char *argv[] ) {
#endif
/*
*+
*  Name:
*     main

*  Purpose:
*     Main routine for the CCDWISH shell.

*  Language:
*     Starlink C

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
      int ifd;                       /* File descriptor of downward pipe */
      int ofd;                       /* File descriptor of upward pipe */
      char *ccddir;                  /* CCDPACK_DIR value */
      int my_argc;                   /* Number of arguments */
      char **my_argv;                /* Vector of arguments, [0..my_argc-1] */
      int last_arg;                  /* Index of the last arg, = my_argc-1  */

/* Arguments to Fortran functions */
      DECLARE_INTEGER(k);
      DECLARE_CHARACTER(argstr, MAX_ARG_LEN);

/* If not a Fortran main, pass on the arguments. */
#if ! HAVE_FC_MAIN
      cnfInitRTL( argc, argv );
#endif

/* Tweak floating point behaviour on linux. */
      F77_CALL(ccd1_linflt)();

/* Extract the arguments using Fortran functions iargc and getarg,
   putting them into my_argv[0..my_argc-1] */
      last_arg = F77_CALL(ccd1_iargc)();
      my_argc = last_arg + 1;

      if (( my_argv = (char**) malloc(sizeof(void *)*(my_argc+1)) ) == NULL ) {
          fprintf( stderr, "Can't allocate memory for argv\n");
          return 1;
      }
      for ( k=0; k<my_argc; k++ ) {
          F77_CALL(ccd1_getarg)(INTEGER_ARG(&k),
                                CHARACTER_ARG(argstr) TRAIL_ARG(argstr));
          if (( my_argv[k] = (char*) malloc( MAX_ARG_LEN + 1 )) == NULL ) {
              fprintf( stderr, "Can't allocate (%d bytes) for arg %d\n",
                       MAX_ARG_LEN, k );
              return 1;
          }
          cnfImprt( argstr, MAX_ARG_LEN, my_argv[k] );
      }
      my_argv[my_argc] = 0;     /* terminate argv list conventionally */


/* Check whether there are flags.  The only valid flag is '-pipes ifd ofd'.
   If we have something which looks like a flag but is not of this form,
   signal an error and bail out. */
      if ( my_argc > 1 && *my_argv[ 1 ] == '-' ) {
         if ( strcmp( my_argv[ 1 ], "-pipes" ) != 0 ||
              my_argc != 4 ||
              sscanf( my_argv[ 2 ], "%d", &ifd ) != 1 ||
              sscanf( my_argv[ 3 ], "%d", &ofd ) != 1 ) {
            fprintf( stderr, "Usage: %s [-pipes ifd ofd]\n", my_argv[ 0 ] );
            return 1;
         }

/* Store the values of the pipes; other routines (currently, ccdlog) may
   need to know these for direct communication with the parent. */
         ccdifd = ifd;
         ccdofd = ofd;

/* We have values for both file descriptors.  Start up a Tcl interpreter
   and loop indefinitely. */
         ccdPipestcl( my_argv[0], ifd, ofd );

/* If the previous call returns then the wish did not run properly. */
         fprintf( stderr, "%s failed.\n", my_argv[ 0 ] );

         RELEASE_ARGV;

         return 1;
      }

/* We are not running in pipe mode.  Call Tk_main and run as a normal
   freestanding wish. */
      Tk_Main( my_argc, my_argv, Tcl_AppInit );

      RELEASE_ARGV;

      return 0;
   }


   void ccdPipestcl( char *path, int ifd, int ofd ) {
/*
*+
*  Name:
*     ccdPipestcl

*  Purpose:
*     Set up a Tcl interpreter to read and write through pipes.

*  Language:
*     Starlink C

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

*  Arguments:
*     ifd = int
*        The file descriptor for the input (downwards) pipe on which
*        Tcl commands are to be read.
*     ofd = int
*        The file descriptor for the output (upwards) pipe to which
*        Tcl exit status and result strings are to be written.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Global variables. */
      extern Tcl_AsyncHandler ccd_async;

/* Local variables. */
      Tcl_Interp *interp;
      char *c;
      char *initfile = "ccdwishrc";
      const char *ccdDir;
      int len;
      int tclrtn;
      int *sp;
      int status[ 1 ] = { SAI__OK };

/* Tweak floating point behaviour on linux. */
      F77_CALL(ccd1_linflt)();

/* Find the executable. Also locates the basic libraries. */
      Tcl_FindExecutable( path );

/* Create a new Tcl interpreter structure. */
      interp = Tcl_CreateInterp();

/* Initialise the interpreter. */
      if ( Tcl_AppInit( interp ) != TCL_OK ) {
         fprintf( stderr, "Application initialization failed: %s\n",
                          Tcl_GetStringResult( interp ) );
         return;
      }

/* Source initialisation script. */
      sprintf( buffer, "%s/%s", getenv( "CCDPACK_DIR" ), initfile );
      if ( Tcl_EvalFile( interp, buffer ) != TCL_OK ) {
         fprintf( stderr, "Error while sourcing %s:\n%s\n", initfile,
                          Tcl_GetStringResult( interp ) );
      }

/* Make sure CCDPACK_DIR is on auto_path, so we can pick up scripts
 * using autoloading, allow override of ccdpack_dir and use a baked
 * in version as fallback. */
    ccdDir = Tcl_GetVar( interp, "ccdpack_dir", TCL_GLOBAL_ONLY );
    if ( ccdDir == NULL ) {
        ccdDir = Tcl_GetVar2( interp, "env", "CCDPACK_DIR",
                              TCL_GLOBAL_ONLY );
    }
    if ( ccdDir == NULL ) {
        ccdDir = CCDPACK_DIR;
    }
    sprintf( buffer, "set auto_path [linsert $auto_path 0 %s]", ccdDir );
    if ( Tcl_Eval( interp, buffer ) != TCL_OK ) {
        fprintf( stderr, "Application initialization failed: %s\n",
                 Tcl_GetStringResult( interp ) );
        return;
    }

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
            c++;
            if (  (ptrdiff_t)(c - buffer) >= BUFLENG ) {
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
         c = (char *) ( tclrtn == TCL_ERROR )
            ? Tcl_GetVar( interp, "errorInfo", TCL_GLOBAL_ONLY )
            : Tcl_GetStringResult( interp );

/* Write the result of the Tcl command to the upward pipe. */
         write( ofd, c, strlen( c ) + 1 );
      }
   }


   int ccdSetenv( char *name, char *value, int overwrite ) {
/*
*+
*  Name:
*     ccdSetenv

*  Purpose:
*     Set an environment variable.

*  Language:
*     Starlink C

*  Description:
*     This routine does the same as setenv(3) on some operating systems,
*     but which is absent on Solaris.  It the value of an environment
*     variable.

*  Arguments:
*     name = char *
*        Name of the environment variable to be set.  Need not be static.
*     value = char *
*        Value that the environment variable is to be set to.  Need not
*        be static.
*     overwrite = int
*        If the environment variable name already exists, then the value
*        will only be overwritten if overwrite is non-zero.

*  Return Value:
*     Zero is returned on success, -1 if there was a failure in memory
*     allocation.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

      if ( overwrite || getenv( name ) ) {
         char *buf;
         int leng;
         leng = strlen( name ) + strlen( value ) + 2;
         if ( ( buf = malloc( leng ) ) == NULL ) return -1;
         sprintf( buf, "%s=%s", name, value );
         if ( putenv( buf ) != 0 ) return -1;
      }
      return 0;
   }



   int ccdEndinterp( ClientData clientData, Tcl_Interp *interp, int code ) {
/*
*+
*  Name:
*     ccdEndinterp

*  Purpose:
*     Asynchronous event handler for Tcl interpreter process.

*  Language:
*     Starlink C

*  Description:
*     This routine should be registered as the asynchronous event
*     handler for use with Tcl interpreters.  If it is invoked
*     (by Tcl_AsyncInvoke, as caused by a call to Tcl_AsyncMark),
*     it will cause the interpreter with which it is associated to
*     exit by executing an 'exit' command.  This will allow the
*     interpreter to shut down in an orderly fashion.
*
*     This routine should not be invoked directly by user code.

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

*  Notes:
*     As noted on the Tcl_Async man page, the interp and code arguments
*     will not necessarily have anything useful in them (this will be
*     the case if the hander is invoked while waiting for an X event,
*     which is quite likely).  For this reason, the clientData argument
*     is used to identify the interpreter to be shut down.

*  Note:
*     This doesn't seem to work at all.  Writing an exit instruction
*     ought to cause clean up, but when executed here it doesn't seem
*     to get rid of the adam message relay and its associated
*     monoliths.  It does exit the process, but then the signal alone
*     would do that anyway.  Leave this in place for the moment in
*     case I can work out how to make it work.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

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
      fprintf( stderr, "Interpreter returned >%s<\n", Tcl_GetStringResult(interpreter) );

/* The above ought to terminate this process, but if it doesn't, then this
   is a sensible thing to return. */
      exit( 1 );
   }


   void ccdSigtcl( int sig ) {
/*
*+
*  Name:
*     ccdSigtcl

*  Purpose:
*     Signal handler for Tcl interpreter process.

*  Language:
*     Starlink C

*  Description:
*     This routine should be registered as the signal handler for
*     signals which will cause the Tcl interpreter process to terminate.
*     It makes sure that the Tcl Asynchronous handler routine
*     registered in the global variable ccd_async (which routine ought
*     to effect a clean exit) will be called at a safe point in the
*     near future.

*  Arguments:
*     sig = int
*        The number of the signal which has been caught.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
      int status[ 1 ] = { SAI__OK };
      msgSeti( "SIG", sig );
      msgOut( " ", "Tcl interpreter process received signal ^SIG", status );
      Tcl_AsyncMark( ccd_async );
   }

/* $Id$ */
