/*
*+
*  Name:
*     tcltalk.c

*  Purpose:
*     Tcl utility routines for CCDPACK.

*  Language:
*     Starlink C

*  Description:
*     This file contains routines which are used by C functions in CCDPACK
*     which have to communicate with Tcl interpreters.

*  Notes:
*     As currently implemented the Tcl interpreter is run in a separate
*     process and communication is done using pipes.  Because of the
*     limitations of pipes this means that it is not possible to ask
*     for evaluation of arbitrarily long Tcl commands, or to receive
*     arbitrarily long responses from the Tcl interpreter, since this
*     could lead to buffers filling up and deadlock.  Neither of these
*     things should be required, but if they were then the functions
*     here would need to be reimplemented using, say, sockets or files
*     for communication.
*
*     The more straightforward option of running the Tcl interpreter
*     within the same process as the program calling these routines
*     is not suitable, since the *only* way, as far as I can tell,
*     of shutting down the Adam message system is to terminate the
*     process which has started it up.  Since it may be necessary to
*     shut AMS down (for instance, if it is required to run more than
*     one separate Tcl script) this means that we can't do it in the
*     calling process.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-JUL-2000 (MBT):
*        Original version.
*     28-JUL-2000 (MBT):
*        Recoded to run the Tcl interpreter in a separate process.
*     2010-05-06 (TIMJ):
*        Add some const
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <stddef.h>

#include "tcl.h"
#include "sae_par.h"
#include "tcltalk.h"
#include "mers.h"
#include "msg_par.h"
#include "cnf.h"

F77_SUBROUTINE(msg_out)( CHARACTER(MSG_NAME), CHARACTER(MSG_TEXT),
                         INTEGER(STATUS)
                         TRAIL(MSG_NAME) TRAIL(MSG_TEXT) );
F77_SUBROUTINE(ccd1_msg)( CHARACTER(ID), CHARACTER(STRING),
                          INTEGER(STATUS)
                          TRAIL(ID) TRAIL(STRING) );
F77_SUBROUTINE(ccd1_errep)( CHARACTER(ID), CHARACTER(STRING),
                            INTEGER(STATUS)
                            TRAIL(ID) TRAIL(STRING) );

#define BUFLENG 4096


/* Global variables. */
   static char buffer[ BUFLENG ];          /* General purpose buffer */

/* Static functions. */
   static char *ccdTclEval( ccdTcl_Interp *cinterp, const char *cmd, int *status );


   ccdTcl_Interp *ccdTclStart( int *status ) {
/*
*+
*  Name:
*     ccdTclStart

*  Purpose:
*     Start up a Tcl interpreter.

*  Language:
*     Starlink C

*  Description:
*     This function returns a pointer to a ccdTcl_Interp structure, which
*     is the value which should be passed as the 'interp' argument to
*     most of the ccdTcl_* routines.  As currently implemented this is
*     NOT a Tcl_Interp structure (though a possible implementation of
*     the ccdTcl_* routines could use this), so don't use it as one.
*
*     Commands can be executed within the Tcl interpreter thus constructed
*     by calling ccdTclDo or one of the other similar functions.  The
*     correct way to free the resources associated with this pointer
*     is using a call to the ccdTclStop routine.

*  Arguments:
*     status = int *
*        The global status.

*  Return Value:
*     On successful execution, a pointer to a ccdTcl_Interp structure,
*     to be used for subsequent calls to related routines, is returned.
*     If there is an error then the status argument is set, and NULL
*     is returned.

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

/* Local variables. */
      ccdTcl_Interp *cinterp;
      pid_t pid;

/* Check inherited status. */
      if ( *status != SAI__OK ) return NULL;

/* Get storage for the interpreter structure. */
      cinterp = malloc( sizeof( ccdTcl_Interp ) );
      if ( cinterp == NULL ) return NULL;

/* Create two pipes, one for shoving commands down, and one for pulling
   result strings up. */
      if ( pipe( cinterp->downfd ) || pipe( cinterp->upfd ) ) {
         *status = SAI__ERROR;
         errSyser( "ERROR", errno );
         errRep( "CCD_TCL_PROC", "pipe: ^ERROR", status );
         free( cinterp );
         return NULL;
      }

/* Child process, in which the Tcl interpreter will run. */
/* ----------------------------------------------------- */
      if ( ! ( pid = fork() ) ) {
         char *av[ 5 ];
         char *ccddir;
         char sifd[ 12 ];
         char sofd[ 12 ];

/* Close unwanted ends of the pipes. */
         close( cinterp->downfd[ 1 ] );
         close( cinterp->upfd[ 0 ] );

/* Ensure that the remaining file descriptors will stay open over an exec
   call. */
         fcntl( cinterp->downfd[ 0 ], F_SETFD, (long) 0 );
         fcntl( cinterp->upfd[ 1 ], F_SETFD, (long) 0 );

/* Overlay the Tcl interpreter on this process.  If the CCDPACK_DIR
   environment variable is defined then use the ccdwish binary there, else
   try to find one on the path. */
         av[ 1 ] = "-pipes";
         av[ 2 ] = sifd;
         av[ 3 ] = sofd;
         av[ 4 ] = NULL;
         sprintf( sifd, "%d", cinterp->downfd[ 0 ] );
         sprintf( sofd, "%d", cinterp->upfd[ 1 ] );
         ccddir = getenv( "CCDPACK_DIR" );
         if ( ccddir != NULL ) {
            av[ 0 ] = buffer;
            strcpy( buffer, ccddir );
            strcat( buffer, "/ccdwish" );
            execv( buffer, av );
         }
         else {
            av[ 0 ] = "ccdwish";
            execvp( "ccdwish", av );
         }

/* Execution will only continue here if ccdwish could not be executed. */
         *status = SAI__ERROR;
         errRep( "TCL_NOWISH", "Failed to execute ccdwish", status );
         return NULL;
      }

/* Parent process. */
/* --------------- */

/* Check that the fork worked. */
      if ( pid < 0 ) {
         *status = SAI__ERROR;
         msgSetc( "ERROR", strerror( errno ) );
         errRep( "CCD_TCL_PROC", "fork: ^ERROR", status );
         free( cinterp );
         return NULL;
      }

/* Close unwanted ends of pipes. */
      close( cinterp->downfd[ 0 ] );
      close( cinterp->upfd[ 1 ] );

/* Execute the CCDFixConvert procedure.  This prevents confusion between
   the temporary HDS container files created for foreign data files by
   the calling process and those created by the ccdwish child process. */
      ccdTclDo( cinterp, "CCDFixConvert tcl-", status );

/* Return the created interpreter structure. */
      return cinterp;
   }


   static char *ccdTclEval( ccdTcl_Interp *cinterp, const char *cmd, int *status ) {
/*
*+
*  Name:
*     ccdTclEval

*  Purpose:
*     Evaluate a Tcl command.

*  Language:
*     Starlink C

*  Description:
*     This routine executes a command in the Tcl interpreter specified
*     by the cinterp argument, and returns the interpreter's result
*     as a character string.  If the return code from the Tcl
*     interpreter was not TCL_OK, then the status argument will be set.
*
*     It does it by writing the text of the command down the pipe to
*     the child process set up by a previous ccdTclStart call, and then
*     reading the response sent back up the pipe from that process.
*     It also watches for, and outputs appropriately, messages for
*     output via the CCDPACK logging system which may come up the pipe.
*
*     This function is declared static, and so not intended for use
*     by external code.  External routines should use ccdTclDo or one
*     of the other ccdTcl* functions instead.

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     cmd = const char *
*        A string representing a Tcl command.  As currently implemented
*        this probably ought not to be too long; below 4096 characters
*        should be all right.
*     status = int *
*        The global status.

*  Return Value:
*     A pointer to a static character string is returned, which contains
*     the string result of the evaluation.  This will be overwritten by
*     the next call to this routine.

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

/* Local variables. */
      int bytes;
      int ifd = cinterp->upfd[ 0 ];
      int ofd = cinterp->downfd[ 1 ];
      int tclrtn;
      void (*handler)(int);
      char *c;
      static char retbuf[ BUFLENG ];
      DECLARE_CHARACTER( fmsg, MSG__SZMSG );
      DECLARE_CHARACTER( fname, MSG__SZMSG );

/* Check inherited status. */
      if ( *status != SAI__OK ) return NULL;

/* Write the text of the command to execute to the downward pipe, so that
   the child process can execute it.  We ignore a SIGPIPE but check for
   an I/O error, so that the program deals gracefully with a broken pipe. */
      handler = signal( SIGPIPE, SIG_IGN );
      if ( write( ofd, cmd, strlen( cmd ) + 1 ) < 0 ) {
         *status = SAI__ERROR;
         msgSetc( "ERROR", strerror( errno ) );
         errRep( "CCD_TCL_WRITE", "^ERROR", status );
      }
      signal( SIGPIPE, handler );

/* Loop until we get a return status which indicates the command has
   completed (i.e. not one which just requires output through the ADAM
   message system). */
      do {

/* Read the Tcl return status from the upward pipe. */
         tclrtn = -1;
         bytes = read( ifd, &tclrtn, sizeof( int ) );

/* Read the result of the evaluation from the upward pipe. */
         c = retbuf - 1;
         *retbuf = '\0';
         do {
            c++;
            if ( (ptrdiff_t)(c - retbuf) >= BUFLENG ) {
               *status = SAI__ERROR;
               errRep( "CCD_TCL_BUF", "Buffer overflow", status );
               return NULL;
            }
            bytes = read( ifd, c, 1 );

/* If the read failed then we probably caught a signal or the child
   process stopped writing in the middle of a command.  Neither of these
   should happen, so signal an error.  It might be desirable to do
   something smarter than this on receipt of a signal (like try the
   read again) but I don't think that it is a very likely eventuality. */
            if ( bytes != 1 ) {
               strcpy( c, "\n   Tcl communications error\n" );
               c += strlen( c );
               tclrtn = -1;
            }
         } while ( *c != '\0' );

/* If the Tcl return status was CCD_CCDMSG, CCD_CCDLOG or CCD_CCDERR then
   what follows are two strings, separated by a carriage return, to output
   via the ADAM message system. */
         if ( tclrtn == CCD_CCDLOG || tclrtn == CCD_CCDERR ||
              tclrtn == CCD_CCDMSG ) {
            c = index( retbuf, '\n' );
            *(c++) = '\0';
            cnfExprt( retbuf, fname, MSG__SZMSG );
            cnfExprt( c, fmsg, MSG__SZMSG );
            if ( tclrtn == CCD_CCDLOG ) {
               F77_CALL(ccd1_msg)( CHARACTER_ARG(fname), CHARACTER_ARG(fmsg),
                                   INTEGER_ARG(status)
                                   TRAIL_ARG(fname) TRAIL_ARG(fmsg) );
            }
            else if ( tclrtn == CCD_CCDERR ) {
               F77_CALL(ccd1_errep)( CHARACTER_ARG(fname), CHARACTER_ARG(fmsg),
                                     INTEGER_ARG(status)
                                     TRAIL_ARG(fname) TRAIL_ARG(fmsg) );
            }
            else if ( tclrtn == CCD_CCDMSG ) {
               F77_CALL(msg_out)( CHARACTER_ARG(fname), CHARACTER_ARG(fmsg),
                                  INTEGER_ARG(status)
                                  TRAIL_ARG(fname) TRAIL_ARG(fmsg) );
            }
         }
      } while ( tclrtn == CCD_CCDLOG || tclrtn == CCD_CCDERR ||
                tclrtn == CCD_CCDMSG );

/* If the Tcl return status was not TCL_OK, then flag an error and write
   an error report. */
      if ( tclrtn != TCL_OK ) {
         int done = 0;
         char *estart;
         const char *fmt = ( tclrtn == TCL_ERROR ) ? "Tcl error:\n%s"
                                             : "Unexpected Tcl return:\n%s";
         snprintf( buffer, BUFLENG - strlen( fmt ), fmt, retbuf );
         *status = SAI__ERROR;
         for ( estart = buffer; ! done; estart = c + 1 ) {
            for ( c = estart; *c != '\n' && *c != '\0'; c++ );
            if ( *c == '\0' ) done = 1;
            *c = '\0';
            errRep( "CCD_TCL_TCLERR", estart, status );
         }
      }

/* Return the result. */
      return retbuf;
   }


   void ccdTclStop( ccdTcl_Interp *cinterp, int *status ) {
/*
*+
*  Name:
*     ccdTclStop

*  Purpose:
*     Delete a Tcl interpreter.

*  Language:
*     Starlink C

*  Description:
*     This routine should be called to shut down a Tcl interpreter which
*     was started by ccdTclStart.  It ought not to be shut down in any
*     other way.  This routine will attempt to execute even if the
*     status is set.

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     status = int *
*        The global status.

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

/* Local variables. */
      int ifd = cinterp->upfd[ 0 ];
      int ofd = cinterp->downfd[ 1 ];
      void (*handler)(int);

/* Write an 'exit' command to the Tcl interpreter.  This should cause it
   to shut down in an orderly fashion and, in particular, it will allow
   the Adam Message System to tidy up in whatever arcane way it sees fit.
   We ignore SIGPIPE but check for an I/O error, so that the program
   copes gracefully in the case that the pipe is broken. */
      handler = signal( SIGPIPE, SIG_IGN );
      if ( write( ofd, "exit", strlen( "exit" ) + 1 ) < 0 ) {
         if ( *status == SAI__OK ) {
            *status = SAI__ERROR;
         }
         msgSetc( "ERROR", strerror( errno ) );
         errRep( "CCD_TCL_PIPE", "^ERROR", status );
      }

/* Close the pipe communication file descriptors. */
      close( ofd );
      close( ifd );

/* Restore SIGPIPE handling. */
      signal( SIGPIPE, handler );

/* Finally free the memory associated with the interpreter structure itself. */
      free( cinterp );
   }


   void ccdTclDo( ccdTcl_Interp *cinterp, const char *script, int *status ) {
/*
*+
*  Name:
*     ccdTclDo

*  Purpose:
*     Execute a Tcl command and discard the result.

*  Language:
*     Starlink C

*  Description:
*     This function executes an arbitrary string of Tcl commands within
*     the current interpreter and discards the result.  It is better
*     to use this than ccdTclGetC cast to void if the result string
*     may be long, since attempting to pass a long result string up
*     the result pipe may result in a buffer overflow.

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     script = const char *
*        Any string of Tcl commands.
*     status = int *
*        The global status.

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

/* Local variables. */
      char dobuf[ BUFLENG ];

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Construct a command which executes the string, but is guaranteed to
   have a return value small enough not to overflow the buffer. */
      sprintf( dobuf, "llength [ %s ]", script );

/* Execute the command in the Tcl interpreter. */
      (void) ccdTclEval( cinterp, dobuf, status );
   }


   void ccdTclRun( ccdTcl_Interp *cinterp, const char *filename, int *status ) {
/*
*+
*  Name:
*     ccdTclRun

*  Purpose:
*     Execute a Tcl script file.

*  Language:
*     Starlink C

*  Description:
*     This routine causes the script file named by the filename argument
*     to be executed within the running Tcl interpreter.  It will look
*     for the script in the CCDPACK_DIR directory.

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     filename = const char *
*        Name of the Tcl script file to interpret.
*     status = int *
*        The global status.

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

/* Local variables. */
      char *ccddir;

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Construct a command to source the file. */
      ccddir = getenv( "CCDPACK_DIR" );
      if ( ccddir != NULL && index( filename, '/' ) == NULL ) {
         sprintf( buffer, "source %s/%s", ccddir, filename );
      }
      else {
         sprintf( buffer, "source %s", filename );
      }

/* Execute the constructed command. */
      ccdTclDo( cinterp, buffer, status );
   }


   void ccdTclAppC( ccdTcl_Interp *cinterp, const char *name, const char *value,
                    int *status ) {
/*
*+
*  Name:
*     ccdTclAppC

*  Purpose:
*     Append an item to a Tcl list.

*  Language:
*     Starlink C

*  Description:
*     This routine appends an item to a Tcl variable using the lappend
*     command.

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     name = const char *
*        The name of the variable to append to.
*     value = const char *
*        The item to appeand to the variable contents.
*     status = int *
*        The global status.

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

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Construct the command to execute. */
      sprintf( buffer, "lappend %s {%s}", name, value );

/* Execute the command in the Tcl interpreter. */
      ccdTclDo( cinterp, buffer, status );
   }


   void ccdTclSetI( ccdTcl_Interp *cinterp, const char *name, int value,
                    int *status ) {
/*
*+
*  Name:
*     ccdTclSetI

*  Purpose:
*     Set the value of a Tcl variable to an integer.

*  Language:
*     Starlink C

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     name = const char *
*        The name of the variable to set.
*     value = int
*        The value to set the variable to.
*     status = int *
*        The global status.

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

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Construct the command to execute. */
      sprintf( buffer, "set %s %d", name, value );

/* Execute the command in the Tcl interpreter. */
      ccdTclDo( cinterp, buffer, status );
   }


   void ccdTclSetD( ccdTcl_Interp *cinterp, const char *name, double value,
                    int *status ) {
/*
*+
*  Name:
*     ccdTclSetD

*  Purpose:
*     Set the value of a Tcl variable to a double.

*  Language:
*     Starlink C

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     name = const char *
*        The name of the variable to set.
*     value = double
*        The value to set the variable to.
*     status = int *
*        The global status.

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

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Construct the command to execute. */
      sprintf( buffer, "set %s %g", name, value );

/* Execute the command in the Tcl interpreter. */
      ccdTclDo( cinterp, buffer, status );
   }


   void ccdTclSetC( ccdTcl_Interp *cinterp, const char *name, const char *value,
                    int *status ) {
/*
*+
*  Name:
*     ccdTclSetC

*  Purpose:
*     Set the value of a Tcl variable to a string.

*  Language:
*     Starlink C

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     name = const char *
*        The name of the variable to set.
*     value = const char *
*        Pointer to string to set the variable to.
*     status = int *
*        The global status.

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

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Construct the command to execute. */
      sprintf( buffer, "set %s {%s}", name, value );

/* Execute the command in the Tcl interpreter. */
      ccdTclDo( cinterp, buffer, status );
   }


   void ccdTclGetI( ccdTcl_Interp *cinterp, const char *script, int *value,
                    int *status ) {
/*
*+
*  Name:
*     ccdTclGetI

*  Purpose:
*     Evaluate a Tcl expression as an integer value.

*  Language:
*     Starlink C

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     script = const char *
*        A pointer to the text of a Tcl expression to evaluate.
*     value = int *
*        A pointer to the variable in which to store the result;
*     status = int *
*        The global status.

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

/* Local variables. */
      char *result;

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Evaluate the expression in the Tcl interpreter. */
      result = ccdTclEval( cinterp, script, status );
      if ( *status != SAI__OK ) return;

/* Convert the returned result to an integer value. */
      if ( sscanf( result, "%d", value ) != 1 ) {
         *status = SAI__ERROR;
         sprintf( buffer, "Error doing %%d conversion of string '%s'", result );
         errRep( "CCD_TCL_CONV", buffer, status );
      }
   }


   void ccdTclGetD( ccdTcl_Interp *cinterp, const char *script, double *value,
                    int *status ) {
/*
*+
*  Name:
*     ccdTclGetD

*  Purpose:
*     Evaluate a Tcl expression as an double value.

*  Language:
*     Starlink C

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     script = const char *
*        A pointer to the text of a Tcl expression to evaluate.
*     value = double *
*        A pointer to the variable in which to store the result;
*     status = int *
*        The global status.

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

/* Local variables. */
      char *result;

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Evaluate the expression in the Tcl interpreter. */
      result = ccdTclEval( cinterp, script, status );
      if ( *status != SAI__OK ) return;

/* Convert the returned result to a double value. */
      if ( sscanf( result, "%lf", value ) != 1 ) {
         *status = SAI__ERROR;
         sprintf( buffer, "Error doing %%lf conversion of string '%s'",
                  result );
         errRep( "CCD_TCL_CONV", buffer, status );
      }
   }


   char *ccdTclGetC( ccdTcl_Interp *cinterp, const char *script, int *status ) {
/*
*+
*  Name:
*     ccdTclGetC

*  Purpose:
*     Evaluate a Tcl expression as a string value.

*  Language:
*     Starlink C

*  Description:
*     This function executes an arbitrary string within the Tcl
*     interpreter and returns the result string.  Note it should NOT
*     be used if the return string may be long (longer than BUFLENG)
*     since this may cause the read buffer to overflow when the result
*     is passed back up the pipe.

*  Arguments:
*     cinterp = ccdTcl_Interp *
*        The interpreter got from a previous ccdTclStart call.
*     script = const char *
*        A pointer to the text of a Tcl expression to evaluate.
*     status = int *
*        The global status.

*  Return Value:
*     A pointer to the result of the expression.  If there was an error
*     then status is set, and the return value will be set to an empty
*     string.

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

/* Local variables. */
      char *result = "";

/* Check the inherited status. */
      if ( *status != SAI__OK ) return NULL;

/* Evaluate the expression in the Tcl interpreter. */
      result = ccdTclEval( cinterp, script, status );
      if ( *status != SAI__OK ) {
         result = "";
      }

/* Return the result. */
      return result;
   }

/* $Id$ */
