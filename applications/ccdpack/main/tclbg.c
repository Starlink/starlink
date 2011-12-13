/*
*+
*  Name:
*     tclbgcmd

*  Purpose:
*     Allow background handling of Tcl events.

*  Language:
*     Starlink C

*  Description:
*     This routine is supplied so that a C extension to Tcl which may
*     have a long execution time can be executed while Tcl events
*     continue to be dealt with.
*
*     This routine can be used to wrap the declaration of the command,
*     so that instead of calling
*
*        Tcl_CreateObjCommand( interp, "newcmd", NewcmdCmd,
*                              (ClientData) NULL, NULL );
*
*     you would call
*
*        Tcl_CreateObjCommand( interp, "newcmd", tclbgcmd,
*                              (ClientData) NewcmdCmd, NULL );
*
*     i.e. pass a pointer to the Tcl_ObjCmdProc representing the new
*     command as client data to the object command registration routine.
*     Other than that, you can hopefully forget that this routine is
*     involved.
*
*     When the command is invoked, this routine forks; the new object
*     command executes in the child and writes the results back to
*     the parent via a pipe; meanwhile the parent continues to service
*     such Tcl events as come along in the normal way.  There are
*     probably implications from this about what the new object
*     command can and cannot safely do, but I'm not quite sure what
*     they are.

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
*     {enter_new_authors_here}

*  History:
*     2-NOV-2000 (MBT):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "tcl.h"

#define BUFLENG 4096

/* Declarations of local files in Tcl terms ensure that their local
   declarations are correct. */

   Tcl_ObjCmdProc tclbgcmd;
   Tcl_FileProc bgready;


/************************************************************************/
   int tclbgcmd( ClientData clientData, Tcl_Interp *interp, int objc,
                 Tcl_Obj *CONST objv[] ) {
/************************************************************************/
      int bytes;
      int ifd;
      int fds[ 2 ];
      int pidstat;
      int ready;
      int tclrtn;
      pid_t pid;
      char *c;
      char *restext;
      char retbuf[ BUFLENG ];

/* Create a pipe for communication between the parent and child. */
      pipe( fds );

/* Fork. */
      pid = fork();
      if ( pid < 0 ) {
         Tcl_SetObjResult( interp, Tcl_NewStringObj( "IPC error", -1 ) );
         return TCL_ERROR;
      }


/* Child process - runs the object command. */
/* ---------------------------------------- */
      if ( pid == 0 ) {
         int ofd;
         char *restext;

/* Arrange pipes correctly for communication. */
         close( fds[ 0 ] );
         ofd = fds[ 1 ];

/* Execute the object command. */
         tclrtn = ((Tcl_ObjCmdProc *) clientData)( (ClientData) NULL, interp,
                                                   objc, objv );

/* Write the exit status and result string up the pipe. */
         write( ofd, &tclrtn, sizeof( int ) );
         restext = Tcl_GetStringResult( interp );
         write( ofd, restext, strlen( restext ) + 1 );

/* Tidy up and exit this process. */
         close( ofd );
         _exit( 0 );
      }

/* Parent process - processes Tcl events. */
/* -------------------------------------- */

/* Arrange pipes correctly for communication. */
      close( fds[ 1 ] );
      ifd = fds[ 0 ];

/* Set up a filehandler so that a Tcl event will be generated when the
   file is ready to read. */
      ready = 0;
      Tcl_CreateFileHandler( ifd, TCL_READABLE | TCL_EXCEPTION, bgready,
                             (ClientData) &ready );

/* Loop processing events until we find that the child has finished. */
      while ( ! ready ) {
         Tcl_DoOneEvent( 0 );
      }

/* Delete the filehandler. */
      Tcl_DeleteFileHandler( ifd );

/* Read the exit status written by the child up the pipe. */
      tclrtn = -1;
      bytes = read( ifd, &tclrtn, sizeof( int ) );

/* Read the result string written by the child up the pipe. */
      c = retbuf - 1;
      do {
         if ( ++c >= retbuf + BUFLENG ) {
            *c = '\0';
         }
         bytes = read( ifd, c, 1 );
         if ( bytes != 1 ) {
            *c = '\0';
         }
      } while ( *c != '\0' );

/* Close the pipe. */
      close( ifd );

/* Reap the child's exit status. */
      waitpid( pid, &pidstat, 0 );

/* Set the result object and return with the correct status. */
      Tcl_SetObjResult( interp, Tcl_NewStringObj( retbuf, -1 ) );
      return tclrtn;
   }



/************************************************************************/
   void bgready( ClientData clientData, int mask ) {
/************************************************************************/
      int *ready = (int *) clientData;
      *ready = 1;
   }


/************************************************************************/
   void tclupdate( void ) {
/************************************************************************/
      while ( Tcl_DoOneEvent( TCL_ALL_EVENTS | TCL_DONT_WAIT ) ) {
      }
   }




/* $Id$ */
