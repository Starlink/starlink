/*
*+
*  Name:
*     ONE_SHELL_ECHO

*  Purpose:
*     Interpret shell metacharacters in a string, without file globbing.

*  Description:
*     This routine is intended to expand shell metacharacters within
*     a supplied file name, in the case where the file may not already
*     exist. Any wild card characters within the string are ignored (i.e.
*     there is no file globbing).

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ONE_SHELL_ECHO( FILESPEC, FILENAME, STATUS )

*  Arguments:
*     FILESPEC = CHARACTER (Given)
*        The file specification to be echoed.
*     FILENAME = CHARACTER (Returned)
*        The result of expanding any shell metacharacters (except wild
*        cards) within FILESPEC.
*     STATUS = INTEGER (Given and Returned)
*        A status code as follows
*        -  SAI__OK for success
*        -  ONE__LENGTHERR - Bad parameter length
*        -  ONE__PIPEERR  - Pipe error
*        -  ONE__MALLOCERR - Malloc error
*        -  ONE__EXECERROR - error in fork/exec

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
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*      DSB: David Berry (JAC, UCLan)
*      TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*      7-MAR-2006 (DSB)
*         Original version.
*      14-MAR-2006 (DSB)
*         Strip any trailing newline characters from the string before
*         exporting it as a Fortran string.
*      19-APR-2006 (TIMJ):
*         reap child process to prevent zombies. Close parent file descriptor.
*         use starmem.
*      2011-03-08 (TIMJ):
*         Use strlcpy and strlcat for string copying.
*         Correct exit status handling.

*-
 */

/*  Local Constants: */

#define TRUE 1                /* Standard truth value */
#define FALSE 0               /* Standard false value */
#define DIRECTORY_LEN 256     /* Number of characters allowed for directory */
#define LINE_LEN 512          /* Length of returned line */

#if HAVE_CONFIG_H
#  include <config.h>
#endif

#include <fcntl.h>
#include <stdio.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

/* The following should be defined by unistd.h, but be defensive */
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

#if HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifndef WIFEXITED
#define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#if STDC_HEADERS
#  include <string.h>
#  include <ctype.h>
#endif

#include <errno.h>
#include "ems.h"
#include "f77.h"
#include "one_err.h"
#include "sae_par.h"
#include "star/mem.h"
#include "star/util.h"

F77_SUBROUTINE(one_shell_echo)( CHARACTER(FileSpec), CHARACTER(FileName),
                                INTEGER(Status)
                                TRAIL(FileSpec) TRAIL(FileName) )
{
   GENPTR_CHARACTER(FileSpec) /* Pointer to file specification. Length is   */
			      /* FileSpec_length			    */
   GENPTR_CHARACTER(FileName) /* Pointer to string to return file name in.  */
			      /* Length is FileName_length		    */
   GENPTR_INTEGER(Status)     /* Used to return the Adam Status Code */

/*  Local variables  */
   char *CFileSpec = NULL; /* C version of Fortran FileSpec */
   char *Command;        /* 'echo' command executed */
   char Char;            /* Byte read from pipe */
   char Line[LINE_LEN];  /* String into which line is read */
   int Bytes;            /* Number of bytes read from pipe */
   int Fdptr[2];         /* Array of two integer file descriptors */
   int Ichar;            /* Index into FileName for next character */
   int NameLength;       /* Number of bytes in FileName - FileName_length */
   int NullFd;           /* File descriptor for the null device */
   pid_t STATUS = 0;     /* Local fork STATUS/pid */
   int SpecLength;       /* Number of bytes in FileSpec - FileSpec_length */

/* Start off by checking for good status  */
   if( *Status != SAI__OK ) return;

/* No file specification so nothing to do */
   if( FileSpec == NULL ) return;

/* Set input string lengths to those returned by CNF. */
   CFileSpec = cnfCreim( FileSpec, FileSpec_length );
   SpecLength = strlen(CFileSpec);
   NameLength = FileName_length;

/* Create two file descriptors of a pipe. */
   if( pipe( Fdptr ) < 0 ) {
      *Status = ONE__PIPEERR;
      emsRep( "one_shell_echo", "Error from pipe", Status );

/* If succesful, allocate enough room for the eventual echo command string */
   } else {
      size_t cmdlen = SpecLength + 20;
      Command = (char *) starMalloc( cmdlen );

/* Fdptr[0] can now be used as the reading end of the pipe, and Fdptr[1] as
   the writing end.  Having that, we now fork off a new process to do the
   'echo' command that we will use to give us the filename we want. */
      STATUS = fork();
      if( STATUS < 0 ) {
         *Status = ONE__EXECERROR;
	 emsRep( "one_shell_echo", "Unable to fork", Status );

/* This is the child process in which we will exec `echo'.  Copy t
   the filespec from the FileSpec pointer that we have been passed. */
      } else if( STATUS == 0 ) {
         star_strlcpy( Command, "set -f ; echo ", cmdlen );
         star_strlcat( Command, CFileSpec, cmdlen );

/* Now we arrange things so that the 'echo' command will send its output back
   down our pipe.  We want to redirect our current standard output to the
   writing end of the pipe.  This is a standard UNIX technique. */
         (void) close( Fdptr[0] );  /* Don't need to read from pipe */
         if( Fdptr[ 1 ] != STDOUT_FILENO ) {

/* Close stdout (not generally necessary, but required by Single Unix spec) */
            close( STDOUT_FILENO );
            dup2( Fdptr[ 1 ], STDOUT_FILENO );
            close( Fdptr[ 1 ] );  /* Pipe write fd no longer needed */
         }

/* We're going to ignore any error messages from 'echo', so we do the same
   trick for standard error, this time using a file descriptor opened on the
   null device. This has the effect of nulling any error messages. */
         NullFd = open( "/dev/null", O_WRONLY, 0 );
         (void) close( STDERR_FILENO );
         (void) dup2( NullFd, STDERR_FILENO );
         (void) close( NullFd );

/* And now we exec the "set -f ; echo filespec" command.  This will write the
   echoed string to our pipe, and the process will close down when this is
   completed. */
         execl( "/bin/sh", "sh", "-c", Command, NULL );

/* We shouldn't get here -- exit without calling exit handlers or flushing
   and closing output */
         _exit( errno );

/* This is the parent process, continuing after forking off the 'echo'
   command. We don't need the output fd for the pipe, so we close that. */
      } else {
         (void) close( Fdptr[1] );
	 starFree( Command );
      }
   }

/* Free the temporary buffer */
   if (CFileSpec) cnfFree(CFileSpec);

/* At this point, if status still indicates OK, we have two file descriptors,
   the first of which is that of the reading end of the pipe to which our
   forked 'echo' process is writing the echoed string. A read of zero bytes
   indicates that the entire string has been read. Do nothing if status
   is bad. */
   if( *Status != SAI__OK ) {

   } else if( NameLength < 1 ) {
      *Status = ONE__LENERR;
      emsRep( "one_shell_echo", "Length of name less than 1", Status );

/*  This loop reads a line of characters from the pipe. */
   } else {
      Ichar = 0;
      for (;;) {
         Bytes = read( Fdptr[0], &Char, 1 );
         if( Bytes <= 0 ) break;
         if( Ichar < sizeof(Line) ) FileName[ Ichar++ ] = Char;
      }

/* Remove any newline characters from the end of the string. */
      while( Ichar && FileName[ Ichar - 1 ] == '\n' ) Ichar--;

/* Terminate the string and export as a Fortran string. */
      FileName[ Ichar ] = '\0';
      cnfExprt( FileName, FileName, NameLength );
   }

/* Close the parent pipe */
   close( Fdptr[0] );

/* Harvest the child to prevent zombies */
   if (STATUS != 0) {
     int process_status;
     waitpid( STATUS, &process_status, 0 );
     if ( !WIFEXITED(process_status) || /* Did not exit using _exit */
          WEXITSTATUS(process_status)   /* Exited with bad status */
          ) {
       /* non-normal exit */
       if (*Status == SAI__OK) {
	 *Status = ONE__PIPEERR;
         emsSetnc( "FS", FileSpec, FileSpec_length);
         emsRepf( "ONE_SHELL_ECHO","ONE_SHELL_ECHO: Error from child expanding '^FS'"
                  " (exit status=%d)",
                  Status, WEXITSTATUS(process_status) );
       }
     }
   }

}

