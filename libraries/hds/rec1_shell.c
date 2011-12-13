#if HAVE_CONFIG_H
#  include <config.h>
#endif

#if defined( vms ) || __MINGW32__
void rec1_shell( void ){};       /* This routine not used on VMS and        */
                                 /* Windows systems                         */
#else

/* C include files:                                                         */
/* ===============                                                          */
#include <errno.h>
#include <string.h>

/* Posix include files:                                                     */
/* ===================                                                      */
#include <fcntl.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#if defined( _POSIX2_VERSION )
#include <stdlib.h>
#endif

/* Other include files:                                                     */
/* ===================                                                      */
#include "star/mem.h"
#include "ems.h"                 /* EMS error reporting routines            */
#include "ems_par.h"             /* EMS__ public constants                  */
#include "hds1.h"                /* Global definitions for HDS              */
#include "rec.h"                 /* Public rec_ definitions                 */
#include "rec1.h"                /* Private rec_ definitions                */
#include "dat_err.h"             /* DAT__ error code definitions            */

/* External Variables: */
/* =================== */

/* Need to use _NSGetEnviron() to access environ on Darwin. */
#if HAVE_CRT_EXTERNS_H && HAVE__NSGETENVIRON
#include <crt_externs.h>
#define environ (*_NSGetEnviron())
#else
extern char **environ;
#endif

   void rec1_shell( pid_t *pid, FILE *stream[ 2 ] )
   {
/*+                                                                         */
/* Name:                                                                    */
/*    rec1_shell                                                            */

/* Purpose:                                                                 */
/*    Create a shell process and communication channels (UNIX & POSIX       */
/*    only).                                                                */

/* Invocation:                                                              */
/*    rec1_shell( pid, stream )                                             */

/* Description:                                                             */
/*    This routine forks a process to execute a shell and returns file      */
/*    streams that may be used to send commands to it and to read results   */
/*    from it. The shell used is determined by the HDS_SHELL tuning         */
/*    parameter. If the "sh" shell is specified, and POSIX.2 is available,  */
/*    then an attempt will be made to use the standard POSIX.2 shell.       */
/*    Otherwise, the shell is located following the PATH of the current     */
/*    process.                                                              */

/* Parameters:                                                              */
/*    pid_t *pid                                                            */
/*       Pointer to an integer of type pid_t into which the routine will    */
/*       write the process ID of the child process which is created to run  */
/*       the shell.  It is the caller's responsibility to ensure that the   */
/*       created process terminates correctly and to wait for it (e.g. with */
/*       the waitpid function).                                             */
/*    FILE *stream[ 2 ]                                                     */
/*       Array in which two file streams will be returned; stream[ 0 ] will */
/*       be open for reading and may be used to read output from the shell, */
/*       while stream[ 1 ] will be open for writing and may be used to send */
/*       input (i.e. commands) to the shell. These streams are associated   */
/*       with pipes which are attached to the standard output and standard  */
/*       input of the shell process, respectively. It is the caller's       */
/*       responsibility to close these streams when they are no longer      */
/*       required.                                                          */

/* Returned Value:                                                          */
/*    void                                                                  */

/* Notes:                                                                   */
/*    -  A *pid value of (pid_t) -1 and stream values of NULL will be       */
/*    returned if this routine is called with the global status set, or if  */
/*    it should fail for any reason.                                        */
/*    -  The standard error stream of the shell process is routed to the    */
/*    null device (i.e. error messages from this process are suppressed).   */
/*    -  When sending commands to the shell process, care should be taken   */
/*    that it does not produce large amounts of output before all the input */
/*    commands have been sent. If this occurs, the shell may block, causing */
/*    the process sending input to block as well, thus resulting in         */
/*    deadlock.  The safest approach is to ensure that the shell will not   */
/*    produce output until each batch of input commands is complete.        */
/*    -  This routine is only implemented on UNIX & POSIX systems.          */

/* Copyright:                                                               */
/*    Copyright (C) 1992 Science & Engineering Research Council             */
/*    Copyright (C) 2006 Particle Physics and Astronomy Research Council    */

/*  Licence:                                                                */
/*     This program is free software; you can redistribute it and/or        */
/*     modify it under the terms of the GNU General Public License as       */
/*     published by the Free Software Foundation; either version 2 of       */
/*     the License, or (at your option) any later version.                  */

/*     This program is distributed in the hope that it will be              */
/*     useful, but WITHOUT ANY WARRANTY; without even the implied           */
/*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR              */
/*     PURPOSE. See the GNU General Public License for more details.        */

/*     You should have received a copy of the GNU General Public            */
/*     License along with this program; if not, write to the Free           */
/*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,       */
/*     MA 02110-1301, USA                                                   */

/* Authors:                                                                 */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)                               */
/*    TIMJ: Tim Jenness (JAC, Hawaii)                                       */
/*    {@enter_new_authors_here@}                                            */

/* History:                                                                 */
/*    25-NOV-1992 (RFWS):                                                   */
/*       Original version.                                                  */
/*    27-NOV-1992 (RFWS):                                                   */
/*       Converted to return streams instead of file descriptors.           */
/*    18-DEC-1992 (RFWS):                                                   */
/*       Use the _fork macro.                                               */
/*    1-JUN-1995 (RFWS):                                                    */
/*       Add function prototype for confstr (missing on sun4_Solaris        */
/*       systems).                                                          */
/*    2-JUN-1995 (RFWS):                                                    */
/*       Fixed bug in exec of POSIX.2 shell - the path argument was missing */
/*       the "/sh" on the end.                                              */
/*    9-MAY-1997 (RFWS):                                                    */
/*       Define a new PATH environment variable before running the POSIX.2  */
/*       shell.                                                             */
/*    23-FEB-2006 (TIMJ):                                                   */
/*       use starmem                                                        */
/*    {@enter_further_changes_here@}                                        */

/* Bugs:                                                                    */
/*    {@note_any_bugs_here@}                                                */

/*-                                                                         */

/* Local Variables:                                                         */
      int done;                  /* sh shell invoked yet?                   */
      int nullfd;                /* File descriptor for the null device     */
      int pipein[ 2 ];           /* File descriptors for input pipe         */
      int pipeout[ 2 ];          /* File descriptors for output pipe        */
      int stat_val;              /* Shell process status information        */

#if defined( _POSIX2_VERSION )   /* Local variables for POSIX.2 systems:    */
      char **new_environ;        /* Pointer to new environment array        */
      char **old_environ;        /* Poiner to original environment array    */
      char* new_path;            /* PATH string for finding POSIX.2 shell   */
      int ipath;                 /* Index of PATH environment variable      */
      int n;                     /* No. of environment variables set        */
      size_t lpath_new;          /* Length required for new PATH assignment */
      size_t lpath_old;          /* Length of original PATH assignment      */
      size_t lpath_std;          /* Length required for standard PATH       */
#endif

/* External References:                                                     */
      pid_t _fork( void );       /* Function to fork a new process          */

#if defined( _POSIX2_VERSION ) && !HAVE_DECL_CONFSTR
      size_t confstr( int name,  /* Prototype missing on sun4_Solaris       */
                      char *buf,
                      size_t len );
#endif

/*.                                                                         */

/* Set initial null values for the returned results.                        */
      *pid = (pid_t) -1;
      stream[ 0 ] = NULL;
      stream[ 1 ] = NULL;

/* Check the inherited global status.                                       */
      if ( !_ok( hds_gl_status ) ) return;

/* Initialise to "safe" values.                                             */
      pipein[ 0 ] = pipein[ 1 ] = -1;
      pipeout[ 0 ] = pipeout[ 1 ] = -1;

/* Create a pipe for sending commands to the shell process. If this fails,  */
/* set safe values for the file descriptors and report an error.            */
      if ( pipe( pipein ) == -1 )
      {
         pipein[ 0 ] = pipein[ 1 ] = -1;
         hds_gl_status = DAT__FATAL;
         emsSyser( "MESSAGE", errno );
         emsRep( "REC1_SHELL_1",
                    "Error creating a pipe for sending commands to a shell "
                    "process - ^MESSAGE", &hds_gl_status );
      }

/* Create a pipe for receiving output from the shell process. If this       */
/* fails, set safe values for the file descriptors and report an error.     */
      if ( _ok( hds_gl_status ) )
      {
         if ( pipe( pipeout ) == -1 )
         {
            pipeout[ 0 ] = pipeout[ 1 ] = -1;
            hds_gl_status = DAT__FATAL;
            emsSyser( "MESSAGE", errno );
            emsRep( "REC1_SHELL_2",
                       "Error creating a pipe for receiving output from a "
                       "shell process - ^MESSAGE", &hds_gl_status );
         }
      }
      if ( _ok( hds_gl_status ) )
      {

/* On POSIX.2 systems, the current process environment might be changed     */
/* (depending on which variant of "_fork" gets used below), so save the     */
/* original environment pointer and initialise pointers to allocated memory */
/* that may need to be freed later.                                         */
#if defined( _POSIX2_VERSION )
         old_environ = environ;
         new_environ = (char **) NULL;
         new_path = (char *) NULL;
#endif

/* Fork a new process, checking for errors. Note that we use a macro for    */
/* the forking function to allow vfork to be used for efficiency if         */
/* available.                                                               */
         *pid = _fork( );
         if ( *pid == (pid_t) -1 )
         {
            hds_gl_status = DAT__FATAL;
            emsSyser( "MESSAGE", errno );
            emsRep( "REC1_SHELL_3",
                       "Error creating a child process to execute a shell - "
                       "^MESSAGE", &hds_gl_status );
         }

/* If this is the child process, then close the writing end of the input    */
/* pipe and the reading end of the output pipe. These are not required.     */
         else if ( *pid == (pid_t) 0 )
         {
            (void) close( pipein[ 1 ] );
            (void) close( pipeout[ 0 ] );

/* Redirect standard input to the reading end of the input pipe.            */
            (void) dup2( pipein[ 0 ], STDIN_FILENO );
            (void) close( pipein[ 0 ] );

/* Redirect standard output to the writing end of the output pipe.          */
            (void) dup2( pipeout[ 1 ], STDOUT_FILENO );
            (void) close( pipeout[ 1 ] );

/* We want to ignore error messages produced by the shell process, so open  */
/* a file descriptor on the null device and redirect standard error to this */
/* device.                                                                  */
            nullfd = open( "/dev/null", O_WRONLY, 0 );
            (void) dup2( nullfd, STDERR_FILENO );
            (void) close( nullfd );

/* Invoke the appropriate shell, specifying that commands are to be read    */
/* from standard input. Note that the commands to invoke the shells         */
/* themselves (apart from the standard POSIX.2 shell) are macros, so their  */
/* defaults may be over-ridden by external definitions appropriate to a     */
/* particular system.                                                       */

/* tcsh shell:                                                              */
/* ==========                                                               */
            if ( hds_gl_shell == HDS__TCSHSHELL )
            {
               (void) execlp( _tcsh, _tcsh, "-f", "-s", (char *) NULL );
            }

/* csh shell:                                                               */
/* =========                                                                */
            else if ( hds_gl_shell == HDS__CSHSHELL )
            {
               (void) execlp( _csh, _csh, "-f", "-s", (char *) NULL );
            }

/* sh shell:                                                                */
/* ========                                                                 */
/* Include the "no shell" case, as the "sh" shell is still used in this     */
/* instance to perform wild-card file searching.                            */
            else if ( ( hds_gl_shell == HDS__SHSHELL ) ||
                      ( hds_gl_shell == HDS__NOSHELL ) )
            {
               done = 0;

/* If POSIX.2 is available, then we will attempt to locate and use the      */
/* standard POSIX.2 shell.                                                  */
#if defined( _POSIX2_VERSION )

/* Determine if the PATH string which locates the standard shell is         */
/* available (note the length returned includes a final null, so correct    */
/* for this).                                                               */
               lpath_std = confstr( _CS_PATH, (char *) NULL, (size_t) 0 ) -
                           (size_t) 1;
               if ( lpath_std != (size_t) -1 )
               {

/* Count the number of environment variables set in the "environ" array and */
/* note which element (if any) contains the assignment to the PATH          */
/* variable. Also obtain the length of this assignment string.              */
                  ipath = -1;
                  lpath_old = (size_t) 0;
                  for ( n = 0; environ[ n ] != NULL; n++ )
                  {
                     if ( ( ipath == -1 ) &&
                          !strncmp( (const char *) environ[ n ], "PATH=", 5 ) )
                     {
                        ipath = n;
                        lpath_old = strlen( environ[ n ] );
                     }
                  }

/* Attempt to allocate a new array of environment pointers. Include an      */
/* extra element if no PATH assignment currently exists (which is           */
/* unlikely).                                                               */
                  new_environ = (char **) MEM_MALLOC( sizeof( char * ) *
                                (size_t) ( n + 1 + ( ipath == -1 ) ) );

/* Attempt to allocate memory to hold a new assignment string for the PATH  */
/* variable, containing the concatenation of the standard PATH and the      */
/* original value (if any), with a ":" separator.                           */
                  lpath_new = (size_t) 5 + lpath_std;
                  if ( lpath_old > (size_t) 5 )
                  {
                     lpath_new += lpath_old - (size_t) 4;
                  }
                  new_path = (char *) MEM_MALLOC( lpath_new + (size_t) 1 );

/* Check for success.                                                       */
                  if ( ( new_environ != NULL ) && ( new_path != NULL ) )
                  {

/* Obtain the standard PATH string, using it to construct an environment    */
/* variable assignment. Check for success.                                  */
                     (void) strcpy( new_path, "PATH=" );
                     lpath_std = confstr( _CS_PATH, new_path + 5,
                                          lpath_std + (size_t) 1 ) -
                                 (size_t) 1;
                     if ( lpath_std != (size_t) -1 )
                     {

/* If a non-empty PATH assignment originally existed, append its value to   */
/* the standard PATH, with a ":" separator.                                 */
                        if ( lpath_old > (size_t) 5 ) {
                           new_path[ 5 + (int) lpath_std ] = ':';
                           (void) strcpy( new_path + 5 + (int) lpath_std + 1,
                                  (const char *) ( environ[ ipath ] + 5 ) );
                        }

/* Make a copy of the original environment array in the newly-allocated     */
/* memory. If no previous PATH assignment existed, append the new one.      */
/* Otherwise, replace the original one in the copy.                         */
                        (void) memcpy( (void *) new_environ,
                                       (const void *) environ,
                                       sizeof( char * ) * (size_t) ( n + 1 ) );
                        if ( ipath == -1 )
                        {
                           new_environ[ n++ ] = new_path;
                           new_environ[ n++ ] = (char *) NULL;
                        }
                        else
                        {
                           new_environ[ ipath ] = new_path;
                        }

/* Install the new environment for use.                                     */
                        environ = new_environ;

/* Execute the "sh" shell, specifying that commands are to be read from     */
/* standard input. The new PATH assignment ensures that "execlp" finds the  */
/* standard POSIX.2 shell and that this shell finds other standard POSIX.2  */
/* utilities. The remainder of the PATH allows any other user-defined       */
/* utilities to be located.                                                 */
                        (void) execlp( "sh", "sh", "-s", (char *) NULL );

/* The "execlp" should not return, but if it does, note we do not want to   */
/* try looking for another shell. The child process will then just exit     */
/* (below).                                                                 */
                        done = 1;
                     }
                  }
               }
#endif

/* If POSIX.2 is not available, or if the required path could not be        */
/* obtained, then invoke the sh shell using the current process PATH value. */
               if ( !done )
               {
                  (void) execlp( _sh, _sh, "-s", (char *) NULL );
               }
            }

/* Terminate the child process in case the exec above failed.               */
            _exit( 127 );
         }

/* We are now back (or still) in the parent process. On POSIX.2 systems,   */
/* the process environment which was changed in the child process (above)  */
/* may have affected this (the parent) process if "vfork" was used to      */
/* create the child. We must therefore tidy up by restoring the original   */
/* environment pointer and freeing any allocated memory (this has no       */
/* effect if "vfork" wasn't used).                                         */
#if defined( _POSIX2_VERSION )
         environ = old_environ;
         MEM_FREE( (void *) new_environ );
         MEM_FREE( (void *) new_path );
#endif
      }

/* Close the reading end of the input pipe (this is not required). Check    */
/* for errors. Perform this operation inside a separate error reporting     */
/* environment, since we may be cleaning up after a previous error.         */
      emsBegin( &hds_gl_status );
      if ( close( pipein[ 0 ] ) != 0 )
      {
         hds_gl_status = DAT__FATAL;
         emsSyser( "MESSAGE", errno );
         emsRep( "REC1_SHELL_4",
                    "Error closing (unused) reading end of input pipe "
                    "after creating a shell process - ^MESSAGE",
                    &hds_gl_status );
      }
      emsEnd( &hds_gl_status );

/* Similarly close the writing end of the output pipe.                      */
      emsBegin( &hds_gl_status );
      if ( close( pipeout[ 1 ] ) != 0 )
      {
         hds_gl_status = DAT__FATAL;
         emsSyser( "MESSAGE", errno );
         emsRep( "REC1_SHELL_5",
                    "Error closing (unused) writing end of output pipe "
                    "after creating a shell process - ^MESSAGE",
                    &hds_gl_status );
      }
      emsEnd( &hds_gl_status );

/* Open a file stream on the input pipe.                                    */
      if ( _ok( hds_gl_status ) )
      {
         stream[ 1 ] = fdopen( pipein[ 1 ], "w" );
         if ( stream[ 1 ] == NULL )
         {
            hds_gl_status = DAT__FATAL;
            emsSyser( "MESSAGE", errno );
            emsSeti( "FD", (INT) pipein[ 1 ] );
            emsRep( "REC1_SHELL_6",
                       "Error associating a stream with file descriptor ^FD "
                       "for writing to a shell prrocess - ^MESSAGE",
                       &hds_gl_status );
         }
      }

/* Open a file stream on the output pipe.                                   */
      if ( _ok( hds_gl_status ) )
      {
         stream[ 0 ] = fdopen( pipeout[ 0 ], "r" );
         if ( stream[ 0 ] == NULL )
         {
            hds_gl_status = DAT__FATAL;
            emsSyser( "MESSAGE", errno );
            emsSeti( "FD", (INT) pipeout[ 0 ] );
            emsRep( "REC1_SHELL_7",
                       "Error associating a stream with file descriptor ^FD "
                       "for reading from a shell prrocess - ^MESSAGE",
                       &hds_gl_status );
         }
      }

/* If an error occurred, then close all streams and file descriptors which  */
/* may still be open.                                                       */
      if ( !_ok( hds_gl_status ) )
      {
         (void) fclose( stream[ 0 ] );
         (void) fclose( stream[ 1 ] );
         (void) close( pipeout[ 0 ] );
         (void) close( pipein[ 1 ] );

/* If a child process was created before the error, then kill it and wait   */
/* for it to terminate.                                                     */
         if ( *pid != (pid_t) -1 )
         {
            (void) kill( *pid, SIGKILL );
            (void) waitpid( *pid, &stat_val, 0 );
         }

/* Return null values.                                                      */
         *pid = (pid_t) -1;
         stream[ 0 ] = NULL;
         stream[ 1 ] = NULL;
      }

/* Exit the routine.                                                        */
      return;
   }
#endif
