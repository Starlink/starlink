/*
*+
*                         O N E _ F I N D _ F I L E
*  Name:
*     ONE_FIND_FILE
*
*  Purpose:
*     Returns successive file names that match a file specification.
*
*  Description:
*     This routine is intended to provide some of the faciltes provided
*     on a VAX by the standard VMS routine LIB$FIND_FILE.  It is passed
*     a file specification that can contain wild card characters such as
*     '*', eg '*.*'. On the first call the Context variable should be set
*     to zero, and the routine will return the name of the first file that
*     matches the file specification. On subsequent calls the calling
*     routine should continue to call using the value of Context returned
*     by the previous call, and each call will return the name of the next
*     file that matches the specification. When the last file that matches
*     the specification has been returned, subsequent calls will return
*     a blank string as the file name and an error code (an even value)
*     as the function value. Finally, a call to ONE_FIND_FILE_END with the
*     last returned value of Context will close down any files opened or
*     memory allocated by ONE_FIND_FILE.
*
*  Language:
*     C, designed to be called from Fortran.
*
*  Invocation:
*     FOUND = ONE_FIND_FILE (FILESPEC,LISDIR,FILENAME,CONTEXT,STATUS)
*
*  Arguments:
*     FILESPEC = CHARACTER (Given)
*        The file specification to be matched.  May contain wildcards.
*        Case sensitive.
*     LISDIR = LOGICAL (Given)
*        TRUE if directory contents are to be listed for directories
*        that match the file specification. Should be set to FALSE
*        if matching directory names should be returned without opening the
*        directories themselves. Note that even if true, this routine
*        will not recurse into all subsubdirectories that match. To be
*        more explicit: TRUE is equivalent to 'ls', FALSE is equivalent to
*        'ls -d'. Neither is equivalent to 'find . -name "filespec"'
*     FILENAME = CHARACTER (Returned)
*        The name of a file that matches FILESPEC.
*     CONTEXT = INTEGER (Given and Returned)
*        A variable used to remember the context of
*        a series of calls for the same file specification.
*        Should be set to zero by the caller for the first call
*        for a new specification and the value then returned
*        by this routine should be used in subsequent calls.
*     STATUS = INTEGER (Given and Returned)
*        A status code as follows
*        -  SAI__OK for success
*        -  ONE__NOFILES - No more files found
*        -  ONE__LENGTHERR - Bad parameter length
*        -  ONE__PIPEERR  - Pipe error
*        -  ONE__EXECERROR - Error in fork/exec
*        -  ONE__MALLOCERR - Malloc error
*
*  Returned Value:
*     ONE_FIND_FILE = LOGICAL
*        TRUE if File Found
*        FALSE if error or no more files
*
*  Copyright:
*     Copyright (C) 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 2000, 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
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
*      KS: K. Shortridge (AAO).
*      PDRAPER: P.W. Draper (STARLINK - Durham University)
*      DLT: Dave Terrett (STARLINK)
*      RTP: Roy Platon (STARLINK)
*      NG: Norman Gray (Starlink)
*      TJ: Tim Jenness (JAC, Hawaii)
*
*  Bugs:
*      This routine does not provide all the facilities offered by the
*      original VAX version; it only accepts the first three arguments as
*      listed above, and almost of necessity it uses and expects UNIX syntax
*      for the file specs.  This means that '/usr/users/ks/ *.*' is OK, but
*      '[ks]*.*' is not. Note that '*' and '*.*' will give quite different
*      results under UNIX, whereas under VMS they would be the same. There
*      is no way of specifying recursion; '/usr/user/ks/...' for example
*      is meaningless. Nevertheless, it is hoped that it is close enough in
*      functionality to the VMS original to act as a useable substitute in
*      most cases. It cannot handle specifications that the standard shell
*      (sh) cannot handle in an 'ls' command - there are some variations of
*      the 'ls' command involving complex wildcarding that will cause sh
*      on a SUN to hang, and they will also hang this routine.
*
*      It is not at all clear that the method used here is  in any way
*      the best solution to the problem, and there are a number of possible
*      alternatives that could be tried, including using 'find' rather than
*      'ls', or using routines such as readdir() to search the file system
*      and do any pattern matching in this routine itself. The program as it
*      stands should be regarded (tolerantly!) as an initial attempt and
*      the author would be glad to be sent a better version.
*
*  Algorithm:
*      The routine works by forking off a process that runs an 'ls Filespec'
*      command and reads the output from that process through a pipe. Each
*      line read will normally contain a file name and that is returned.
*      The main complication comes from file specifications such as '*'
*      for which 'ls' will list both normal files and the contents of all
*      directories, including in its output both blank lines and the names
*      of the directories.  The routine traps such directory names and
*      remembers them and prefixes them to the subsequent filenames.
*      Context is used as the address of a structure containing things that
*      need to be remembered across calls, such as file descriptors and
*      the last directory specification.
*
*
*  Notes:
*      This routine returns bad status (ONE__NOFILES) even when the
*      status is not technically bad. In general, the caller should
*      annul this particular status condition before proceeding.
*
*  History:
*      Unknown (KS)
*         Original version. Replaces lib$find_file
*      28-FEB-1992 (PDRAPER)
*         Changed name to find_file from lib$find_file. Problems with
*         gcc compiler and $ in name. Increased command length to 256.
*         Fixed bug comparing Nchars and SpecLength. Added Wait for forked
*         process. Removed extra argument to strncat function (incorrect
*         length used as consequence).
*      01-FEB-1993 (DLT):
*         Void ** to int * as Alpha returns addresses as 64 bits (casting
*         to int * avoids this as compiler options are 32 bit. Fixed \0
*         appending to wrong part of string. (Comments added by PDRAPER).
*      16-MAR-1993 (PDRAPER):
*         Changed to use Starlink CNF macros for enhanced portabilility
*      7-JUL-1993 (PDRAPER):
*         Stopped modification of FileSpec point by ++ operator. Now
*         uses local copy.
*      11-NOV-1993 (PDRAPER):
*         New version of cnf casts input pointer as unsigned int (in
*         line with alpha port). This breaks under cast from pointer
*         to structure (ContextPtr) on ultrix. Added cast to
*         F77_POINTER_TYPE to fix this.
*      15-MAR-1995 (PDRAPER):
*         Run through lint on OSF/1, loads of include files added.
*      15-AUG-1995 (PDRAPER):
*         Changed system call to execl and exit to _exit. These
*         changes are necessary to stop exit handlers being invoked
*         on exit of child process when being run from ICL/TCL using
*         the AMS/MSP systems.
*      21-SEP-2000 (RTP)
*         Imported from Figaro/unix/find_file.c
*         Edited for Odds & Ends Library, changed to Adam return codes
*      17-FEB-2004 (NG):
*         Changed to use execlp() rather than system() (you should use
*         system(3) or fork(2)+execve(2), but not both), and be more
*         cautious about dup'ping file descriptors.  Though I can't pin
*         it down precisely, the previous version seemed to be getting
*         _very_ confused, and apparently trying to read in its own
*         output.  Waits for the child in find_file_end rather than
*         immediately after the fork -- the previous version probably
*         worked only when the ls output was smaller than the default
*         pipe buffer, so that the child process could run to completion
*         before the parent even started reading from the pipe.
*         Required adding an extra pid field to ContextStruct.  Uses
*         <config.h>, containing results of autoconf tests.
*      28-AUG-2004 (TJ):
*         Merge Norman's fixes to SST with Roy's fixes for ONE and
*         attempt to sync all extant find_file.c variants into one.
*         Use CNF to do fortran integer to C pointer mapping. Can not
*         execlp "ls" directly since that will not expand the wildcard pattern.
*      21-OCT-2004 (PDRAPER):
*         Made the Command string dynamic (was 256, which is too
*         small for the current NDF_FORMATS_IN). Increased length
*         of returned line to 512, in line with NDG practice.
*      19-SEP-2005 (TIMJ):
*         Removed unused defines for isspace and memcpy.
*         Use cnfExprt rather than hand-rolled padding of strings
*      19-APR-2006 (TIMJ):
*         Use starmem

*-
 */

/* Note that return codes used to be chosen as much as possible to duplicate
   the effect of the VMS routine that this is based on. This is no longer
   the case since it now seems more sensible for people to use MSG generated
   ADAM codes. This will only bite if people are hard-wiring old constants
   into code.
*/

/*  Constants used. */

#define TRUE 1                /* Standard truth value */
#define FALSE 0               /* Standard false value */
#define DIRECTORY_LEN 256     /* Number of characters allowed for directory */
#define LINE_LEN 512          /* Length of returned line */

/*  Structures used:
 *
 *  A ContextStruct is used to maintain the context of a given series of
 *  calls to this routine.  It contains the two file descriptors used
 *  for the pipe that reads from the 'ls' process, and the string used
 *  to remember any directory specification output by the 'ls' process,
 *  when a number of directories are being listed.  One of these is
 *  allocated when Context is passed as zero, and its address is then
 *  the quantity returned in Context.
 */

typedef struct ContextStruct {
    int Fds[2];                 /* Input and output file descriptors
                                   for the pipe */
    char Directory[DIRECTORY_LEN];
                                /* Last directory spec output by 'ls'
                                   process */
    int ls_pid;                 /* pid of child process */
} ContextStruct;

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

F77_INTEGER_FUNCTION(one_find_file)( CHARACTER(FileSpec), LOGICAL(LisDir),
                        CHARACTER(FileName), POINTER(Context), INTEGER(Status)
                         TRAIL(FileSpec) TRAIL(FileName) )
{
   GENPTR_CHARACTER(FileSpec) /* Pointer to file specification. Length is   */
			      /* FileSpec_length			    */
   GENPTR_LOGICAL(LisDir)   /* Are we opening directories? */
   GENPTR_CHARACTER(FileName) /* Pointer to string to return file name in.  */
			      /* Length is FileName_length		    */
   GENPTR_POINTER(Context)    /* Used to remember context of search	    */
   GENPTR_INTEGER(Status)     /* Used to return the Adam Status Code */

   /*  Local variables  */

   int SpecLength;                /* Number of bytes in FileSpec - FileSpec_length*/
   int NameLength;                /* Number of bytes in FileName - FileName_length*/

   int Bytes;            /* Number of bytes read from pipe */
   char Char;            /* Byte read from pipe */
   int ColonIndex;       /* Index in Line of last colon */
   ContextStruct *ContextPtr; /* Pointer to current context information */
   char *Command;        /* 'ls' command executed */
   int *Fdptr;           /* Pointer to array of two integer file descriptors */
   int GotFileName;      /* Used to control loop reading lines from 'ls' */
   int Ichar;            /* Index into FileName for next character */
   char LastChar;        /* Last non-blank char read in line */
   char Line[LINE_LEN];  /* String into which line is read */
   int Nchars;           /* Maximum number of chars that can be copied */
   int NullFd;           /* File descriptor for the null device */
   char *PointFSpec;     /* Local copy of FileSpec          */
   int STATUS;           /* Local fork STATUS */

   /*  Start off by checking for good status  */

   if ( *Status != SAI__OK ) return FALSE;

   ColonIndex = 0;

   /*  No file specification so nothing to do */
   if ( FileSpec == NULL ) return FALSE;

   /*
      Set input string lengths to those returned by CNF.
   */

   SpecLength = FileSpec_length;
   NameLength = FileName_length;
   PointFSpec = FileSpec;
   /*
      Set return value to File not found.
   */
   GotFileName = FALSE;

   /*  The first time through for a new file specification, Context will
    *  be zero.  In this case we allocate ourselves enough space for the
    *  context structure we will use and use Context to hold its address.
    *  We initialise the directory specification in the context to null.
    */

   if (*Context == 0) {

       /* FIXME: explain why malloc 2*sizeof()? */
      ContextPtr = (ContextStruct *) cnfMalloc(2 * sizeof(ContextStruct));
      if ( ContextPtr == NULL ) {
         *Status = ONE__MALLOCERR;
	 emsRep("one_find_file","Unable to allocate memory for context struct",
		Status);
      } else {
         Fdptr = ContextPtr->Fds;
         ContextPtr->Directory[0] = '\0';
         ContextPtr->ls_pid = 0; /* safe value, tested for in find_file_end */

         /*  Fdptr now points to an array of two ints, which we will use
          *  as the two file descriptors of a pipe which we now create.
          */

         if (pipe(Fdptr) < 0) {
            *Status = ONE__PIPEERR;
	    emsRep("one_find_file","Error from pipe",Status);
         } else {

            /* Allocate enough room for the eventual ls command string */
            Command = (char *) starMalloc( SpecLength + 10 );

            /*  Fdptr[0] can now be used as the reading end of the pipe,
             *  and Fdptr[1] as the writing end.  Having that, we now
             *  fork off a new process to do the 'ls' command that we
             *  will use to give us the filenames we want.
             */

            STATUS = fork();
            if (STATUS < 0) {

                /* error: unable to fork */
                *Status = ONE__EXECERROR;
		emsRep("one_find_file","Unable to fork", Status);

            } else if (STATUS == 0) {

                /*  This is the child process in which we will exec
                 *  `ls'.  Copy the filespec from the PointFSpec
                 *  pointer that we have been passed, remembering that
                 *  this is a string from a Fortran program, and so is
                 *  blank padded rather than null terminated.
                 */

                /* SpecLength is the total length of FileSpec,
                   including trailing blanks */
                /* Add -d to ls if we are not listing directory contents */

	        (void) strcpy( Command, "ls " );
		if ( F77_ISFALSE( *LisDir ) ) {
		  strcat( Command, "-d " );
		}
	        (void) strncat( Command, FileSpec, SpecLength );

               /*  Now we arrange things so that the 'ls' command will
                *  send its output back down our pipe.  We want to redirect
                *  our current standard output to the writing end of the
                *  pipe.  This is a standard UNIX technique.
                */

               (void) close(Fdptr[0]);  /* Don't need to read from pipe */
               if (Fdptr[1] != STDOUT_FILENO) {
                   /* Close stdout (not generally necessary, but
                      required by Single Unix spec) */
                   close(STDOUT_FILENO);
                   dup2(Fdptr[1], STDOUT_FILENO);
                   close(Fdptr[1]);  /* Pipe write fd no longer needed */
               }

               /*  We're going to ignore any error messages from 'ls', so
                *  we do the same trick for standard error, this time using
                *  a file descriptor opened on the null device. This has the
                *  effect of nulling any error messages.
                */

	       NullFd = open("/dev/null",O_WRONLY,0);
               (void) close(STDERR_FILENO);
               (void) dup2(NullFd, STDERR_FILENO);
               (void) close(NullFd);

               /*  And now we exec the "ls filespec" command.  This
                *  will write the set of files to our pipe, and the process
                *  will close down when all the files are listed.
		*  Note that we do not call ls directly since we require
		*  the shell to expand our wildcard string.
                */

	       execl( "/bin/sh", "sh", "-c", Command, NULL);

               /* We shouldn't get here -- exit without calling exit
                  handlers or flushing and closing output */
               _exit (errno);

            } else {
                /* Save the pid of the child */
                ContextPtr->ls_pid = STATUS;

               /*  This is the parent process, continuing after forking off
                *  the 'ls' command. Here we set Context to be the address
                *  of the pair of file descriptors being used for the pipe.
                *  and carry on. We don't need the output fd for the pipe,
                *  so we close that.
                */

               (void) close (Fdptr[1]);
	       *Context = cnfFptr( ContextPtr );
	       starFree( Command );
            }
         }
      }
   }

   /*  At this point, if status still indicates OK, Context points to
    *  a context structure containing a directory specification and two
    *  file descriptors, the first of which is that of the reading end of
    *  the pipe to which our forked 'ls' process is writing a set of file
    *  names. Each name will be terminated by a newline, so we just read
    *  a character at a time from the pipe until such a newline indicates
    *  that we have a complete filename. A read of zero bytes indicates
    *  that all the filenames have been listed.
    */

   ContextPtr = (ContextStruct *) cnfCptr( *Context );
   Fdptr = ContextPtr->Fds;
   if (*Status != SAI__OK) {
       /* Do nothing */
   } else if (NameLength < 1) {
      *Status = ONE__LENERR;
      emsRep("one_find_file","Length of name less than 1",
	     Status);
   } else {

      /*  The following loop continues until we have a filename read from the
       *  pipe or until the pipe is empty.  'ls' can produce lines that are not
       *  simple filenames; if multiple directories are being listed, it will
       *  separate them with blank lines and will precede each with the
       *  directory name, ending with a colon.  These have to be trapped.
       */

      GotFileName = FALSE;
      while (!GotFileName) {

         /*  This loop reads a line of characters from the pipe. */

         Ichar = 0;
         LastChar = ' ';
         for (;;) {
            Bytes = read (Fdptr[0],&Char,1);
            if (Bytes <= 0) {
               *Status = ONE__NOFILES;
	       emsRep("one_find_file","No more files found for this search",
		      Status);
               break;
            }
            if (Char == '\n') {
               if (Ichar < sizeof(Line)) Line[Ichar] = '\0';
               else Line[sizeof(Line) - 1] = '\0';
               break;
            } else {
               if (Char != ' ') LastChar = Char;
               if (Char == ':') ColonIndex = Ichar;
               if (Ichar < sizeof(Line)) Line[Ichar++] = Char;
            }
         }

         /*  At the end of the loop, LastChar contains the last non-blank
          *  character in the line.  If this is blank, we have a blank line
          *  which we ignore.  If it was ':' we have a directory name that
          *  we should remember, but we still need a filename.  If status is
          *  bad, we have reached the end of the 'ls' output.
          */

         if (*Status != SAI__OK) break;
         if (LastChar != ' ') {
            if (LastChar == ':') {

               /*  It was a directory spec, so save it in the currrent
                *  context -  replacing the ':' with a '/' and terminating
                *  it at that point with a null. ColonIndex will tell us
                *  where the last colon was.
                */

               if (ColonIndex < (sizeof(Line) - 1)) {
                  Line[ColonIndex] = '/';
                  Line[ColonIndex + 1] = '\0';
               }
               (void) strncpy(ContextPtr->Directory,Line,DIRECTORY_LEN);
               ContextPtr->Directory[DIRECTORY_LEN - 1] = '\0';

            } else {

               /*  It was an ordinary file name, so combine it with any
                *  directory spec we may have in the context, and return it
                *  in FileName, making sure it's properly terminated and being
                *  very careful not to excede its length.
                */

               GotFileName = TRUE;
               (void) strncpy(FileName,ContextPtr->Directory,NameLength);
               FileName[NameLength - 1] = '\0';
               Nchars = NameLength - strlen(FileName);
               (void) strncat(FileName,Line,Nchars); /* remove NameLength   */
						     /*	as third argument   */
						     /*	in this list	    */
               FileName[NameLength - 1] = '\0';

               /*  And, remembering that this is a Fortran character string,
                *  blank pad it properly. Overwriting the last NULL.
                */

	       cnfExprt( FileName, FileName, NameLength );
            }
         }
      }

   }

   return (GotFileName);
}



/*                    O N E _ F I N D _ F I L E _ E N D
*+
*  Name:
*     ONE_FIND_FILE_END
*
*  Purpose:
*     Terminate a sequence of ONE_FIND_FILE calls.
*
*  Description:
*     This routine should be called after a sequence of calls to ONE_FIND_FILE
*     in order to release any resources used by ONE_FIND_FILE.  It should be
*     passed in its Context argument the value of the Context argument
*     as returned by the ONE_FIND_FILE in the last call in the sequence that
*     is to be closed down.
*
*  Language:
*     C   (Intended to be called from Fortran)
*
*  Invocation:
*    CALL ONE_FIND_FILE_END (CONTEXT, STATUS )
*
*  Arguments:
*    CONTEXT = INTEGER (Given)
*       The context argument returned by the last call to ONE_FIND_FILE
*       in the sequence to be closed down.
*    STATUS = INTEGER (Given and Returned)
*       Inherited status. Routine will attempt to free resources even
*       if status is bad on entry.

*  Authors:
*    KS: K. Shortridge (AAO).
*    PDRAPER: P.W. Draper (STARLINK - Durham University)
*    RTP: Roy Platon (STARLINK)
*    NG: Norman Gray (Starlink)
*    TJ: Tim Jenness (JAC, Hawaii)

*  History:
*    Unknown (KS):
*       Original unix version for Figaro
*    28-FEB-1992 (PDRAPER):
*       Changed name to FIND_FILE_END removing LIB$
*    16-MAR-1993 (PDRAPER):
*       Changed to use Starlink CNF interface.
*    25-SEP-2000 (RPT):
*       Changed for Odds and Ends Library
*       Uses cnf pointers.
*       Also changed to Subroutine.
*    28-AUG-2004 (TIMJ):
*       Incorporate tweaks from Norman's patches to SST. Use cnfCptr
*       Always try to free regardless of status
*    2011-03-08 (TIMJ):
*       Check exit status from waitpid and set EMS status accordingly.
*-
 */

F77_SUBROUTINE(one_find_file_end)( POINTER(Context), INTEGER(Status) )
{
   GENPTR_POINTER(Context)
   GENPTR_INTEGER(Status)
   char Char;                        /* Character read from pipe */
   ContextStruct *ContextPtr;        /* Pointer to context structure */

   /*  If the Context passed to us is a non-null pointer, attempt to
    *  close the input fd for the pipe it should contain, and to release
    *  the memory used for it. Also empty the pipe so that the writing
    *  process is closed down neatly.
    */

   if (*Context != 0) {
      ContextPtr = (ContextStruct *) cnfCptr( *Context );
      if (ContextPtr != NULL) {
	while (read(ContextPtr->Fds[0],&Char,1) > 0);
	(void) close (ContextPtr->Fds[0]);
	if (ContextPtr->ls_pid > 0) {
          /* Wait for the child process, and reap its status.  The pid
             should be positive, but we test this just out of
             paranoia. */
          int process_status;
          waitpid (ContextPtr->ls_pid, &process_status, 0);
          if (! WIFEXITED(process_status) || /* Did not exit using _exit */
              WEXITSTATUS(process_status)    /* Exited with bad status */
              ) {
	    /* non-normal exit */
	    if (*Status == SAI__OK) {
	      *Status = ONE__PIPEERR;
	      emsRep( "ONE_FIND_FILE_END","Error from child",
		      Status);
	    }
          }
	}
	(void) cnfFree ((char *) ContextPtr);
      }
   }

   return;
}
/* $Id$ */

