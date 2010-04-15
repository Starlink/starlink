/*+
 *                           F I N D _ F I L E
 *  Routine:
 *     FIND_FILE
 *
 *  Function:
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
 *     as the function value. Finally, a call to FIND_FILE_END with the
 *     last returned value of Context will close down any files opened or
 *     memory allocated by FIND_FILE.
 *
 *  Language:
 *     C, designed to be called from Fortran.
 *
 *  Call:
 *     (from Fortran)
 *     STATUS = FIND_FILE (FILESPEC,FILENAME,CONTEXT)
 *
 *  Parameters:
 *     (>) FILESPEC   (Character string) The file specification to be
 *                    matched.  May contain wildcards.  Case sensitive.
 *     (<) FILENAME   (Character string) The name of a file that matches
 *                    FILESPEC.
 *     (!) CONTEXT    (Integer, ref) A variable used to remember the context of
 *                    a series of calls for the same file specification.
 *                    Should be set to zero by the caller for the first call
 *                    for a new specification and the value then returned
 *                    by this routine should be used in subsequent calls.
 *  Returns:
 *     (<) STATUS     (Integer, function value) A status code. These have been
 *                    chosen to match with VMS conventions, with an odd value
 *                    (actually 1) indicating success and an even value
 *                    indicating a failure.
 *
 *  Original by: K. Shortridge, AAO.
 *
 *  Version date: 5th Feb 1992.
 *
 *  Bugs and limitations:
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
 *  Method:
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
 *  Changes:
 *      PDRAPER: 28-FEB-1992:
 *         Changed name to find_file from lib$find_file. Problems with
 *         gcc compiler and $ in name. Increased command length to 256.
 *         Fixed bug comparing Nchars and SpecLength. Added Wait for forked
 *         process. Removed extra argument to strncat function (incorrect
 *         length used as consequence).
 *      DLT: FEB 1993:
 *         Void ** to int * as Alpha returns addresses as 64 bits (casting
 *         to int * avoids this as compiler options are 32 bit. Fixed \0
 *         appending to wrong part of string. (Comments added by PDRAPER).
 *      PDRAPER: 16-MAR-1993:
 *         Changed to use Starlink CNF macros for enhanced portabilility
 *      PDRAPER: 7-JUL-1993:
 *         Stopped modification of FileSpec point by ++ operator. Now
 *         uses local copy.
 *      PDRAPER: 11-NOV-1993:
 *         New version of cnf casts input pointer as unsigned int (in
 *         line with alpha port). This breaks under cast from pointer
 *         to structure (ContextPtr) on ultrix. Added cast to
 *         F77_POINTER_TYPE to fix this.
 *      PDRAPER: 15-MAR-1995:
 *         Run through lint on OSF/1, loads of include files added.
 *      PDRAPER: 15-AUG-1995:
 *         Changed system call to execl and exit to _exit. These
 *         changes are necessary to stop exit handlers being invoked
 *         on exit of child process when being run from ICL/TCL using
 *         the AMS/MSP systems.
 *+
 */

/*  Return codes.  The values chosen are a little odd, being chosen as much
 *  as possible to duplicate the effect of the VMS routine this is based on.
 *  In particular, the NO_MORE_FILES error is the same code value as the
 *  RMS$_FNF code on the VAX, and the OK value is the same as the VAX
 *  code SS$_NORMAL.
 */

#define OK 1                  /* Same as VMS SS$_NORMAL code */
#define NO_MORE_FILES 98692   /*   "   "  "  RMS$_NMF     "  */
#define PIPE_ERROR 44         /*   "   "  "  SS$_ABORT    "  */
#define LENGTH_ERROR 20       /*   "   "  "  SS$_BADPARAM "  */
#define MALLOC_ERROR 292      /*   "   "  "  SS$_INSFMEM  "  */

/*  Constants used. */

#define TRUE 1                /* Standard truth value */
#define FALSE 0               /* Standard false value */
#define DIRECTORY_LEN 256     /* Number of characters allowed for directory */

/*  Include files */
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include "f77.h"

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
  int Fds[2];          /* Input and output file descriptors for the pipe */
  char Directory[DIRECTORY_LEN];
  /* Last directory spec output by 'ls' process */
} ContextStruct;


F77_INTEGER_FUNCTION(find_file)( CHARACTER(FileSpec), CHARACTER(FileName),
                                POINTER(Context) TRAIL(FileSpec) TRAIL(FileName) )
{
  GENPTR_CHARACTER(FileSpec) /* Pointer to file specification. Length is
                                FileSpec_length */
  GENPTR_CHARACTER(FileName) /* Pointer to string to return file name in
                                Length is FileName_length */
  GENPTR_POINTER(Context)    /* Used to remember context of search */


  /*  Local variables  */
  int SpecLength;                /* Number of bytes in FileSpec - FileSpec_length*/
  int NameLength;                /* Number of bytes in FileName - FileName_length*/
  int Bytes;            /* Number of bytes read from pipe */
  char Char;            /* Byte read from pipe */
  int ColonIndex;       /* Index in Line of last colon */
  ContextStruct *ContextPtr; /* Pointer to current context information */
  char Command[255];    /* 'ls' command executed */
  int *Fdptr;           /* Pointer to array of two integer file descriptors */
  int GotFileName;      /* Used to control loop reading lines from 'ls' */
  int I;                /* Loop index used to copy strings */
  int Ichar;            /* Index into FileName for next character */
  char LastChar;        /* Last non-blank char read in line */
  char Line[256];       /* String into which line is read */
  int Nchars;           /* Maximum number of chars that can be copied */
  int NullFd;           /* File descriptor for the null device */
  int Status;           /* Status value returned to caller */
  char *PointFSpec;     /* Local copy of FileSpec          */

  /*  Start off with good status  */

  Status = OK;

  /*
     Set input string lengths to those returned by CNF.
     */

  SpecLength = FileSpec_length;
  NameLength = FileName_length;
  PointFSpec = FileSpec;

  /*  The first time through for a new file specification, Context will
   *  be zero.  In this case we allocate ourselves enough space for the
   *  context structure we will use and use Context to hold its address.
   *  We initialise the directory specification in the context to null.
   */

  if (*Context == 0) {
    ContextPtr = (ContextStruct *) malloc(2 * sizeof(ContextStruct));
    if ((int) ContextPtr == 0) {
      Status = MALLOC_ERROR;
    } else {
      Fdptr = ContextPtr->Fds;
      ContextPtr->Directory[0] = '\0';

      /*  Fdptr now points to an array of two ints, which we will use
       *  as the two file descriptors of a pipe which we now create.
       */
      if (pipe(Fdptr) < 0) {
        Status = PIPE_ERROR;
      } else {

        /*  Fdptr[0] can now be used as the reading end of the pipe,
         *  and Fdptr[1] as the writing end.  Having that, we now
         *  fork off a new process to do the 'ls' command that we
         *  will use to give us the filenames we want.
         */

        int STATUS;
        if ((STATUS = fork()) == 0) {

          /*  This is the child process that we will use to run 'ls'
           *  for us.  We construct the command using the filespec
           *  we have been passed, remembering that this is a string
           *  from a Fortran program, and so is blank padded rather than
           *  null terminated.
           */

          Command[0] = '\0';
          (void) strcpy(Command,"ls ");
          Nchars = sizeof(Command) - 3;
          if (Nchars > SpecLength) Nchars = SpecLength;
          (void) strncat( Command, FileSpec, Nchars );

          /*  Now we arrange things so that the 'ls' command will
           *  send its output back down our pipe.  We want to redirect
           *  our current standard output to the writing end of the
           *  pipe.  This is a standard UNIX technique.
           */
          (void) close(Fdptr[0]);  /* Don't need to read from pipe */
          (void) close(1);         /* Close fd 1 (standard output) so that
                                    * the dup call will find fd 1 as the
                                    * first available fd. */
          (void) dup(Fdptr[1]);    /* Duplicate pipe write fd as first
                                    * available fd, ie standard output */
          (void) close(Fdptr[1]);  /* Pipe write fd no longer needed */
          /*  We're going to ignore any error messages from 'ls', so
           *  we do the same trick for standard error, this time using
           *  a file descriptor opened on the null device. This has the
           *  effect of nulling any error messages.
           */

          NullFd = open("/dev/null",O_WRONLY,0);
          (void) close(2);
          (void) dup(NullFd);
          (void) close(NullFd);

          /*  And now we run execute the "ls filespec" command.  This
           *  will write the set of files to our pipe, and the process
           *  will close down when all the files are listed.
           */

          execl( "/bin/sh", "sh", "-c", Command, NULL);

          /* This part should never be reached unless there are
           * problems with the "/bin/sh" execution. Exit using
           * _exit in this unlikely case. This terminates the
           * child correctly without invoking any exit handlers.
           */
          (void) _exit(127);

        } else {
          /* Wait for forked process to terminate			    */

          (void) wait( &STATUS );
          /* .... and continue					    */

          /*  This is the parent process, continuing after forking off
           *  the 'ls' command. Here we set Context to be the address
           *  of the pair of file descriptors being used for the pipe.
           *  and carry on. We don't need the output fd for the pipe,
           *  so we close that.
           */

          (void) close (Fdptr[1]);
          *Context = (F77_POINTER_TYPE) ContextPtr;
        }
      }
    }
  }

  /*  At this point, if Status still indicates OK, Context points to
   *  a context structure containing a directory specification and two
   *  file descriptors, the first of which is that of the reading end of
   *  the pipe to which our forked 'ls' process is writing a set of file
   *  names. Each name will be terminated by a newline, so we just read
   *  a character at a time from the pipe until such a newline indicates
   *  that we have a complete filename. A read of zero bytes indicates
   *  that all the filenames have been listed.
   */

  ContextPtr = (ContextStruct *) *Context;
  Fdptr = ContextPtr->Fds;
  if (NameLength < 1) {
    Status = LENGTH_ERROR;
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
          Status = NO_MORE_FILES;
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
       *  we should remember, but we still need a filename.  If Status is
       *  bad, we have reached the end of the 'ls' output.
       */

      if (Status != OK) break;
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
           *  blank pad it properly.
           */

          for (I = strlen(FileName); I < NameLength; FileName[I++] = ' ');
        }
      }
    }

  }

  return (Status);
}



/*+                      F I N D _ F I L E _ E N D
 *  Routine:
 *     FIND_FILE_END
 *
 *  Function:
 *     Terminate a sequence of FIND_FILE calls.
 *
 *  Description:
 *     This routine should be called after a sequence of calls to FIND_FILE
 *     in order to release any resources used by FIND_FILE.  It should be
 *     passed in its Context argument the value of the Context argument
 *     as returned by the FIND_FILE in the last call in the sequence that
 *     is to be closed down.
 *
 *  Language:
 *     C   (Intended to be called from Fortran)
 *
 *  Call:
 *    (from Fortran)
 *    STATUS = LIB$FIND_FILE_END (CONTEXT)
 *
 *  Parameters:
 *
 *    (>) CONTEXT   (Integer, ref) The context argument returned by the last
 *                  call to LIB$FIND_FILE in the sequence to be closed down.
 *
 *  Returns:
 *
 *    (<) STATUS    (Integer, function value) A status code, chosen to match
 *                  the VMS convention where an odd number represents OK status,
 *                  while an even number indicates an error.
 *
 *  Support: K. Shortridge, AAO.
 *
 *  Version date: 5th Feb 1992.
 *
 *  Changes:
 *  PDRAPER: 28-Feb-1992: Changed name to FIND_FILE_END removing LIB$
 *  PDRAPER: 16-Mar-1993: Changed to use Starlink CNF interface.
 *+
 */

F77_INTEGER_FUNCTION(find_file_end)( POINTER(Context) )
{
  GENPTR_POINTER(Context)
    char Char;                        /* Character read from pipe */
  ContextStruct *ContextPtr;        /* Pointer to context structure */

  /*  If the Context passed to us is a non-null pointer, attempt to
   *  close the input fd for the pipe it should contain, and to release
   *  the memory used for it. Also empty the pipe so that the writing
   *  process is closed down neatly.
   */

  if (*Context != 0) {
    ContextPtr = (ContextStruct *) *Context;
    while (read(ContextPtr->Fds[0],&Char,1) > 0);
    (void) close (ContextPtr->Fds[0]);
    (void) free ((char *) ContextPtr);
  }

  return (OK);
}
/* @(#)find_file.c   2.4   95/08/18 15:09:31   97/02/17 14:42:08 */
