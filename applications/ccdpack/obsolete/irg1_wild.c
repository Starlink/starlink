/*+
 *
 *  Routine:
 *     IRG1_WILD
 *
 *  Function:
 *     Returns successive file names that can be processed by the NDF
 *     library
 *
 *  Description:
 *     This routine expands a filename pattern into an ordered list of
 *     files that can be processed by the NDF library given the
 *     current state of the NDF_FORMATS_IN environment variable. If
 *     thus expands a pattern into a list of files that can be processed
 *     using the foreign file format capabilities of the NDF library.
 *
 *     On the first call the Context variable should be set
 *     to zero, and the routine will return the name of the first file that
 *     matches the file specification. On subsequent calls the calling
 *     routine should continue to call using the value of Context returned
 *     by the previous call, and each call will return the name of the next
 *     file that matches the specification. When the last file that matches
 *     the specification has been returned, subsequent calls will return
 *     a blank string as the file name and an error code (an even value)
 *     as the function value. Finally, a call to IRG1_EWILD with the
 *     last returned value of Context will close down any files opened or
 *     memory allocated by IRG1_WILD.
 *
 *  Language:
 *     C, designed to be called from Fortran.
 *
 *  Call:
 *     (from Fortran)
 *     STATUS = IRG1_WILD( FILESPEC, FILENAME, CONTEXT )
 *
 *  Parameters:
 *     (>) FILESPEC   (Character string) The file pattern to be
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
 *  Method:
 *      The routine works by forking off a process that runs a script
 *      that expands the given file pattern into a list of files
 *      suitable for processing by the NDF library. The output from this
 *      process is then read through a pipe. Each
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
 *  History:
 *     26-FEB-1997: Peter W. Draper (Starlink)
 *        Original version heavily based on "file_file.c" by Keith
 *        Shortridge. Changes from the original are that this is now
 *        NDF foreign file format specific and is no longer general
 *        about the file types it can handle.
 *+
 */

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
#include "cnf.h"
#include "sae_par.h"
#include "irg_err.h"

/*  Constants used */

#define TRUE 1                /* Standard truth value */
#define FALSE 0               /* Standard false value */
#define LINE_LEN 512          /* Number of characters allowed in one line */


/*  Macros used */
#define MINIMUM(a,b) ( (a) < (b) ) ? (a) : (b)


/*  Structures used:
 *
 *  A ContextStruct is used to maintain the context of a given series of
 *  calls to this routine.  It contains the two file descriptors used
 *  for the pipe that reads from the forked process.
 */

typedef struct ContextStruct {
  int Fds[2];          /* Input and output file descriptors for the pipe */
  int dum[2];
} ContextStruct;

/*  Prototypes: */
static char *get_irg_wild ();

/*  Global variables:
 *
 *  Define a character array for storing the name of the irg_wild
 *  script, this is set once and reuse by all subsequent calls.
 */

static char wild_script[LINE_LEN];
static char *irg_wild = NULL;

F77_INTEGER_FUNCTION(irg1_wild)( CHARACTER(FileSpec), CHARACTER(FileName),
                                 POINTER(Context)
                                 TRAIL(FileSpec) TRAIL(FileName) )
{
  GENPTR_CHARACTER(FileSpec) /* Pointer to file specification. Length is
                                FileSpec_length */
  GENPTR_CHARACTER(FileName) /* Pointer to string to return file name in
                                Length is FileName_length */
  GENPTR_POINTER(Context)    /* Used to remember context of search */


  /*  Local variables  */
  ContextStruct *ContextPtr; /* Pointer to current context information */
  char *spec;           /* Local copy of FileSpec */
  char *wild;           /* Pointer to irg_wild script*/
  char Char;            /* Byte read from pipe */
  char Command[LINE_LEN];    /* script command executed */
  char LastChar;        /* Last non-blank char read in line */
  char Line[LINE_LEN];  /* String into which line is read */
  int *Fdptr;           /* Pointer to array of two integer file descriptors */
  int Bytes;            /* Number of bytes read from pipe */
  int ColonIndex;       /* Index in Line of last colon */
  int GotFileName;      /* Used to control loop reading lines from command */
  int I;                /* Loop index used to copy strings */
  int Ichar;            /* Index into FileName for next character */
  int Nchars;           /* Maximum number of chars that can be copied */
  int NullFd;           /* File descriptor for the null device */
  int Status;           /* Status value returned to caller */

  /*  First check we can find the irg_wild script */
  wild = get_irg_wild();
  if ( wild == NULL ) {
    return (IRG__WSER);
  }

  /*  Start off with good status */
  Status = SAI__OK;

  /* Import the FileSpec string */
  spec = cnf_creim( FileSpec, FileSpec_length );

  /*  The first time through for a new file specification, Context will
   *  be zero.  In this case we allocate ourselves enough space for the
   *  context structure we will use to hold its address.
   */

  if (*Context == 0) {
    ContextPtr = (ContextStruct *) malloc( 2 * sizeof(ContextStruct) );
    if ( (int) ContextPtr == 0 ) {
      Status = IRG__WMER;
    } else {
      Fdptr = ContextPtr->Fds;

      /*  Fdptr now points to an array of two ints, which we will use
       *  as the two file descriptors of a pipe which we now create.
       */
      if ( pipe(Fdptr) < 0 ) {
        Status = IRG__WPER;
      } else {

        /*  Fdptr[0] can now be used as the reading end of the pipe,
         *  and Fdptr[1] as the writing end.  Having that, we now
         *  fork off a new process to do the 'ls' command that we
         *  will use to give us the filenames we want.
         */
        int STATUS;
        if ((STATUS = fork()) == 0) {

          /*  This is the child process that we will use to run a
           *  script to expand all the NDF allowable names for us.
           *  Now we arrange things so that the irg_wild command will
           *  send its output back down our pipe. We want to redirect our
           *  current standard output to the writing end of the pipe.
           *  This is a standard UNIX technique.
           */
          (void) close(Fdptr[0]);  /* Don't need to read from pipe */
          (void) close(1);         /* Close fd 1 (standard output) so that */
                                   /* the dup call will find fd 1 as the */
                                   /* first available fd. */
          (void) dup(Fdptr[1]);    /* Duplicate pipe write fd as first */
                                   /* available fd, ie standard output */
          (void) close(Fdptr[1]);  /* Pipe write fd no longer needed */
          /*  We're going to ignore any error messages so
           *  we do the same trick for standard error, this time using
           *  a file descriptor opened on the null device. This has the
           *  effect of nulling any error messages.
           */
          NullFd = open("/dev/null",O_WRONLY,0);
          (void) close(2);
          (void) dup(NullFd);
          (void) close(NullFd);

          /*  And now we run execute the "irg_wild filespec" command.  This
           *  will write the set of files to our pipe, and the process
           *  will close down when all the files are listed. Note that
           *  this command assumes that irg_wild exists somewhere on
           *  the path and is executable.
           */
          execlp( "/bin/sh", "-c", wild, spec, NULL);

          /* This part should never be reached unless there are
           * problems with the "/bin/sh" execution.
           * Exit using _exit in this unlikely case. This
           * terminates the child correctly without invoking any
           * exit handlers (which close down HDS for instance).
           */
          (void) _exit(127);

        } else {
          /* Wait for forked process to terminate */
          (void) wait( &STATUS );

          /*  This is the parent process, continuing after forking off.
           *  Here we set Context to be the address
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
  cnf_free( spec );

  /*  At this point, if Status still indicates OK, Context points to
   *  a context structure containing two file descriptors, the first
   *  of which is that of the reading end of the pipe to which our
   *  forked process is writing a set of file names. The names are
   *  separated by spaces.
   */

  ContextPtr = (ContextStruct *) *Context;
  Fdptr = ContextPtr->Fds;
  if ( FileName_length < 1 ) {
    Status = IRG__WLER;
  } else {

    /*  The following loop continues until we have a filename read from the
     *  pipe or until the pipe is empty.
     */

    GotFileName = FALSE;
    while ( ! GotFileName ) {

      /*  This loop reads a line of characters from the pipe. Each
       *  filename should be a single line.
       */

      Ichar = 0;
      for ( ;; ) {
        Bytes = read( Fdptr[0], &Char, 1 );
        if ( Bytes <= 0 ) {
          /* No bytes so must be end of pipe */
          Status = IRG__WNMF;
          break;
        }
        if (Char != '\n' ) {
          if ( Ichar < LINE_LEN - 1 ) {
            Line[Ichar++] = Char;
          } else {
            Line[Ichar++] = '\0';
            break;
          }
        } else {
          if ( Ichar < LINE_LEN ) {
            Line[Ichar++] = '\0';
          } else {
            Line[LINE_LEN-1] = '\0';
          }
          break;
        }
      }

      /*  If Status is bad exit while loop */
      if (Status != SAI__OK ) break;

      /*  Otherwise we have a filename, so store it */
      GotFileName = TRUE;
      cnf_expn( Line, LINE_LEN, FileName, FileName_length );
    }
  }
  return (Status);
}



/*+
 *  Routine:
 *     IRG1_EWILD
 *
 *  Function:
 *     Terminate a sequence of IRG1_WILD calls.
 *
 *  Description:
 *     This routine should be called after a sequence of calls to IRG1_WILD
 *     in order to release any resources. It should be passed in its
 *     Context argument the value of the Context argument as returned
 *     in the last call in the sequence that is to be closed down.
 *
 *  Language:
 *     C   (Intended to be called from Fortran)
 *
 *  Call:
 *    (from Fortran)
 *    STATUS = IRG1_EWILD( CONTEXT )
 *
 *  Parameters:
 *
 *    (>) CONTEXT   (Integer, ref) The context.
 *
 *  Returns:
 *
 *    (<) STATUS    (Integer, status code) A standard status code.
 *                  The values are defined in the files irg_err.h
 *+
 */

F77_INTEGER_FUNCTION(irg1_ewild)( POINTER(Context) )
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

  return (SAI__OK);
}

static char *get_irg_wild ()

  /*  Local procedure to locate a copy of irg_wild. This is looked
   *  for in CCDPACK_DIR and then somewhere on the PATH. An general
   *  version of IRG would only look on the PATH.
   */
{

  /* Local Variables */
  struct stat buffer;
  char *dir;
  char *path;
  char *end;
  int found = 0;

  if ( irg_wild == NULL ) {

    /* Set irg_wild to point to the static storage space for this result */
    irg_wild = wild_script;

    /*
     *  Look for environment variable.
     *
     */
    dir = getenv( "CCDPACK_DIR" );
    if ( dir ) {
      strcpy( irg_wild, dir );
      strcat( irg_wild, "/" );
      strcat( irg_wild, "irg_wild" );
      if ( stat( irg_wild, &buffer ) == 0 ) {
        found = 1;
      }
    }
    if ( ! found ) {

      /*
       *  Search PATH.
       */
      path = getenv( "PATH" );
      for ( ;; ) {
        end = strchr( path, ':' );
        if ( end == NULL ) {
          strcpy( irg_wild, path );
        } else {
          strncpy( irg_wild, path, end-path );
          irg_wild[ end-path ] = '\0';
        }
        strcat( irg_wild, "/" );
        strcat( irg_wild, "irg_wild" );
        if (stat( irg_wild, &buffer ) == 0 ) {
          found = 1;
          break;
        }
        if ( end != NULL ) {
          path = end + 1;
        } else {
          break;
        }
      }
    }
    if ( ! found ) {
      irg_wild = NULL;
    }
  }
  return irg_wild;
}

/* $Id$ */
