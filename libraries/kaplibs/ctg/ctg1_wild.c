#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>
#include <glob.h>
#include "f77.h"
#include "cnf.h"
#include "sae_par.h"

#define LINE_LEN 512

#define CTG__OK     0
#define CTG__WNMF 101         
#define CTG__WPER 102
#define CTG__WLER 103
#define CTG__WMER 104

typedef struct ContextStruct {
  int Fds[2];          /* Input and output file descriptors for the pipe */
  int pid;             /* PID for child process */        
} ContextStruct;


F77_INTEGER_FUNCTION(ctg1_wild)( CHARACTER(FileSpec), CHARACTER(FileName),
                                 POINTER(Context) TRAIL(FileSpec) 
                                 TRAIL(FileName) ) {
/*
*  Routine:
*     CTG1_WILD

*  Function:
*     Returns successive file names matching a given filename pattern.

*  Description:
*     This routine expands a filename pattern into an list of matching
*     files. All matching files are included, not just those which can be 
*     processed by the CAT library. Shell meta-characters can be included
*     in the filename pattern. The tcsh shell is used if it is available.
*     otherwise the csh shell is used if available, otherwise sh is used.
*
*     On the first call the Context variable should be set to zero, and 
*     the routine will return the name of the first file that matches 
*     the file specification. On subsequent calls the calling routine 
*     should continue to call using the value of Context returned
*     by the previous call, and each call will return the name of the next
*     file that matches the specification. When the last file that matches
*     the specification has been returned, subsequent calls will return
*     a blank string as the file name and an error code (CTG__WNMF)
*     as the function value. Finally, a call to CTG1_EWILD with the
*     last returned value of Context will close down any files opened or
*     memory allocated by CTG1_WILD.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     STATUS = CTG1_WILD( FILESPEC, FILENAME, CONTEXT )

*  Arguments:
*     FILESPEC = CHARACTER * ( * ) (Given)
*        The file pattern to be matched.  May contain wildcards, etc.  
*        Case sensitive.
*     FILENAME = CHARACTER * ( * ) (Returned) 
*        The name of a file that matches FILESPEC.
*     CONTEXT = INTEGER (Given and Returned)
*        A variable used to remember the context of a series of calls for 
*        the same file specification. Should be set to zero by the caller 
*        for the first call for a new specification and the value then 
*        returned by this routine should be used in subsequent calls.

*  Function Value:
*     CTG_WILD = INTEGER 
*        A status code. Zero for success.

*  Method:
*      The routine works by forking off a process that runs an ls
*      command that expands the given file pattern into a list of files.
*      The output from this process is then read through a pipe. File
*      names are assumed to be separated by white space or newlines. 

*  Authors:
*     DSB: David S. Berry (STARLINK)

*  History:
*     10-SEP-1999 (DSB):
*        - First version, based on NDG1_WILD.
*/
   GENPTR_CHARACTER(FileSpec) /* Pointer to file specification. Length is
                                 FileSpec_length */
   GENPTR_CHARACTER(FileName) /* Pointer to string to return file name in
                                 Length is FileName_length */
   GENPTR_POINTER(Context)    /* Used to remember context of search */

/* Local variables  */
   ContextStruct *ContextPtr; /* Pointer to current context information */
   char *command;        /* The shell commmand to use */
   char *spec;           /* Local copy of FileSpec */
   char Char;            /* Byte read from pipe */
   char Line[LINE_LEN];  /* String into which line is read */
   const char *shell_exe;/* The path to the shell to use */
   const char *shell_name;/* The name of the shell to use */
   glob_t *pglob;        /* Results oflooking for a shell */
   int *Fdptr;           /* Pointer to array of two integer file descriptors */
   int Bytes;            /* Number of bytes read from pipe */
   int GotFileName;      /* Used to control loop reading lines from command */
   int Ichar;            /* Index into FileName for next character */
   int NullFd;           /* File descriptor for the null device */
   int Status;           /* Status value returned to caller */

/* Start off with good status */
   Status = CTG__OK;

/* The first time through for a new file specification, Context will
   be zero. */
   if ( *Context == 0 ) {

/* First find the shell to use. Allocate memory for a structure to hold 
   the results of a globbing operation. */
      pglob = (glob_t *) malloc( sizeof( glob_t ) );

/* Abort if we couldn't allocate memory. */
      if( !pglob ) return (CTG__WMER);

/* Use tcsh if it can be found. Otherwise use csh if it can be found. 
   Otherwise, use sh. */
      shell_exe = "/bin/tcsh";
      shell_name = "tcsh";
   
      if( glob( shell_exe, 0, NULL, pglob ) ) {
         shell_exe = "/bin/csh";
         shell_name = "csh";
   
         if( glob( shell_exe, 0, NULL, pglob ) ) {
            shell_exe = "/bin/sh";
            shell_name = "sh";
         }
   
      }

/* Free the resources used to check for the existence of the shell
   commands. */
      globfree( pglob );
      free( pglob );
   
/* Import the FileSpec string */
      spec = cnf_creim( FileSpec, FileSpec_length );

/* Construct the command used to list the file. */
      command = (char *) malloc( strlen( spec ) + 10 );
      if( !command )  return (CTG__WMER);

      strcpy( command, "ls " );
      strcpy( command + strlen( command ), spec );

/* Allocate ourselves enough space for the context structure we will use 
   to hold its address. */
      ContextPtr = (ContextStruct *) malloc( 2 * sizeof(ContextStruct) );
      if ( (int) ContextPtr == 0 ) {
         Status = CTG__WMER;
      } else {
         Fdptr = ContextPtr->Fds;

/* Fdptr now points to an array of two ints, which we will use as the 
   two file descriptors of a pipe which we now create. */
         if ( pipe(Fdptr) < 0 ) {
            Status = CTG__WPER;
         } else {

/* Fdptr[0] can now be used as the reading end of the pipe, and Fdptr[1] as 
   the writing end.  Having that, we now fork off a new process to do the 
   command that we will use to give us the filenames we want. */
            if( ( ContextPtr->pid = fork() ) == 0 ) {

/* This is the child process that we will use to run a command to get 
   all the files matching the supplied file spec. Now we arrange things 
   so that the command will send its output back down our pipe. We want 
   to redirect our current standard output to the writing end of the pipe.
   This is a standard UNIX technique. */
               (void) close(Fdptr[0]);/* Don't need to read from pipe */
               (void) close(1);       /* Close fd 1 (standard output) so that */
                                      /* the dup call will find fd 1 as the */
                                      /* first available fd. */
               (void) dup(Fdptr[1]);  /* Duplicate pipe write fd as first */
                                      /* available fd, ie standard output */
               (void) close(Fdptr[1]);/* Pipe write fd no longer needed */

/*  We're going to ignore any error messages so we do the same trick for 
    standard error, this time using a file descriptor opened on the 
    null device. This has the effect of nulling any error messages. */
               NullFd = open("/dev/null",O_WRONLY,0);
               (void) close(2);
               (void) dup(NullFd);
               (void) close(NullFd); 

/*  And now we execute the command.  This will write the set of files to 
    our pipe, and the process will close down when all the files are 
    listed. */
               execlp( shell_exe, shell_name, "-c", command, NULL );

/* This part should never be reached unless there are problems with the 
   shell execution. Exit using _exit in this unlikely case. This terminates 
   the child correctly without invoking any exit handlers (which close 
   down HDS for instance). */
               (void) _exit(127);

/* Wait for forked process to terminate */
            } else {

/* This is the parent process, continuing after forking off. Here we set 
   Context to be the address of the pair of file descriptors being used 
   for the pipe, and carry on. */
               *Context = (F77_POINTER_TYPE) ContextPtr;

/* Close the writing end of the pipe. */
               close( Fdptr[1] );

            }
         }
      }

      cnf_free( spec );
      free( command );

   }

/* At this point, if Status still indicates OK, Context points to a context 
   structure containing two file descriptors, the first of which is that of 
   the reading end of the pipe to which our forked process is writing a set 
   of file names. The names are separated by spaces and/or newlines. */

   ContextPtr = (ContextStruct *) *Context;
   Fdptr = ContextPtr->Fds;
   if ( FileName_length < 1 ) {
      Status = CTG__WLER;
   } else {

/* The following loop continues until we have a filename read from the pipe 
   or until the pipe is empty. */
      GotFileName = 0; 
      while ( ! GotFileName ) {

/* This loop reads a filename from the pipe. Each filename should be a single 
   word terminated by white space. */
         Ichar = 0;
         for ( ;; ) {
            Bytes = read( Fdptr[0], &Char, 1 );
            if ( Bytes <= 0 ) break;

            if( !isspace( Char ) ) {

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

/* If no characters were read, set status */
         if( Ichar == 0 ) Status = CTG__WNMF;

/* If Status is bad exit while loop */
         if (Status != CTG__OK ) break;

/* Otherwise we have a filename, so store it */
         GotFileName = 1;
         cnf_expn( Line, LINE_LEN, FileName, FileName_length );
      }
   }

   return (Status);

}



F77_INTEGER_FUNCTION(ctg1_ewild)( POINTER(Context) ) {
/*
*  Name:
*     CTG1_EWILD

*  Purpose:
*     Terminate a sequence of CTG1_WILD calls.

*  Description:
*     This routine should be called after a sequence of calls to CTG1_WILD
*     in order to release any resources. It should be passed in its
*     Context argument the value of the Context argument as returned
*     in the last call in the sequence that is to be closed down.

*  Language:
*     C   (Intended to be called from Fortran)

*  Invocation:
*    STATUS = CTG1_EWILD( CONTEXT )
*
*  Arguments:
*    CONTEXT = INTEGER (Given and Returned)
*       The context.

*  Returned Value:
*    CTG1_EWILD = INTEGER 
*       A standard status code. 

*/
   GENPTR_POINTER(Context)
   char Char;                        /* Character read from pipe */
   int status;
   ContextStruct *ContextPtr;        /* Pointer to context structure */

/* If the Context passed to us is a non-null pointer, wait for the 
   child process to die (if it has not already died). Also attempt to
   close the input fd for the pipe it should contain, and to release
   the memory used for it. Also empty the pipe so that the writing
   process is closed down neatly. */
   if (*Context != 0) {

      ContextPtr = (ContextStruct *) *Context;

      waitpid(ContextPtr->pid, &status, 0 );

      while (read(ContextPtr->Fds[0],&Char,1) > 0);
      (void) close (ContextPtr->Fds[0]);
      (void) close (ContextPtr->Fds[1]);
      (void) free ((char *) ContextPtr);
   }

   return (CTG__OK);

}

