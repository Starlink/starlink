#include "hds1_feature.h"	 /* Define feature-test macros, etc.	    */

#if defined( vms )
void rec1_find_file( void ){};	 /* This routine not used on VMS systems    */
#else

/* C include files:							    */
/* ===============							    */
#include <errno.h>
#include <stdio.h>

/* Posix include files:							    */
/* ===================							    */
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/* Other include files:							    */
/* ===================							    */
#include "ems.h"		 /* EMS error reporting routines	    */
#include "ems_par.h"		 /* EMS__ public constants		    */
#include "hds1.h"		 /* Global definitions for HDS		    */
#include "rec.h"		 /* Public rec_ definitions		    */
#include "rec1.h"		 /* Private rec_ definitions		    */
#include "dat_err.h"		 /* DAT__ error code definitions	    */

   void rec1_find_file( const char *fspec, INT fspec_len, pid_t *pid,
		        FILE **stream )
   {
/*+									    */
/* Name:								    */
/*    rec1_find_file							    */

/* Purpose:								    */
/*    Execute a shell script to find files (UNIX & POSIX only).		    */

/* Invocation:								    */
/*    rec1_find_file( fspec, fspec_len, pid, stream )			    */

/* Description:								    */
/*    This routine creates a child process to execute a shell script which  */
/*    searches for wild-carded files. Shell meta-characters are		    */
/*    interpreted, including pattern matching, environment variable	    */
/*    expansion and command substitution. Defaulting of the file type	    */
/*    extension to ".sdf" takes place when appropriate. A space-separated   */
/*    list of wild-card file specifications may also be supplied.	    */
/*									    */
/*    The returned file names may be absolute path names, or may be	    */
/*    relative to the current working directory. Files given explicitly	    */
/*    (i.e. without pattern-matching characters) are not checked for	    */
/*    existence. File specifications containing pattern-matching characters */
/*    which do not match any file may also be returned as file names	    */
/*    (depending on the shell in use, as determined by the HDS_SHELL tuning */
/*    parameter).							    */
/*									    */
/*    The routine returns a file stream attached to the shell process and   */
/*    open for reading. The names of the files found can be read from this  */
/*    stream; they are separated by white space and terminated by the end   */
/*    of file.								    */

/* Parameters:								    */
/*    const char *fspec							    */
/*	 Pointer to a char array containing the file specification to be    */
/*	 expanded (not null terminated). Leading and trailing white space   */
/*	 is ignored.							    */
/*    INT fspec_len							    */
/*       The number of characters in the file specification.		    */
/*    pid_t *pid							    */
/*	 Pointer to an integer of type pid_t into which the routine will    */
/*	 write the process ID of the child process which is created to	    */
/*	 execute the shell script.  It is the caller's responsibility to    */
/*	 ensure that the created process terminates correctly and to wait   */
/*	 for it (e.g. with the waitpid function).			    */
/*    FILE **stream							    */
/*	 The pointer *stream will be set by this routine to identify the    */
/*	 file stream from which the list of files found may be read. It is  */
/*	 the caller's reponsibility to close this stream when it is no	    */
/*	 longer required.						    */

/* Returned Value:							    */
/*    void								    */

/* Notes:								    */
/*    -  A *pid value of (pid_t) -1 and a *stream value of NULL will be	    */
/*    returned if this routine is called with the global status set, or if  */
/*    it should fail for any reason.					    */
/*    -  The standard error stream of the shell process is routed to the    */
/*    null device (i.e. error messages from this process are suppressed).   */
/*    -  This routine is not implemented on VMS systems.		    */

/* Copyright:								    */
/*    Copyright (C) 1992 Science & Engineering Research Council		    */

/* Authors:								    */
/*    RFWS: R.F. Warren-Smith (STARLINK, RAL)				    */
/*    {@enter_new_authors_here@}					    */

/* History:								    */
/*    27-NOV-1992 (RFWS):						    */
/*       Original version.						    */
/*    30-NOV-1992 (RFWS):						    */
/*	 Added "sh" and "tcsh" shell implementations.			    */
/*    {@enter_changes_here@}						    */

/* Bugs:								    */
/*    {@note_any_bugs_here@}						    */

/*-									    */

/* Local Variables:							    */
      FILE *str[ 2 ];		 /* File streams for the shell process	    */
      int stat_val;		 /* Shell process status information	    */

/*.									    */

/* Set initial null values for the returned results.			    */
      *pid = (pid_t) -1;
      *stream = NULL;

/* Check the inherited global status.					    */
      if ( !_ok( hds_gl_status ) ) return;

/* Create a shell process to perform the file search.			    */
      rec1_shell( pid, str );
      if ( _ok( hds_gl_status ) )
      {

/* Write shell commands to the shell process on its input stream. Each of   */
/* the following sends a script in the appropriate shell language which	    */
/* searches for files using the HDS rules on the use of file extensions.    */

/* csh and tcsh shells:							    */
/* ===================							    */
         if ( ( hds_gl_shell == HDS__CSHSHELL ) ||
	      ( hds_gl_shell == HDS__TCSHSHELL ) )
	 {

/* Turn filename expansion off. This allows each element in the file	    */
/* specification to be handled explicitly as text, rather than being	    */
/* expanded into a list of files.					    */
            (void) fprintf( str[ 1 ], "set noglob\n"

/* Initialise the result string and loop to process each wild-card element. */
/* Enter the supplied file specification here; command substitution,	    */
/* environment variable expansion, etc. may occur at this point, but	    */
/* without file name expansion.						    */
			              "set r\n"
			              "foreach f(" );
	    (void) fwrite( (const void *) fspec, sizeof( char ),
	                   (size_t) fspec_len, str[ 1 ] );
	    (void) fprintf( str[ 1 ],            ")\n"

/* Test each element of the file specification. If the operation of	    */
/* removing the file extension and replacing it with a dot gives a string   */
/* equal to the original, then it ends with a dot. Remove this dot (a	    */
/* single final dot is always removed from file names by HDS before use).   */
			                 "if(\"$f:r.\" == \"$f\")then\n"
			                     "set f=\"$f:r\"\n"
			                 "else\n"

/* Otherwise, if the element has no file extension, then add a default one  */
/* of ".sdf".								    */
			                    "if(\"$f:e\" == \"\")then\n"
				               "set f=\"$f.sdf\"\n"
			                    "endif\n"
			                 "endif\n"

/* Concatenate all the (processed) elements of the file specification into  */
/* a single string. Finally, turn file name expansion back on and echo the  */
/* expanded file name list to standard output.				    */
			                 "set r=\"$r $f\"\n"
			              "end\n"
			              "unset noglob\n"
			              "echo $r\n" );
         }
	 
/* sh shell:								    */
/* ========								    */
/* Include the "no shell" case, as the "sh" shell is still used in this	    */
/* instance to perform wild-card file searching.			    */
	 else if ( ( hds_gl_shell == HDS__SHSHELL ) ||
	           ( hds_gl_shell == HDS__NOSHELL ) )
	 {

/* Turn filename expansion off. This allows each element in the file	    */
/* specification to be handled explicitly as text, rather than being	    */
/* expanded into a list of files.					    */
            (void) fprintf( str[ 1 ], "set -f\n"

/* Initialise the result string and loop to process each wild-card element. */
/* Enter the supplied file specification here; command substitution,	    */
/* environment variable expansion, etc. may occur at this point, but	    */
/* without file name expansion.						    */
			              "r=\n"
			              "for f in " );
	    (void) fwrite( (const void *) fspec, sizeof( char ),
	                   (size_t) fspec_len, str[ 1 ] );
            (void) fprintf( str[ 1 ],           "\n"

/* Pipe each element of the file specification through an "awk" script to   */
/* apply the appropriate file type extension defaulting rules.		    */
			              "do\n"
			                 "f=\"`echo $f | awk \'{"

/* If the final character is a dot, then remove it (a single final dot is   */
/* always removed from file names by HDS before use).			    */
			                    "if(substr($0,length($0),1)==\".\")"
			                    "{"
				                "print "
						"substr($0,1,length($0)-1);"
			                    "}"

/* Otherwise, loop backwards through the file specification examining each  */
/* character.								    */
			                    "else"
			                    "{"
				                "e=0;"
				                "for(i=length($0);i>0;i--)"
				                "{"
				                    "c=substr($0,i,1);"

/* Quit searching if a slash '/' is found; this marks the start of the	    */
/* final file name field.						    */
				                    "if(c==\"/\")break;"

/* If a dot '.' is found in the final name field, then note this fact and   */
/* quit the loop.							    */
				                    "if(c==\".\")"
				                    "{"
					                "e=1;"
					                "break;"
				                    "}"
				                "};"

/* If a dot was found, then a file type extension is present, so use the    */
/* specification as supplied.						    */
				                "if(e)"
				                "{"
				                    "print $0;"
				                "}"

/* If there is no dot, then add the default HDS file type extension of	    */
/* ".sdf".								    */
				                "else"
				                "{"
				                    "print $0\".sdf\";"
				                "}"
				             "}"
/* End of "awk" script.							    */
				         "}\'`\"\n"

/* Concatenate all the (processed) elements of the file specification into  */
/* a single string. Finally, turn file name expansion back on and echo the  */
/* expanded file name list to standard output.				    */
			                 "r=\"$r $f\"\n"
			              "done\n"
			              "set +f\n"
			              "echo $r\n" );
	 }

/* If the specified shell was not recognised, then there is an internal HDS */
/* programming error (perhaps a new shell has been added incorrectly), so   */
/* detect this case and report an error.				    */
	 else
	 {
	    hds_gl_status = DAT__FATAL;
	    ems_seti_c( "SHELL", hds_gl_shell );
	    ems_rep_c( "REC_FIND_FILE_1",
	               "Invalid UNIX shell (no. ^SHELL) specified; this shell \
is not properly supported within HDS (internal programming error).",
		       &hds_gl_status );
	 }

/* Check for errors.							    */
/* ================							    */
/* If an error occurred while sending commands to the shell process, then   */
/* report it.								    */
	 if ( _ok( hds_gl_status ) )
	 {
	    if ( ferror( str[ 1 ] ) )
	    {
	       hds_gl_status = DAT__FATAL;
               ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZTOK );
	       ems_rep_c( "REC_FIND_FILE_2",
	                  "Error sending commands to a shell process to \
perform a wild-card file search - ^MESSAGE",
		          &hds_gl_status );
	    }
	 }

/* Close the input stream to the shell process and check for errors. Do	    */
/* this inside a new error reporting environment in case we are cleaning up */
/* after a previous error.						    */
	 ems_begin_c( &hds_gl_status );
         if ( fclose( str[ 1 ] ) != 0 )
	 {
	    hds_gl_status = DAT__FATAL;
            ems_setc_c( "MESSAGE", strerror( errno ), EMS__SZTOK );
	    ems_rep_c( "REC_FIND_FILE_3",
	               "Error closing stream used for sending commands to a \
shell process - ^MESSAGE",
		       &hds_gl_status );
	 }
         ems_end_c( &hds_gl_status );

/* If there was no error, then return the output stream for the shell	    */
/* process, from which the list of files can be read.			    */
         if ( _ok( hds_gl_status ) )
         {
            *stream = str[ 0 ];
         }

/* Otherwise, close the output stream, kill the process, wait for it, and   */
/* return a null process ID.						    */
         else
         {
            (void) fclose( str[ 0 ] );
	    (void) kill( *pid, SIGKILL );
	    (void) waitpid( *pid, &stat_val, 0 );
	    *pid = (pid_t) -1;
         }
      }


/* Exit the routine.							    */
      return;
   }
#endif
