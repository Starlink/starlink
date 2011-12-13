#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#if defined( vms )		 /* Not used on VMS:			    */
   void ndf1_tilde( void ){};	 /* Define dummy function		    */

#else				 /* POSIX version:			    */
#include <errno.h>		 /* Define errno			    */
#include <string.h>		 /* String handling functions		    */
#include <stdlib.h>		 /* Define malloc, free, etc.		    */

#include <sys/types.h>		 /* Type definitions (getpwnam)		    */
#include <pwd.h>		 /* Define getpwnam function		    */
#include <unistd.h>		 /* Define getlogin function		    */

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* ems_ error reporting routines	    */
#include "ems_par.h"		 /* ems_ public constants		    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   char *ndf1_tilde( const char *file, F77_INTEGER_TYPE *status )
   {
/*
*+
* Name:
*    ndf1_tilde

* Purpose:
*    Expand leading tildes at the start of POSIX file names.

* Language:
*    ANSI C

* Invocation:
*    ndf1_tilde( file, status )

* Description:
*    The routine expands tildes at the start of POSIX file path names,
*    subsituting them with the name of the user's initial working
*    directory.

* Parameters:
*    const char *file
*	 Pointer to a null-terminated string containing the initial file
*	 path name.
*    F77_INTEGER_TYPE *status
*	 Pointer to the inherited global status.

* Returned Value:
*    char *ndf1_tilde
*	 Pointer to a null-terminated string (allocated using malloc)
*	 containing the expanded file path name. The allocated space should
*	 be deallocated by the caller (using free) when no longer required.

* Notes:
*    -  If the file path name begins with ~/ then the ~ is replaced with
*    the name of the current user's initial working directory. If it
*    begins with ~user/ then the ~user is replaced with the name of the
*    specified user's initial working directory. In either case, the
*    initial working directory string is obtained from the user database.
*    The "/" terminating the first path name field may also be a null
*    (i.e. this may be the only field).
*    -  A NULL pointer is returned if this routine is invoked with *status
*    set, or if it should fail for any reason.

* Copyright:
*    Copyright (C) 1993 Science & Engineering Research Council

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

* Authors:
*    RFWS: R.F. Warren-Smith (STARLINK, RAL)
*    {@enter_new_authors_here@}

* History:
*    4-OCT-1993 (RFWS):
*       Original version.
*    {@enter_changes_here@}

* Bugs:
*    {@note_any_bugs_here@}

*-
*/

/* Local Variables:							    */
     char *result = NULL;	 /* Pointer to result string		    */
     char *user;		 /* Pointer to user name string		    */
     const char *dir;		 /* Pointer to initial working directory    */
     int dyn = 0;		 /* user string dynamically allocated?	    */
     int i;			 /* Number of initial characters to discard */
     size_t size;		 /* Allocated size of result string	    */
     struct passwd *pw;	         /* Pointer to user's passwd structure	    */

/*.									    */

/* Check the inherited global status.					    */
      if ( *status != SAI__OK ) return result;

/* If the path name does not start with ~, then allocate space to hold the  */
/* result and check for errors.						    */
      if ( file[ 0 ] != '~' )
      {
	 size = strlen( file ) + (size_t) 1;
         result = (char *) malloc( size );
	 if ( result == NULL )
	 {
	    *status = NDF__NOMEM;
	    emsSeti( "NBYTES", (int) size );
	    emsErrno( "MESSAGE", errno );
	    emsRep( "NDF1_TILDE_1",
                       "Unable to allocate ^NBYTES bytes of memory - ^MESSAGE",
		       status );
	 }

/* Copy the file into the result buffer unchanged.			    */
	 else
	 {
	    (void) strcpy( result, file );
	 }
      }

/* Otherwise, determine how many leading characters in the path name should */
/* be discarded and replaced by the initial working directory name.	    */
      else
      {
         for ( i = 1; ( file[ i ] != '\0' ) && ( file[ i ] != '/' ); i++ );

/* If there is only one (~), then find the user name under which the	    */
/* current process is logged in, checking for errors.			    */
         if ( i == 1 )
         {
            user = getlogin( );
	    if ( user == NULL )
	    {
	       *status = NDF__FATIN;
	       emsErrno( "MESSAGE", errno );
	       emsRep( "NDF1_TILDE_2",
                          "Unable to determine the user name under which the \
current process is logged in - ^MESSAGE",
		          status );
	    }
	 }

/* If the path name starts with ~user or ~user/, then allocate space to	    */
/* store the user name and check for errors.				    */
	 else
	 {
	    user = (char *) malloc( (size_t) i );
	    if ( user == NULL )
	    {
	       *status = NDF__NOMEM;
	       emsSeti( "NBYTES", i );
	       emsErrno( "MESSAGE", errno );
	       emsRep( "NDF1_TILDE_3",
                          "Unable to allocate ^NBYTES bytes of memory - \
^MESSAGE",
			  status );
	    }

/* Make a copy of the user name, appending a null.			    */
	    else
	    {
	       (void) strncpy( user, file + 1, (size_t) ( i - 1 ) );
	       user[ i - 1 ] = '\0';

/* Note the user string was dynamically allocated.			    */
	       dyn = 1;
	    }
	 }

/* If OK, obtain a pointer to the user's passwd structure in the user	    */
/* database.								    */
	 if ( *status == SAI__OK )
	 {
	    pw = getpwnam( user );

/* Check for errors.							    */
	    if ( pw == NULL )
	    {
	       *status = NDF__FATIN;
	       emsSetnc( "USER", user, EMS__SZTOK );
	       emsErrno( "MESSAGE", errno );
	       emsRep( "NDF1_TILDE_4",
                          "Unable to obtain initial working directory \
information from the system database for user \'^USER\' - ^MESSAGE",
		          status );
	    }

/* Obtain a pointer to the user's initial working directory string.	    */
	    else
	    {
	       dir = pw->pw_dir;

/* Determine the amount of space needed for the expanded file path name and */
/* allocate it.	Check for errors.					    */
	       size = strlen( dir ) + strlen( file ) - (size_t) ( i - 1 );
	       result = (char *) malloc( size );
	       if ( result == NULL )
	       {
	          *status = NDF__NOMEM;
	          emsSeti( "NBYTES", (int) size );
		  emsErrno( "MESSAGE", errno );
	          emsRep( "NDF1_TILDE_5",
                             "Unable to allocate ^NBYTES bytes of memory - \
^MESSAGE",
                             status );
	       }

/* Concatenate the initial working directory string and the remainder of    */
/* the path name.							    */
	       else
	       {
	          (void) strcpy( result, dir );
		  (void) strcat( result, file + i );
	       }
	    }
	 }
      }

/* Free the user string if necessary.					    */
      if ( dyn ) free( (void *) user );

/* If necessary, call the error tracing function. */
      if ( *status != SAI__OK ) ndf1Trace( "ndf1_tilde", status );

/* Return a pointer to the result string.				    */
      return result;
   }
#endif
