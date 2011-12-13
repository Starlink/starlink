#define _POSIX_SOURCE 1		 /* Declare POSIX source		    */

#if defined( vms )		 /* VMS version include files:		    */
#include <stddef.h>		 /* Define NULL				    */

#include <descrip.h>		 /* Descriptor definitions		    */
#include <jpidef.h>		 /* lib$getjpi definitions		    */
#include <libdef.h>		 /* Run time library definitions	    */
#include <stsdef.h>		 /* System status codes			    */

#else				 /* POSIX version include files:	    */
#include <errno.h>		 /* For errno				    */
#include <limits.h>		 /* System limits (for PATH_MAX)	    */
#include <stdio.h>		 /* For FILENAME_MAX			    */
#include <stdlib.h>		 /* For malloc, free, getenv, etc.	    */
#include <string.h>		 /* String functions			    */

#include <sys/types.h>		 /* For stat function			    */
#include <sys/stat.h>		 /* For stat function			    */
#include <unistd.h>		 /* For access, getcwd, etc.		    */

#if !defined( FILENAME_MAX )	 /* Overcome gcc compiler problems on SUNs  */
#if defined( PATH_MAX )		 /* Use POSIX definition instead of ANSI C  */
#define FILENAME_MAX PATH_MAX
#else
#define FILENAME_MAX _POSIX_PATH_MAX
#endif
#endif
#endif

#include "sae_par.h"		 /* Standard SAE constants		    */
#include "ems.h"		 /* EMS_ error reporting routines	    */
#include "f77.h"		 /* Fortran 77 <=> C interface macros	    */
#include "ndf1.h"		 /* Internal NDF definitions		    */

   F77_SUBROUTINE(ndf1_gtfil)( CHARACTER(NAME),
			       INTEGER(LNAME),
			       INTEGER(STATUS)
			       TRAIL(NAME) )
   {
/*
*+
*  Name:
*     NDF1_GTFIL

*  Purpose:
*     Get the full name of the currently-executing file.

*  Language:
*     ANSI C

*  Invocation:
*     CALL NDF1_GTFIL( NAME, LNAME, STATUS )

*  Description:
*     The routine returns the full operating system name of the
*     currently-executing file, left justified. The returned value will
*     be truncated without error if the variable supplied is too short.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Returned)
*        Full name of the currently executing file.
*     LNAME = INTEGER (Returned)
*        Number of significant characters in the returned file name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - A value of zero will be returned for LNAME if an error has
*     occurred prior to calling this routine, or if an error occurs
*     during this routine.
*     - A blank value may be returned if the file name cannot be
*     determined.
*     -  This routine is intended to be callable from Fortran.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     DSB: David S. Berry (STARLINK)
*     {@enter_new_authors_here@}

*  History:
*     4-OCT-1993 (RFWS):
*        Original version.
*     8-JUN-2001 (DSB):
*        Initialize LNAME before checking inherited status.
*     {@enter_changes_here@}

*  Bugs:
*     {@note_any_bugs_here@}

*-
*/

/* Arguments Returned:							    */
      GENPTR_CHARACTER(NAME)
      GENPTR_INTEGER(LNAME)

/* Status:								    */
      GENPTR_INTEGER(STATUS)

/* External References:							    */
#if defined( vms )		 /* VMS system calls:			    */
      extern unsigned int lib$getjpi /*	Get job/process information	    */
         ( int *item_code,
	   unsigned int *process_id,
	   struct dsc$descriptor *process_name,
	   void *resultant_value,
	   struct dsc$descriptor *resultant_string,
	   unsigned short int *resultant_length );
#endif

/* Local Variables:							    */
#if defined( vms )		 /* VMS version local variables:	    */
      int code;			 /* getjpi item code			    */
      int i;			 /* Loop counter for characters		    */
      static char *result = NULL; /* Pointer to file name		    */
      static int first = 1;	 /* First invocation?			    */
      static int length;	 /* Length of file name			    */
      struct dsc$descriptor dsc; /* VMS descriptor			    */
      unsigned int systat;	 /* System status code			    */
      unsigned short int len;	 /* Length of file name (temporary)	    */

#else				 /* POSIX version local variables:	    */
      DECLARE_CHARACTER(arg0,FILENAME_MAX); /* Buffer for command string    */
      DECLARE_INTEGER(iarg);	 /* Number of the argument required	    */
      DECLARE_INTEGER(larg);	 /* Length of argument			    */
      char *dir;		 /* Pointer to directory path string	    */
      char *file;		 /* Pointer to full file name string	    */
      char *localpath;		 /* Pointer to local copy of path	    */
      char *name;		 /* Pointer to path name string		    */
      char *path;		 /* Pointer to search path string	    */
      char *tmp;	         /* Temporary variable			    */
      char cwd[ FILENAME_MAX + 1 ]; /* Current working directory buffer	    */
      int found;		 /* File search successful?		    */
      int i;			 /* Loop counter for characters		    */
      size_t size;		 /* Amount of memory to allocate	    */
      size_t szname;		 /* Size of name string			    */
      static char *result = NULL; /* Pointer to result string		    */
      static int first = 1;	 /* First invocation?			    */
      struct stat statbuf;	 /* File status information buffer	    */
#endif

/*.									    */


/* Initialize a safe value for LNAME.                                       */
      *LNAME = 0;

/* Check the inherited global status.					    */
      if ( *STATUS != SAI__OK ) return;

/* VMS version:								    */
/* ===========								    */
#if defined( vms )

/* We only need to perform this on the first invocation.		    */
      if ( first )
        {

/* Initialise a dynamic character string descriptor to describe the	    */
/* returned value.							    */
          dsc.dsc$a_pointer = NULL;
          dsc.dsc$b_class = DSC$K_CLASS_D;
          dsc.dsc$b_dtype = DSC$K_DTYPE_T;
          dsc.dsc$w_length = (unsigned short int) 0;

/* Obtain the file name of the currently executing image.		    */
          code = JPI$_IMAGNAME;
          systat = lib$getjpi( &code, (unsigned int) 0,
                               (struct dsc$descriptor *) 0, (void *) 0,
                               &dsc, &len );

/* Check for and report any errors - also trap string truncation (normally  */
/* regarded as a success status).					    */
          if ( !( systat & STS$M_SUCCESS ) || ( systat == LIB$_STRTRU ) )
            {
              *STATUS = NDF__FATIN;
              emsSyser( "MESSAGE", systat );
              emsRep( "NDF1_GTFIL_1",
                         "Error determining the file name of the currently \
executing VMS image - ^MESSAGE.", STATUS );
            }

/* Obtain a pointer to the resulting file name and save its length. Note    */
/* that the first invocation completed successfully.			    */
          else
            {
              result = (char *) dsc.dsc$a_pointer;
              length = (int) len;
              first = 0;
            }
        }

/* If OK, copy the result into the caller's buffer, truncating if	    */
/* necessary.								    */
      if ( *STATUS == SAI__OK )
        {
          for ( i = 0; ( i < length ) && ( i < NAME_length ); i++ )
            {
              NAME[ i ] = result[ i ];
            }

/* Return the file name length.						    */
          *LNAME = (F77_INTEGER_TYPE) i;

/* Pad the caller's buffer with blanks if necessary.			    */
          for ( ; i < NAME_length; i++ )
            {
              NAME[ i ] = (F77_CHARACTER_TYPE) ' ';
            }
        }

/* POSIX version:							    */
/* =============							    */
#else

/* We only need to perform this on the first invocation.		    */
      if ( first )
        {

/* Get the value of the zero'th argument (the name of the command being	    */
/* executed).								    */
          iarg = (F77_INTEGER_TYPE) 0;
          F77_LOCK( F77_CALL(ndf1_gtarg)( INTEGER_ARG(&iarg), CHARACTER_ARG(arg0),
                                INTEGER_ARG(&larg), INTEGER_ARG(STATUS)
                                TRAIL_ARG(arg0) ); )

/* If OK, allocate space to hold the argument value as a C string and check */
/* for errors.								    */
          if ( *STATUS == SAI__OK )
            {
              size = (size_t) larg + (size_t) 1;
              name = (char *) malloc( size );
              if ( name == NULL )
                {
                  *STATUS = NDF__NOMEM;
                  emsSeti( "NBYTES", (int) size );
                  emsErrno( "MESSAGE", errno );
                  emsRep( "NDF1_GTFIL_2",
                             "Unable to allocate a block of ^NBYTES bytes of \
memory - ^MESSAGE",
                             STATUS );
                }

/* Copy the argument value into the allocated space, appending a null. This */
/* forms the first guess at the required file path name.		    */
              else
                {
                  for ( i = 0; i < (int) larg; i++ )
                    {
                      name[ i ] = (char) arg0[ i ];
                    }
                  name[ i ] = '\0';
                }
            }

/* If the path name is blank, then return a blank result.		    */
          if ( *STATUS == SAI__OK )
            {
              if ( name[ 0 ] == '\0' )
                {
                  result = name;
                }

/* Otherwise, if the path name starts with ~, then expand this into the	    */
/* appropriate user's initial working directory name and free the original  */
/* string.								    */
              else
                {
                  if ( name[ 0 ] == '~' )
                    {
                      tmp = ndf1_tilde( name, STATUS );
                      free( (void *) name );
                      name = tmp;
                    }

/* If the path name does not contain a "/", we must identify the file by    */
/* duplicating the directory search performed when the current program was  */
/* invoked. Note the file has not yet been found. Also record the current   */
/* path name length.							    */
                  if ( *STATUS == SAI__OK )
                    {
                      found = 1;
                      if ( strchr( name, '/' ) == NULL )
                        {
                          found = 0;
                          szname = strlen( name );

/* Translate the PATH environment variable to give the directory search	    */
/* path string. Use an empty string if it has no translation.		    */
                          path = getenv( "PATH" );
                          if ( path == NULL ) path = "";

/* Since the path string will be modified (by the strtok function), we must */
/* allocate space for a local copy, checking for errors.		    */
                          size = strlen( path ) + (size_t) 1;
                          localpath = (char *) malloc( size );
                          if ( localpath == NULL )
                            {
                              *STATUS = NDF__NOMEM;
                              emsSeti( "NBYTES", (int) size );
                              emsErrno( "MESSAGE", errno );
                              emsRep( "NDF1_GTFIL_3",
                                         "Unable to allocate a block of ^NBYTES \
bytes of memory - ^MESSAGE",
                                         STATUS );
                            }

/* Make a local copy of the search path string and loop to process each	    */
/* directory field within it.						    */
                          else
                            {
                              (void) strcpy( localpath, path );
                              for ( dir = strtok( localpath, ":" );
                                    ( *STATUS == SAI__OK ) && ( dir != NULL ) &&
                                      !found; dir = strtok( (char *) NULL, ":" ) )
                                {

/* Allocate space to hold the full file name and check for errors.	    */
                                  size = strlen( dir ) + (size_t) 1 + szname +
                                    (size_t) 1;
                                  file = (char *) malloc( size );
                                  if ( file == NULL )
                                    {
                                      *STATUS = NDF__NOMEM;
                                      emsSeti( "NBYTES", (int) size );
                                      emsErrno( "MESSAGE", errno );
                                      emsRep( "NDF1_GTFIL_4",
                                                 "Unable to allocate a block of \
^NBYTES bytes of memory - ^MESSAGE",
                                                 STATUS );
                                    }

/* Concatenate the directory name and the file name with a separating "/".  */
                                  else
                                    {
                                      (void) strcpy( file, dir );
                                      (void) strcat( file, "/" );
                                      (void) strcat( file, name );

/* If the resulting path name starts with ~ (because this appeared in the   */
/* search path), then expand it into the name of the appropriate user's	    */
/* initial working directory and free the original string.		    */
                                      if ( dir[ 0 ] == '~' )
                                        {
                                          tmp = ndf1_tilde( file, STATUS );
                                          free( (void *) file );
                                          file = tmp;
                                        }

/* If file status information is available, the file is not a directory and */
/* it is executable, then it has been found. Note this fact.		    */
                                      if ( *STATUS == SAI__OK )
                                        {
                                          if ( !stat( file, &statbuf ) &&
                                               !S_ISDIR( statbuf.st_mode ) &&
                                               !access( file, X_OK ) )
                                            {
                                              found = 1;

/* Save the full file name string (which may still be relative and need the */
/* current working directory prepending). Set file to NULL to prevent this  */
/* string from being deallocated.					    */
                                              free( (void *) name );
                                              name = file;
                                              file = NULL;
                                            }
                                        }

/* Free the full file name string before returning to search the next	    */
/* directory on the search path.					    */
                                      free( (void *) file );
                                    }
                                }

/* Deallocate the local copy of the search path.			    */
                              free( (void *) localpath );
                            }
                        }
                    }

/* If the path name is absolute, or a search to identify the file was made  */
/* which was not successful, then use the current value directly.	    */
                  if ( *STATUS == SAI__OK )
                    {
                      if ( ( name[ 0 ] == '/' ) || !found )
                        {
                          result = name;
                        }

/* Otherwise, obtain the name of the current working directory, checking    */
/* for errors.								    */
                      else
                        {
                          if ( getcwd( cwd, (size_t) FILENAME_MAX ) == NULL )
                            {
                              *STATUS = NDF__FATIN;
                              emsErrno( "MESSAGE", errno );
                              emsRep( "NDF1_GTFIL_5",
                                         "Unable to determine the path name of the \
current working directory - ^MESSAGE",
                                         STATUS );
                            }

/* Loop to repeatedly remove any redundant occurrences of ./ at the start   */
/* of the relative path name.						    */
                          else
                            {
                              for ( szname = strlen( name );
                                    ( szname >= (size_t) 2 ) &&
                                      ( name[ 0 ] == '.' ) &&
                                      ( name[ 1 ] == '/' );
                                    szname -= (size_t) 2 )
                                {
                                  (void) memmove( (void *) name,
                                                  (void *) ( name + 2 ),
                                                  szname - (size_t) 1 );
                                }

/* Allocate space to hold the final path name and check for errors.	    */
                              size = strlen( cwd ) + (size_t) 1 + szname +
                                (size_t) 1;
                              result = (char *) malloc( size );
                              if ( result == NULL )
                                {
                                  *STATUS = NDF__NOMEM;
                                  emsSeti( "NBYTES", (int) size );
                                  emsErrno( "MESSAGE", errno );
                                  emsRep( "NDF1_GTFIL_6",
                                             "Unable to allocate a block of ^NBYTES \
bytes of memory - ^MESSAGE",
                                             STATUS );
                                }

/* Prepend the current working directory name to the file path name (with a */
/* separating "/") and deallocate the previous version of the path name.    */
                              else
                                {
                                  (void) strcpy( result, cwd );
                                  (void) strcat( result, "/" );
                                  (void) strcat( result, name );
                                  free( (void *) name );
                                }
                            }
                        }
                    }
                }
            }

/* Note if the first invocation of this routine has completed successfully. */
          if ( *STATUS == SAI__OK ) first = 0;
        }

/* If OK, copy the result to the caller's buffer, truncating if necessary.  */
      if ( *STATUS == SAI__OK )
        {
          for ( i = 0; ( result[ i ] != '\0' ) && ( i < NAME_length ); i++ )
            {
              NAME[ i ] = (F77_CHARACTER_TYPE) result[ i ];
            }

/* Return the length of the returned path name.				    */
          *LNAME = (F77_INTEGER_TYPE) i;

/* Pad the caller's buffer with blanks if necessary.			    */
          for ( ; i < NAME_length; i++ )
            {
              NAME[ i ] = (F77_CHARACTER_TYPE) ' ';
            }
        }
#endif

/* If necessary, call the error tracing function. */
      if ( *STATUS != SAI__OK ) ndf1Trace( "ndf1_gtfil", STATUS );

/* Exit the routine.							    */
      return;
   }
