#define _POSIX_SOURCE 1            /* Declare POSIX source */

#include <errno.h>                 /* For errno  */
#include <limits.h>                /* System limits (for PATH_MAX) */
#include <stdio.h>                 /* For FILENAME_MAX */
#include <stdlib.h>                /* For malloc, free, getenv, etc. */
#include <string.h>                /* String functions */

#include <sys/types.h>             /* For stat function */
#include <sys/stat.h>              /* For stat function */
#include <unistd.h>                /* For access, getcwd, etc. */

#if !defined( FILENAME_MAX )       /* Overcome gcc compiler problems on SUNs */
#if defined( PATH_MAX )            /* Use POSIX definition instead of ANSI C */
#define FILENAME_MAX PATH_MAX
#else
#define FILENAME_MAX _POSIX_PATH_MAX
#endif
#endif

#include "sae_par.h"               /* Standard SAE constants */
#include "ems.h"                   /* EMS_ error reporting routines */
#include "ndf1.h"                  /* Internal NDF definitions */
#include "ndf_err.h"               /* NDF error definitions */

void ndf1Gtfil( char *name, size_t name_length, int *status ){
/*
*+
*  Name:
*     ndf1Gtfil

*  Purpose:
*     Get the full name of the currently-executing file.

*  Synopsis:
*     void ndf1Gtfil( char *name, size_t name_length, int *status )

*  Description:
*     This function returns the full operating system name of the
*     currently-executing file, left justified. The returned value will
*     be truncated without error if the variable supplied is too short.

*  Arguments:
*     name
*        Array in which to return the full name of the currently
*        executing file.
*     name_length
*        The length of the supplied "name" array.
*     status
*        The global status.

*  Notes:
*     - A blank value may be returned if the file name cannot be
*     determined.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory

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

*  History:
*     4-OCT-1993 (RFWS):
*        Original version.
*     8-JUN-2001 (DSB):
*        Initialize LNAME before checking inherited status.
*     23-JUL-2018 (DSB):
*        Modify for calling from C. Rename to ndf1Gtfil.

*-
*/

/* Local Variables: */
   char *dir;                /* Pointer to directory path string */
   char *file;               /* Pointer to full file name string */
   char *localpath;          /* Pointer to local copy of path */
   char *path;               /* Pointer to search path string */
   char *tmp;                /* Temporary variable */
   char cwd[ FILENAME_MAX + 1 ]; /* Current working directory buffer */
   int found;                /* File search successful? */
   int nc;                   /* String length */
   int there;                /* Command line argument obtained? */
   size_t szname;            /* Size of name string */
   struct stat statbuf;      /* File status information buffer */

/* Check the inherited global status. */
   if ( *status != SAI__OK ) return;

/* Get the value of the zero'th argument (the name of the command being
   executed). */
   ndf1Gtarg( 0, name, name_length, &there, status );

/* If the path name is blank, then return a blank result. */
   if( *status == SAI__OK ){
      if ( name[ 0 ] == '\0' ) {


/* Otherwise, if the path name starts with ~, then expand this into the
   appropriate user's initial working directory name and free the original
   string. */
      } else {
         if( name[ 0 ] == '~' ){
            tmp = ndf1Tilde( name, status );
            ndf1Ccpy( tmp, name, name_length, status );
            tmp = astFree( tmp );
         }

/* If the path name does not contain a "/", we must identify the file by
   duplicating the directory search performed when the current program was
   invoked. Note the file has not yet been found. Also record the current
   path name length. */
         if( *status == SAI__OK ){
            found = 1;
            if( strchr( name, '/' ) == NULL ) {
               found = 0;
               szname = strlen( name );

/* Translate the PATH environment variable to give the directory search
   path string. Use an empty string if it has no translation. */
               path = getenv( "PATH" );
               if( path == NULL ) path = "";

/* Since the path string will be modified (by the strtok function), we must
   allocate space for a local copy, checking for errors. */
               localpath = astStore( NULL, path, strlen( path ) + 1 );
               if( *status == SAI__OK ){

/* Loop to process each directory field within the local copy of the search
   path string. */
                  for( dir = strtok( localpath, ":" );
                       *status == SAI__OK && dir && !found;
                       dir = strtok( NULL, ":" ) ) {

/* Concatenate the directory name and the file name with a separating "/". */
                     file = astAppendString( NULL, &nc, dir );
                     file = astAppendString( file, &nc, "/" );
                     file = astAppendString( file, &nc, name );

/* If the resulting path name starts with ~ (because this appeared in the
   search path), then expand it into the name of the appropriate user's
   initial working directory and free the original string. */
                     if( dir[ 0 ] == '~' ){
                        tmp = ndf1Tilde( file, status );
                        (void) astFree( file );
                        file = tmp;
                     }

/* If file status information is available, the file is not a directory and
   it is executable, then it has been found. Note this fact. */
                     if( *status == SAI__OK ){
                        if( !stat( file, &statbuf ) &&
                            !S_ISDIR( statbuf.st_mode ) &&
                            !access( file, X_OK ) ) {
                           found = 1;

/* Save the full file name string (which may still be relative and need the
   current working directory prepending). */
                           ndf1Ccpy( file, name, name_length, status );
                        }
                     }

/* Free the full file name string before returning to search the next
   directory on the search path. */
                     file = astFree( file );
                  }
               }

/* Deallocate the local copy of the search path. */
               localpath = astFree( localpath );
            }
         }
      }

/* If the path name is absolute, or a search to identify the file was made
   which was not successful, then use the current value directly. */
      if( *status == SAI__OK ){
         if( name[ 0 ] == '/' || !found ) {

/* Otherwise, obtain the name of the current working directory, checking
   for errors. */
         } else {
            if( getcwd( cwd, FILENAME_MAX ) == NULL ) {
               *status = NDF__FATIN;
               emsErrno( "MESSAGE", errno );
               emsRep( " ", "Unable to determine the path name of the "
                       "current working directory - ^MESSAGE", status );

/* Loop to repeatedly remove any redundant occurrences of ./ at the start
   of the relative path name. */
            } else {
               for( szname = strlen( name );
                    szname >= 2 && name[ 0 ] == '.' && name[ 1 ] == '/';
                    szname -= 2 ) {
                  (void) memmove( (void *) name, (void *) ( name + 2 ),
                                  szname - (size_t) 1 );
               }

/* Prepend the current working directory name to the file path name (with a
   separating "/") and deallocate the previous version of the path name. */
               file = astAppendString( NULL, &nc, cwd );
               file = astAppendString( NULL, &nc, "/" );
               file = astAppendString( NULL, &nc, name );
               ndf1Ccpy( file, name, name_length, status );
               file = astFree( file );
            }
         }
      }
   }

/* If necessary, call the error tracing function. */
   if ( *status != SAI__OK ) ndf1Trace( "ndf1Gtfil", status );

/* Exit the routine.                       */
   return;
}
