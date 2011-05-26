/*
 *+
 *  Name:
 *     ems1Starf

 *  Purpose:
 *     To find a file in the $STARLINK_DIR hierarchy with the specified access
 *     permission.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     C version:
 *        found = ems1Starf( envar, relpath, access, filename, pathlen )
 *
 *     Fortran version:
 *        CALL EMS1_STARF( ENVAR, RELPATH, ACCESS, FILENAME, PATHLEN )

 *  Description:
 *     Environment variable ENVAR may contain zero or more pathnames (normally
 *     directory names) separated by :.
 *     RELPATH is a relative pathname (or blank) which is appended in turn to
 *     each pathname defined in ENVAR. The resultant pathname is checked to see
 *     if the file exists and has the specified permission (ACCESS)
 *
 *     For the C version:
 *       If a file with the specified pathname and access permission is found,
 *       the value of the function ems1Starf is returned as 1, and the
 *       pointer pointed to by 'filename' points to a static area containing
 *       the found filename (note that the static area will be overwritten
 *       on the next call. The length of the directory spec portion of the
 *       filename (without the last /) is returned in *pathlen.
 *       Otherwise the function value is returned as 0.
 *     For the Fortran version:
 *       If a file with the specified pathname and access permission is found,
 *       the pathname in FILENAME with the length of the directory spec in
 *       PATHLEN.
 *       Otherwise FILENAME is returned as a blank string and PATHLEN as 0.
 *
 *     Neither ENVAR nor RELPATH may contain shell metacharacters for
 *     translation.
 *
 *  Arguments:
 *     envar = *char (Given)
 *        The name of the environment variable containing the search path.
 *        It may be specified as a blank string, or translate to a blank string
 *        in which case relpath is assumed to specify the full pathname.
 *     relpath = *char (Given)
 *        The relative pathname of the file - it may be a blank string
 *        in which case the search path is assumed to specify the full
 *        pathname.
 *     access = *char (Given)
 *        The required access code "r", "w", "x". Any other code will
 *        just test for existence.
 *     filename = *char (Returned)
 *        The filename. Copy immediately if required after other EMS calls.
 *     pathlen = *int (Returned)
 *        The length of filename which is directory spec.
 *     status (Fortran version only) = INTEGER (Returned)
 *

 *  Copyright:
 *     Copyright (C) 1994 Science & Engineering Research Council.
 *     Copyright (C) 1995, 1999 Central Laboratory of the Research Councils.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
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
 *     AJC: A J Chipperfield (STARLINK)
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     21-DEC-1994 (AJC):
 *        Original version.
 *      4-AUG-1995 (AJC):
 *        Remove removal of ..
 *      1-JUN-1999 (AJC):
 *        Rename C interface to ems1Starf
 *     23-FEB-2006 (TIMJ):
 *        Use starMem
 *     19-MAY-2008 (PWD):
 *        Use thread-specific data buffer for returned filename.
 *     17-JUL-2008 (PWD):
 *        Replace strtok with strtok_r for thread-safety. Protect
 *        getenv with a mutex (better than nothing, but other threads
 *        could still do a setenv/putenv outside of EMS).
 *     28-JUL-2008 (TIMJ):
 *        - Use POSIX-friendly S_ISDIR() rather than S_IFDIR.
 *        - Define POSIX compliance level for strtok_r
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* for strtok_r in standards compliance mode */
#if HAVE_STRTOK_R
#define _POSIX_C_SOURCE 200112L
#endif

#if HAVE_CONFIG_H
#include "config.h"
#endif

/* Include Statements: */
#include <stdlib.h>
#if USE_PTHREADS
#include <pthread.h>
#endif
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "star/mem.h"
#include "ems_par.h"
#include "ems_sys.h"
#include "ems1.h"
#include "ems.h"

void ems1_starf_( const char *envar, const char *relpath, const char *acmode,
                  char *filename, int *pathlen,
                  int envar_len, int relpath_len, int acmode_len,
                  int filename_len );

#if USE_PTHREADS
static pthread_mutex_t foo_mutex = PTHREAD_MUTEX_INITIALIZER;
#endif

/****************************************************************************
 *
 * E M S 1 S T A R F
 *
 *  The C routine to do the work
 *
 ****************************************************************************
 */
int ems1Starf( const char *envar, const char *relpath, const char *acmode,
               char **filename, int *pathlen )
{
   char *dir;
   char *pathname;
   char *s;
   char *strtok_context = NULL;
   char *tmppath;
   int accno;
   int notfound;
   struct stat statb;

   /* Get the thread specific data character buffer to return the filename. */
   pathname = ems1Gthreadbuf();

   /* Initialise to file not found. */
   notfound = 1;

   /* Convert the access mode. */
   switch ( acmode[0] )  {
      case 'x' :
         accno = X_OK;
         break;
      case 'w' :
         accno = W_OK;
         break;
      case 'r' :
         accno = R_OK;
         break;
      case 'X' :
         accno = X_OK;
         break;
      case 'W' :
         accno = W_OK;
         break;
      case 'R' :
         accno = R_OK;
         break;
      default :
         accno = F_OK;
   }

   /* Check for user defined Env variable. Use mutex as getenv may return a
    * pointer to some static memory. */
#if USE_PTHREADS
    pthread_mutex_lock( &foo_mutex );
#endif
   if ( ( s = getenv( envar ) ) != NULL ) {

       /* If blank, initialize tmppath to a space, otherwise copy its value. */
       if ( strspn( s, " " ) == strlen( s ) ) {
           tmppath = (char *) starMalloc( 3 );
           if (tmppath) strcpy( tmppath, " :"  );
       } else {
           tmppath = (char *) starMalloc( strlen( s ) + 1 );
           if (tmppath) strcpy( tmppath, s );
       }
   } else {

       /* Env variable not given, use an empty search path. */
       tmppath = (char *) starMalloc( 3 );
       if (tmppath) strcpy( tmppath, " :" );
   }
#if USE_PTHREADS
        pthread_mutex_unlock( &foo_mutex );
#endif

if (!tmppath) {
     *pathlen = 0;
     return 0;
   }

   /*
    * tmppath is now the required search path (or blank if a given environment
    * variable was not defined).
    * For each item along it, if relpath is not blank, append / and relpath,
    * expand the result and check for the require access
    */
    for ( s = tmppath;
#if HAVE_STRTOK_R
          ( (dir = strtok_r( s, ":", &strtok_context ) ) != NULL ) && notfound;
#else
          ( (dir = strtok( s, ":" ) ) != NULL ) && notfound;
#endif
         s = NULL ) {

       (void) strcpy( pathname, dir );
       if ( strspn( pathname, " " ) != strlen( pathname ) ) {

           /* dir is not blank */
           if ( strspn( relpath," " ) != strlen( relpath ) ) {
               (void) strcat( pathname, "/" );
               (void) strcat( pathname, relpath );
           }
       } else {

           /* dir is blank, if relpath is also blank, set pathname to null
            * string */
           if ( strspn( relpath," ") != strlen( relpath ) ) {
               (void) strcpy( pathname, relpath );
           } else {
               *pathname = '\0';
           }
       }

       if ( !*pathname ) {
           continue;

       } else {
           notfound = access( pathname, accno );

           /* If a file is found, check it's not a directory. */
           if ( !notfound ) {
               if ( !stat( pathname, &statb ) ) {
		    if ( S_ISDIR(statb.st_mode) ) {
                       notfound = 1;
                   }
               }
           }
       }
   }

   if (tmppath) starFree( tmppath );

   if ( !notfound ) {
       *pathlen = strlen( pathname) - 1;
   } else {

       /* File was not found - set pathlen to 0 */
       *pathlen = 0;
   }

   /* Point filename at pathname, which will be a null string if no file was
    * found. */
   *filename = pathname;

   /* and return whether or not found */
   return !notfound;
}


/*************************************************************************
 *
 *  EMS1_CREIM( source_f, sourcelen )
 *
 *  Purpose:
 *     Create a temporary C string and import a FORTRAN string into it
 *     Returns a pointer to the temporary string
 *
 *  Note:
 *     This is a copy of the CNF routine. It is included here to avoid
 *     a dependency on CNF for the Conly system. But it may be system
 *     dependent.
 *
 *************************************************************************
 */
static char *ems1_creim( const char *source_f, int source_len )
{
    /* Local Variables:*/
    int i;			 /* Loop counter */
    char *ptr;			 /* Pointer to storage allocated */

    /* Locate the last non blank character in the input FORTRAN string. */
    for( i = source_len - 1 ; ( i >= 0 ) && ( source_f[i] == ' ' ) ; i-- )
        ;

    /* Allocate enough space for a copy of the input string. */
    ptr = (char *)starMalloc( (size_t)( i + 2 ) );

    /* If the space was allocated successfully, copy the input FORTRAN
     * string to it. */
    if( ptr != 0 ) {
        ptr[i+1] = '\0';

        for(  ; i >= 0 ; i-- ) {
            ptr[i] = source_f[i];
        }
    }

    return( ptr );
}

/***************************************************************************
 *
 * EMS_STARF_
 *
 * The Fortran interface to ems1Starf
 * Called from Fortran by:
 *
 * CALL EMS1_STARF( ENVAR, RELPATH, ACMODE, FILENAME, PATHLEN )
 *
 ***************************************************************************
 */
void ems1_starf_( const char *envar, const char *relpath, const char *acmode,
                  char *filename, int *pathlen,
                  int envar_len, int relpath_len, int acmode_len,
                  int filename_len )
{
    int index;
    char *envar_c;
    char *relpath_c;
    char *acmode_c;
    char *pfn;

    envar_c = ems1_creim( envar, envar_len );
    relpath_c = ems1_creim( relpath, relpath_len );
    acmode_c = ems1_creim( acmode, acmode_len );

    if ( ems1Starf( envar_c, relpath_c, acmode_c, &pfn, pathlen ) ) {
        (void)strncpy( filename, pfn, filename_len );
    } else {
        filename[0] = '\0';
    }

    /* Add trailing blank spaces for FORTRAN compatibility. */
    for ( index = strlen( filename ); index < filename_len; index++ ) {
        filename[index] = ' ';
    }
    starFree( envar_c );
    starFree( relpath_c );
    starFree( acmode_c );
}
