/*
*  Name:
*     ems1Starf

*  Purpose:
*     To find a file in the "/star" hierarchy with the specified access
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
*        The filename.
*     pathlen = *int (Returned)
*        The length of filename which is directory spec.
*     status (Fortran version only) = INTEGER (Returned)
*     

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1994 (AJC):
*        Original version.
*      4-AUG-1995 (AJC):
*        Remove removal of ..
*      1-JUN-1999 (AJC):
*        Rename C interface to ems1Starf
*      23-FEB-2006 (TIMJ):
*        Use starMem
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Include Statements: */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "star/mem.h"

/****************************************************************************
 *
 * E M S 1 S T A R F
 *
 *  The C routine to do the work
 *
 ****************************************************************************
 */
int ems1Starf( char *envar, char *relpath, char *acmode, 
              char **filename, int *pathlen )
{
   int notfound;
   int accno;
   char *s;
   char *tmppath;
   char *dir;
   static char pathname[200];
   struct stat statb;

/* Initialise to file not found
*/
   notfound = 1;

/* Convert the access mode 
*/
   switch (acmode[0])  {
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
/*
 * If 'envar' is not given, use the default search path.
 * $INSTALL:$STARLINK. If $INSTALL is not defined, we attempt to get a value
 * from ~/.star_config, failing that we try $HOME/star: 
 * if $STARLINK is not defined, we attempt to get a value from ~/.star_config,
 * failing that we try /star.
 */
   if ((s = getenv(envar)) != NULL) {	/* user defined search path */
      if ( strspn( s, " " ) == strlen(s) ) {   /* defined but blank */
/* Env variable is defined. If it's blank, initialize tmppth to space;
 * otherwise copy its value int tmppth.
 */
         tmppath = (char *) starMalloc(3);
         (void) strcpy( tmppath, " :" );
      } else {
         tmppath = (char *) starMalloc(strlen(s) + 1);
         (void) strcpy(tmppath, s);
      }
   } else {
         tmppath = (char *) starMalloc(3);
         (void) strcpy( tmppath, " :" );
   }

/* 
 * tmppath is now the required search path (or blank if a given environment
 * variable was not defined).
 * For each item along it, if relpath is not blank, append / and relpath,
 * expand the result and check for the require access
 */
   for (s = tmppath; 
       ((dir = strtok(s, ":")) != NULL) && notfound;
       s = NULL) {
      (void) strcpy(pathname, dir);
      if ( strspn(pathname," ") != strlen(pathname) ) {
/*
 * dir is not blank
 */
         if (strspn( relpath," ") != strlen(relpath) ) {
            (void)strcat( pathname, "/" );
            (void)strcat( pathname, relpath );
         }
      } else {
/*
 * dir is blank
 * if relpath is also blank, set pathname to null string
 */
         if ( strspn(relpath," ") != strlen(relpath) ) {
            (void) strcpy( pathname, relpath );
         } else {
            *pathname = '\0';
         }
      }

      if (!*pathname) {
         continue;

      } else {
         notfound = access ( pathname, accno );
/*
 * If a file is found, check it's not a directory.
 */
        if ( !notfound ) {
            if ( !stat( pathname, &statb ) ) {
               if ( statb.st_mode & S_IFDIR ) {
                  notfound = 1;
               }
            }
         }
      }
   }

   starFree(tmppath);

   if ( !notfound ) {
      *pathlen = strlen(pathname) - 1;
   } else {
/* File was not found - set pathlen to 0
 */
      *pathlen = 0;
   }
/* Point filename at pathname, which will be a null string if no file was found
 */
   *filename = pathname;

/* and return whether or not found
 */
   return !notfound;
}


/*************************************************************************
 *
 *  E M S 1 _ C R E I M ( source_f, sourcelen )
 *
 *  Purpose:
 *     Create a temporary C string and import a FORTRAN string into it
 *     Returns a pointer to the temporary string
 *
 *  Note:
 *     This is a copy of the CNF routine. It is included here to avoid
 *     a dependency on CNF. But it may be system dependent.
 *
 *************************************************************************
 */
static char *ems1_creim( char *source_f, int source_len )
{
/* Local Variables:							    */

   int i;			 /* Loop counter			    */
   char *ptr;			 /* Pointer to storage allocated	    */


/* Locate the last non blank character in the input FORTRAN string.	    */

   for( i = source_len - 1 ; ( i >= 0 ) && ( source_f[i] == ' ' ) ; i-- )
      ;

/* Allocate enough space for a copy of the input string.		    */

   ptr = (char *)starMalloc( (size_t)( i + 2 ) );

/* If the space was allocated successfully, copy the input FORTRAN string   */
/* to it.								    */

   if( ptr != 0 )
   {
      ptr[i+1] = '\0';

      for(  ; i >= 0 ; i-- )
         ptr[i] = source_f[i];
   }

   return( ptr );
}

/***************************************************************************
 *
 * E M S _ S T A R F _
 *
 * The Fortran interface to ems1Starf
 * Called from Fortran by:
 *
 * CALL EMS1_STARF( ENVAR, RELPATH, ACMODE, FILENAME, PATHLEN )
 *
 ***************************************************************************
 */
void ems1_starf_( char *envar, char *relpath, char *acmode, 
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
