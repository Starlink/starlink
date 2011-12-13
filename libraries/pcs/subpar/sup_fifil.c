/*
*+
*  Name:
*     SUBPAR_FIFIL

*  Purpose:
*     To search along a given path for a NON-DIRECTORY file with a given
*     name and one of a possible choice of extensions and the specified
*     access permission: 'r' read, 'w' write and 'x' execute.
*     This is the Unix version - written in C and expected to be called
*     from Fortran.

*  Language:
*     C

*  Invocation:
*     CALL SUBPAR_FIFIL( PATH, NAME, EXT, ACC, FILE, IND, STATUS )

*  Description:
*     For each directory in the given PATH, an attempt is made to access,
*     in the given access mode, a file with the given NAME and each of
*     the given extensions in turn. If the access is successful, the file
*     is checked to see if it is a directory. If it is a directory, it is
*     ignored, otherwise its full name and an index to indicate which
*     extension was successful, are returned.

*  Arguments:
*     PATH = CHARACTER*(*) (Given)
*        The name of an environment variable specifying the path
*     NAME = CHARACTER*(*) (Given)
*        The name of the file
*     EXT = CHARACTER*(*) (Given)
*        A list of up to 5 extensions separated by !
*        The extensions are just appended to the name so any required
*        '.' must be included.
*        No-extension is indicated by a space- if it is the last
*        in the list, it must be terminated by !
*     ACC = CHARACTER*(*) (Given)
*        The specified access code
*     FILE = CHARACTER*(*) (Returned)
*        The full name of the file found
*     IND = INTEGER (Returned)
*        The number of the extension within the EXT list which
*        corresponds with the file found
*     STATUS = INTEGER (Given and returned)
*        The global status.

*  [optional_subroutine_items]...
*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council.
*     Copyright (C) 2002 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     AJC: A J Chipperfield (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1993 (AJC):
*        Original C version.
*      9-FEB-1993 (AJC):
*        Correct checking of access mode
*     10-SEP-2002 (AJC):
*        Free C-from-Fortran strings
*     20-SEP-2005 (TIMJ):
*        Update CNF usage. Fix cnfFree vs free usage
*     13-FEB-2006 (TIMJ):
*        Use cnfFree again since this is easier to control when changing
*        malloc library.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Include Statements: */
#include "sae_par.h"
#include "cnf.h"
#include "f77.h"
#include "ems.h"
#include "ems_par.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

/* Global Constants: */
#include "subpar_err.h"

/* Function Definition: */
F77_SUBROUTINE(subpar_fifil)(CHARACTER(path), CHARACTER(name), CHARACTER(ext),
                             CHARACTER(acc), CHARACTER(file), INTEGER(ind),
                             INTEGER(status) TRAIL(path) TRAIL(name)
                             TRAIL(ext) TRAIL(acc) TRAIL(file) )
{
GENPTR_CHARACTER(path)
GENPTR_CHARACTER(name)
GENPTR_CHARACTER(ext)
GENPTR_CHARACTER(acc)
GENPTR_CHARACTER(file)

/* Local Variables:
*/
char *path_c;
char *name_c;
char *ext_c;
char *acc_c;
char *tmpext;
char *tpath;
char *tmppath;
char *exts[5];
char *dir;
char *tmpdir;
char *tmphome;
char *tmpname;
int nexts;
int accno;
int notfound;
int namlen;
struct stat statb;
/* .
*/

/* Exit if bad given status
*/
   if ( *status != SAI__OK ) return;

/* Initialise to file not found
*/
   notfound = 1;

/* Convert given strings to C strings
*/
   path_c = cnfCreim( path, path_length );
   name_c = cnfCreim( name, name_length );
   ext_c = cnfCreim( ext, ext_length );
   tmpext = malloc( strlen(ext_c) + 1 );
   acc_c = cnfCreim( acc, acc_length );

   if ( ( path_c != NULL )
     && ( name_c != NULL )
     && ( ext_c != NULL )
     && ( acc_c != NULL )
     && ( tmpext != NULL ) ) {

/* Get an array of pointers to the extensions into the exts array
   Leaves nexts = number of possible extensions
*/
      strcpy( tmpext, ext_c );

      exts[nexts=0] = strtok( tmpext, "!" );
      for ( nexts = 1; ( exts[nexts-1] != NULL ) && ( nexts < 5 ) ; nexts++ )
         exts[nexts] = strtok( NULL, "!" );

/* Adjust nexts for case where termination is by null token ( <5 extensions ).
*/
         if ( ( nexts-1 ) && ( exts[nexts-1] == NULL ) ) nexts--;

/* Now convert any extensions consisting entirely of spaces to NULL pointers
*/
      for ( *ind=0; *ind<nexts; (*ind)++ )
         if ( exts[*ind] != NULL )
            if ( strspn( exts[*ind], " " ) == strlen( exts[*ind] ) )
                exts[*ind] = NULL;

/* Get size of filename
*/
      namlen = strlen( name_c );

/* Convert the access mode
*/
      switch (acc_c[0])
         {
         case 'x' :
            accno = X_OK;
            break;
         case 'w' :
            accno = W_OK;
            break;
         case 'r' :
            accno = R_OK;
            break;
         default :
            accno = F_OK;
         }

/* Get the path - if there is no translation, get a null string
*/
      if ( ( tpath = getenv(path_c) ) != NULL )
         {
         tmppath = malloc( strlen(tpath) + 1 );

/* If a string has been allocated for the path, search it
*/
         if ( tmppath )
            {
            strcpy(tmppath,tpath);

/* For each directory on the path -
*/
            for ( dir=strtok(tmppath,":");
                  ( *status == SAI__OK ) && notfound && ( dir != NULL );
                  dir=strtok(NULL,":") )
               {

/* If the directory name begins ~, substitute the translation of HOME
*/
               if ( *dir == '~' )
                  {
                  if ( tmpdir = malloc( strlen( tmphome = getenv("HOME") )
                           + strlen(dir) + 1 ) )
                     {
                     strcpy( tmpdir, tmphome );
                     strcat( tmpdir, dir+1 );
                      }
                  }
               else
                  if ( tmpdir = malloc( strlen( dir ) + 1 ) )
                     strcpy( tmpdir, dir );


/* If tmpdir allocated
   then for each extension in turn look for a file with the given directory,
   filename and extension, and with the given access mode.
   Set found TRUE if file is found.
*/
               if ( tmpdir )
                  {
                  for ( *ind=0;
                        ( *status == SAI__OK ) && notfound &&
                          ( !ind || (*ind<nexts) );
                        (*ind)++ )
                     {
                     if ( tmpname=malloc
                        ( strlen(tmpdir) + namlen
                          + ( ( exts[*ind] != NULL ) ? strlen(exts[*ind]) : 0 )
                          + 2 ) )
                        {
                        strcpy( tmpname, tmpdir );
                        strcat( tmpname,"/");
                        strcat( tmpname, name_c );
                        if( exts[*ind] != NULL )
                           strcat( tmpname, exts[*ind] );
                        if ( !(notfound=access( tmpname, accno )) )
                           {
/*       Check it's not a directory
         If stat fails don't do the check
*/
                           if ( !stat( tmpname, &statb ) )
                              {
                              if( statb.st_mode & S_IFDIR )
                                 notfound = 1;
                              else
                                 cnfExprt( tmpname, file, file_length);
                              }
                           }
                        free( tmpname );
                        }
                     else
                        {
                        *status = SUBPAR__IFNF;
                        emsRep("SUP_FIFIL1",
                        "malloc failed for name construction", status );
                        }
                     }
                   free( tmpdir );
                  }
               else
/* Failed to malloc for directory construction
*/
                  {
                  *status = SUBPAR__IFNF;
                  emsRep("SUP_FIFIL2",
                  "malloc failed for directory construction", status );
                  }
               }
            free( tmppath );
            }
         else
            {
/* Failed to malloc for path
*/
            *status = SUBPAR__IFNF;
            emsSetnc( "PATH", path_c, EMS__SZTOK);
            emsRep("SUP_FIFIL3",
            "malloc failed for ^PATH translation", status );
            }
         }
      else
         {
         *status = SUBPAR__IFNF;
         emsSetnc( "PATH", path_c, EMS__SZTOK);
         emsRep("SUP_FIFIL4",
         "Environment variable ^PATH not defined", status );
         }
      }
   else
      {
      *status = SUBPAR__IFNF;
      emsRep("SUP_FIFIL5",
      "Failed importing arguments for FIFIL", status );
      }

   if ( notfound )
      {
      *status = SUBPAR__IFNF;
      emsSetnc( "NAME", name_c, EMS__SZTOK);
      emsSetnc( "EXT", ext_c, EMS__SZTOK);
      emsSetnc( "PATH", path_c, EMS__SZTOK);
      emsRep("SUP_FIFIL6",
      " SUBPAR: Failed to find file ^NAME^EXT on path ^PATH", status );
      }

/* Free the imported C strings etc. - works OK even if they weren't allocated
*/
   cnfFree( path_c );
   cnfFree( name_c );
   cnfFree( ext_c );
   free( tmpext );
   cnfFree( acc_c );

   return;
   }

