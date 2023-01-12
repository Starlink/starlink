/*
*+
*  Name:
*     subpar_mkdir

*  Purpose:
*     To create the specified directory - equivalent to shell command
*     mkdir -p directory

*  Language:
*     C

*  Invocation:
*     int subpar_admus( char *dir )

*  Description:
*     Creates the required directory if possible. No error is reported if
*     the directory exists already. Any intermediate directories which are
*     created have permission u+wx regardless of the user's umask value.
*     In the event of a failure error messages are reported using EMS and
*     the function value SAI__ERROR returned.

*  Arguments:
*     dir = *char (Given)
*        String containing the directory name

*  Returned value:
*     The function returns SAI__OK if successful and SAI__ERROR if it fails.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     {enter_new_authors_here}

*  History:
*     10-MAY-1995 (AJC):
*        Original version.
*     {enter_changes_here}


*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "sae_par.h"
#include "ems.h"
#include "ems_par.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "subpar1.h"

int subpar_mkdir( char *dir )
{
char *buff;
int status;
char path[200];
char *s;
char *tmpdir;
mode_t mode;
struct stat statb;

/* Add final / to dir to ensure at least once through the for loop */
   if ( ( buff = (char *)malloc ( strlen(dir) + 2 ) ) != NULL ) {
      strcpy( buff, dir );
      strcat( buff, "/" );
      status = SAI__OK;

/* If buff begins with / copy it to path as strtok will ignore it */
      if ( *buff == '/' ) {
         strcpy( path, "/" );
      } else {
         *path = '\0';
      }
/* Now for each component ensure the directory exists */
      for ( s = buff;
          ( ( ( tmpdir = strtok( s, "/" ) ) != NULL ) && !status );
          s = NULL ) {

          strcat( path, tmpdir );
          if ( !stat( path, &statb ) ) {
/* File exists check it's a directory */
             if ( ! ( statb.st_mode & S_IFDIR ) ) {
/* Not a directory */
                status = SAI__ERROR;
                emsSetnc("PATH", path, EMS__SZTOK);
                emsRep("MKDIR1",
                          "mkdir error: ^PATH exists and is not a directory",
                           &status );
             }
          } else {
/* Failed to get file info
 * Maybe because the file doesn't exist
 * Try to make the directory anyway
 */
             if ( mkdir ( path, S_IRWXU | S_IRWXG | S_IRWXO ) ) {
/*         error in creating dir */
                status = SAI__ERROR;
                emsSyser("ERRNO", errno );
                emsRep("MKDIR2",
                          "^ERRNO", &status );
                emsSetnc("PATH", path, EMS__SZTOK);
                emsRep("MKDIR3",
                          "mkdir error: creating ^PATH", &status );
             } else {
                stat( path, &statb );
                mode = statb.st_mode & 07777;
                chmod( path, mode | S_IRWXU );
             }
          }
          strcat( path, "/" );
      }
      free( buff );

   } else {
/* malloc failed */
      status = SAI__ERROR;
      emsSyser("ERRNO", errno );
      emsRep("MKDIR4",
                "^ERRNO", &status );
   }

   return status;

}
