/*
      SUBROUTINE SUBPAR_ADMUS( ADMUSR, AULEN, STATUS )
*+
*  Name:
*     SUBPAR_ADMUS

*  Purpose:
*     To obtain a string defining the ADAM_USER directory

*  Language:
*     C (Fortran callable)

*  Invocation:
*     CALL SUBPAR_ADMUS( ADMUSR, STATUS )

*  Description:
*     This is the UNIX version.
*     The routine translates environment variable ADAM_USER
*     If there is no translation, environment variable HOME is translated
*     and subdirectory /adam of it is used.
*     If neither of these are successful, a null string is returned.

*  Arguments:
*     ADMUSR = CHARACTER*(*) (Returned)
*        String containing the definition of the ADAM_USER directory
*        (including a terminating /).
*     AULEN = INTEGER (Returned)
*        The used length of the string
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1995, 1998 Central Laboratory of the Research Councils.
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
*     14-DEC-1995 (AJC):
*        Include stdlib.h for getenv (required by LINUX).
*     27-FEB-1998 (AJC):
*        Trap $HOME not defined
*     {enter_changes_here}


*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "sae_par.h"
#include "f77.h"
#include "cnf.h"
#include "ems.h"
#include "ems_par.h"
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "subpar1.h"

F77_SUBROUTINE(subpar_admus)( CHARACTER(admus), INTEGER(aulen), INTEGER(status)
                           TRAIL(admus) )
{
GENPTR_CHARACTER(admus)
GENPTR_INTEGER(aulen)
GENPTR_INTEGER(status)

char buffer[200];
char *tmp;
char output[201];
int *nchars;
struct stat statb;

/*  Check inherited global status. */
   if (*status) return;

/*  Attempt the translation of ADAM_USER.
 *  If it didn't work, use ~/adam
 */
   if ( ( tmp = getenv( "ADAM_USER" ) ) != NULL ) {
      strcpy( buffer, tmp );
   } else if ( ( tmp = getenv( "HOME" ) ) != NULL ) {
      strcpy( buffer, tmp );
      strcat( buffer, "/adam" );
   }

   if ( tmp != NULL ) {
      *aulen = strlen( buffer );

/*  Ensure that the directory exists
 */
      if ( !stat ( buffer, &statb ) ) {
/* File exists - check it's a directory */
         if ( ! ( statb.st_mode & S_IFDIR ) ) {
/* Not a directory */
            *status = SAI__ERROR;
            emsRep("ADMUS1",
                   "Failed to create ADAM_USER directory", status );
            emsSetnc("PATH", buffer, EMS__SZTOK);
            emsRep("ADMUS2",
                   "^PATH exists and is not a directory", status );
         }
      } else {
/* Failed to get file info
 * Maybe because the file doesn't exist
 * Try to make the directory anyway
 */
         if ( subpar_mkdir( buffer ) ) {
            *status = SAI__ERROR;
            emsRep("ADMUS3","Failed to create ADAM_USER directory",status);
            emsSetnc( "DIR", buffer, EMS__SZTOK );
            emsRep("ADMUS4", "^DIR", status);
         }
      }
      if( *(buffer + *aulen -1) != '/' ) {
         *( buffer + (*aulen)++ ) = '/';
         *(buffer + *aulen ) = '\0';
      }
/* Export the name to Fortran
 */
      cnf_exprt( buffer, admus, admus_length );

   } else {
/* Failed to translate environment variables
 */
      buffer[0] = '\0';
      *status = SAI__ERROR;
      emsRep("ADMUS3","Failed to create ADAM_USER directory",status);
      emsRep("ADMUS4", "Neither $ADAM_USER nor $HOME are defined", status);
   }
}
