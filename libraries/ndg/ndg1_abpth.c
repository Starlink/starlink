#include <unistd.h>

#include "f77.h"
#include "cnf.h"
#include "star/grp.h"
#include "sae_par.h"

static void removeJunk(char *, char *);

F77_SUBROUTINE(ndg1_abpth)( CHARACTER(PATH), CHARACTER(ABPATH),
                            F77_INTEGER_TYPE *STATUS TRAIL(PATH)
                            TRAIL(ABPATH) ) {
/*
*+
*  Routine:
*     NDG1_ABPTH

*  Purpose:
*     Expand a relative path name into an absolute path name.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL NDG1_ABPTH( PATH, ABPATH, STATUS )

*  Description:
*     This routine returns expans the supplied path name into an absolute
*     pathname relative to the current working directory. It assumes Unix
*     file paths.

*  Arguments:
*     PATH = CHARACTER*(GRP__SZNAM) (Given)
*        The supplied path name.
*     ABPATH = CHARACTER*(GRP__SZNAM) (Returned)
*        The absolute path name.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (STARLINK)

*  History:
*     11-OCT-2012 (DSB):
*        Original version.

*-
*/

/* Local Variables */
   char path[ GRP__SZNAM + 1 ];
   char abpath[ GRP__SZNAM + 1 ];
   char *s;
   char *priorSlash;
   int lin;
   int lout;

/* Check the global status. */
   if( *STATUS != SAI__OK ) return;

/* Import the supplied path. */
   cnfImpn( PATH, PATH_length, GRP__SZNAM,  path );

   if (path[0]=='/') {
      strcpy(abpath, path);
   } else {
      getcwd(abpath, GRP__SZNAM);
      strcat(abpath, "/");
      strcat(abpath, path);
   }

   while( (s=strstr(abpath, "/../")) ) {
      *s = 0;
      if( !(priorSlash = strrchr(abpath, '/')) ) {
         *s ='/';
         break;
      }
      removeJunk( priorSlash, s + 3 );
   }

   while( ( s = strstr(abpath, "/./")) ) removeJunk( s, s + 2 );
   while( ( s = strstr(abpath, "//")) ) removeJunk( s, s + 1 );
   s = abpath + ( strlen(abpath) - 1 );
   if( s != abpath && *s == '/') *s=0;

/* If the supplied path ended with a slash, ensure the returned path ends
   with a slash. */
   lin = strlen( path );
   lout = strlen( abpath );
   if( path[ lin - 1 ] == '/' && lout < GRP__SZNAM ) {
      if( abpath[ lout - 1 ] != '/' ) {
         abpath[ lout ] = '/';
         abpath[ lout + 1 ] = 0;
      }

/* If the supplied path does not end with a slash, ensure the returned
   path does not end with a slash. */
   } else if( path[ lin - 1 ] != '/' ) {
      if( abpath[ lout - 1 ] == '/' ) {
         abpath[ lout ] = '/';
         abpath[ lout ] = 0;
      }
   }

/* Export returned absolute path */
   cnfExpn( abpath, GRP__SZNAM, ABPATH, ABPATH_length );
}


static void removeJunk(char *begin, char *end) {
    while( *end ) *begin++ = *end++;
    *begin = 0;
}










