/*
*+
*  Name:
*     smf_open_textfile

*  Purpose:
*     Open a text file specified via an environment parameter.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     FILE *smf_open_textfile( const char *param, const char *mode,
*                              const char *def, int *status )

*  Arguments:
*     param = const char * (Given)
*        The name of the environment parameter to use.
*     mode = const char * (Given)
*        The mode with which to open the file. Can be any mode acceptable
*        to the "fopen" system routine.
*     def = const char * (Given)
*        A path to a file that is to be opened if the user enters a null
*        (!) value for the parameter. If the supplied pointer is NULL,
*        then an error is reported if the user enters a null value. If
*        the special value "<none>" is supplied, no error is reported if
*        a null (!) parameter value is supplied, but a NULL file descriptor
*        is returned.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This is like the system "fopen" routine, except the name of
*     the file to be opened is specified by an environment parameter.
*     The user is re-prompted if the specified file cannot be opened.

*  Authors:
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-JUN-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* System includes */
#include <stdio.h>
#include <string.h>
#include <errno.h>

/* Starlink includes */
#include "mers.h"
#include "par.h"
#include "star/one.h"
#include "sae_par.h"
#include "par_err.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* Maximum length of a file name */
#define MAXLEN 255

FILE *smf_open_textfile( const char *param, const char *mode, const char *def,
                         int *status ){

/* Local Variables */
   FILE *result = NULL;
   char filename[ MAXLEN + 1 ];
   int isdef;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* If a default file name was supplied, establish it as the default parameter
   value. */
   if( def ) parDef0c( param, def, status );

/* Loop until we have a file or an error occurs. */
   while( !result && *status == SAI__OK ) {

/* Get a value for the parameter. */
      parGet0c( param, filename, sizeof(filename), status );

/* If a null value was supplied, and we have a default file, annull the
   error and use the default file name. */
      if( *status == PAR__NULL && def ) {
         errAnnul( status );
         one_strlcpy( filename, def, sizeof(filename), status );
         isdef = 1;
      } else {
         isdef = 0;
      }

/* If the file name is "<none>" return a NULL file descriptor but do not
   report an error. */
      if( !strcmp( "<none>", filename ) ) break;

/* If no error occurred getting the file name, attempt to open it. */
      if( *status == SAI__OK ) {
         result = fopen( filename, mode );

/* If an error occurred, issue a suitable error message, and then flush
   the error to the user so what we can continue to get a new file name. */
         if( !result ) {
            *status = SAI__ERROR;
            msgSetc( "P", param );
            errRep( "", "Failed to open a text file using parameter ^P.",
                    status );

            msgSetc( "F", filename );

            if( mode[0] == 'r' ) {
               msgSetc( "A", "reading" );
            } else if( mode[0] == 'w' ) {
               msgSetc( "A", "writing" );
            } else if( mode[0] == 'a' ) {
               msgSetc( "A", "appending" );
            } else {
               msgSetc( "A", "unknown access" );
            }

            msgSetc( "M", mode );

            if( isdef ) {
               msgSetc( "W", "default" );
            } else {
               msgSetc( "W", "" );
            }

            errRep( "", "Supplied ^W file (^F) "
                    "cannot be opened for ^A (mode ^M):", status );

            errRep( "", strerror( errno ), status );

            errFlush( status );
         }
      }

/* Cancel the parameter if a new value is required. */
      if( !result ) parCancl( param, status );

   }

/* Return the file pointer. */
   return result;
}
