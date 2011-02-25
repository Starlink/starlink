#include <stdio.h>
#include <errno.h>

#include "atl.h"
#include "ast.h"
#include "sae_par.h"

void atlDumpFits( const char *param, AstFitsChan *fc, int *status ){
/*
*+
*  Name:
*     atlDumpFits

*  Purpose:
*     Write the contents of a FitsChan to a text file.

*  Language:
*     C.

*  Invocation:
*     void atlDumpFits( const char *param, AstFitsChan *fc, int *status )

*  Description:
*     This function creates a new text file containing the contents of
*     the supplied FitsChan as a set of FITS header cards. The name of
*     the text file is obtained via the evironment using a specified
*     parameter.

*  Arguments:
*     param
*        The parameter name.
*     fc
*        A pointer to the FitsChan.
*     status
*        Pointer to the global status variable.

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
*     DSB: David S. Berry  (JAC, HAwaii)
*     {enter_new_authors_here}

*  History:
*     25-FEB-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   FILE *fd;                      /* Output file descriptor */
   char card[ 81 ];               /* Formatted header card */
   char fpath[ 255 ];             /* Output file path */
   int *old_status;               /* Original AST status pointer */
   int icard;                     /* Index of current card in FitsChan */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status pointer. */
   old_status = astWatch( status );

/* Loop until we have opened a file successfully, or an error has
   occurred. */
   fd = NULL;
   while( !fd ) {

/* Get the name of the text file. Break out of the loop if an error
   occurred. */
      parGet0c( param, fpath, sizeof( fpath ), status );
      if( *status != SAI__OK ) break;

/* Attempt to open the file for writing. */
      fd = fopen( fpath, "w" );

/* If the file could not be opened, issue a suitable error message, and
   then flush the error to the user so what we can continue to get a new
   file name. */
      if( !fd ) {
         *status = SAI__ERROR;
         msgSetc( "P", param );
         errRep( "", "Failed to open a text file using parameter ^P.",
                 status );

         msgSetc( "F", fpath );
         errRep( "", "Supplied file (^F) cannot be opened for writing:", status );
         errRep( "", strerror( errno ), status );
         errFlush( status );

/* Cancel the parameter if a new value is required. */
         parCancl( param, status );
      }
   }

/* If we have opened a file successfully... */
   if( fd ) {

/* Save the original current card index and then rewind the
   fitschan. */
      icard = astGetI( fc, "Card" );
      astClear( fc, "Card" );

/* Loop round all headers in the FitsChan. */
      while( astFindFits( fc, "%f", card, 1 ) ){

/* Terminate the card to omit trailing shite space */
         card[ astChrLen( card ) ] = 0;

/* Write the formatted header card to the output text file. */
         fprintf( fd, "%s\n", card );
      }

/* Re-instate the original current card index in the FitsChan. */
      astSetI( fc, "Card", icard );

/* Close the file. */
      fclose( fd );
   }

/* Revert to using the old AST status pointer. */
   astWatch( old_status );
}

