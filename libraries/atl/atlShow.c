#include "ast.h"
#include "mers.h"
#include "sae_par.h"
void atlSink1( const char * );

void atlShow( AstObject *this, const char *fname, const char *options,
              int *status ) {
/*
*+
*  Name:
*     atlShow

*  Purpose:
*     Dumps an AST Object to a text file.

*  Language:
*     C.

*  Invocation:
*     void atlShow( AstObject *this, const char *fname, const hcar *options,
*                   int *status )

*  Description:
*     This function dumps the supplied AST Object to a new text file with
*     the given name. It may be read back from the file using astReadFile.

*  Arguments:
*     this
*        A pointer to the Object.
*     fname
*        The file name.
*     options
*        Optional attribute settings for the Channel used to create the
*        dump.
*     status
*        Pointer to the global status variable.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     11-DEC-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstChannel *ch;        /* Pointer to the Channel */
   int *old_status;       /* Pointer to original status variable */
   FILE *fd;              /* File descriptor for output file */

/* Check the inherited status. */
   if( *status != SAI__OK ) return;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Open the file */
   fd = fopen( fname, "w" );

/* Report an error if the file could not be opened. */
   if( !fd ) {
      *status = SAI__ERROR;
      msgSetc( "F", fname );
      errRep( "", "atlShow: Failed to open output text file: \"^F\".",
              status );

/* Otherwise, create a Channel. */
   } else {
      ch = astChannel( NULL, atlSink1, options );

/* Store the file descriptor in the Channel. */
      astPutChannelData( ch, fd );

/* Write out the Object to the Channel. */
      astWrite( ch, this );

/* Annull the Channel. */
      ch = astAnnul( ch );

/* Close the output file. */
      fclose( fd );
   }

/* Make AST use its original status variable. */
   astWatch( old_status );

}

/* Sink function for writing out a link of text to the output file. */
void atlSink1( const char *text ){
   FILE *fd;

/* Get the file descriptor from the Channel. Do not use a static
   global variable to pass the file descriptor since that would not be
   thread-safe. */
   fd = astChannelData;

/* Write out the text. */
   fprintf( fd, "%s\n", text );

}


