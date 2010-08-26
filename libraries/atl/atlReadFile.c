#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include <stdio.h>

static const char *atlSource1( void );

/* A structure that contains information needed by the source function. */
typedef struct SourceData {
   FILE *fd;
   char *buf;
   int buflen;
} SourceData;


AstObject *atlReadFile( const char *fname, const char *options,
                        int *status ) {
/*
*+
*  Name:
*     atlReadFile

*  Purpose:
*     Reads an AST Object from a text file.

*  Language:
*     C.

*  Invocation:
*     AstObject *atlReadFile( const char *fname, const char *options,
*                             int *status )

*  Description:
*     This function creates an AST Object by reading the contents of a
*     given text file. The file should contain a dump of an AST Object
*     such as produced by the "atlShow" function.

*  Arguments:
*     fname
*        The file name.
*     options
*        Optional attribute settings for the Channel used to read the file.
*     status
*        Pointer to the global status variable.

*  Returned Value:
*     A pointer to the Object read form the file, or NULL if no Object
*     could be read. A NULL pointer is also returned if an error occurs.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     26-AUG-2010 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstChannel *ch;        /* Pointer to the Channel */
   AstObject *result;     /* Returned pointer */
   FILE *fd;              /* File descriptor for output file */
   SourceData sd;         /* Data to pass to the source function */
   int *old_status;       /* Pointer to original status variable */
   int c;                 /* Current character read from file */
   int linelen;           /* Length of current line of text */
   int maxlen;            /* Length of longest line of text */

/* Initialise */
   result = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Open the file */
   fd = fopen( fname, "r" );

/* Report an error if the file could not be opened. */
   if( !fd ) {
      *status = SAI__ERROR;
      msgSetc( "F", fname );
      errRep( "", "atlReadFile: Failed to open input text file: \"^F\".",
              status );

/* Otherwise, create a Channel. */
   } else {
      ch = astChannel( atlSource1, NULL, options );

/* Read all characters from the file, and find the length of the longest
   line. */
      linelen = 0;
      maxlen = 0;
      while( ( c = fgetc( fd ) ) != EOF ) {
         linelen++;
         if( c == '\n' ) {
            if( linelen > maxlen ) maxlen = linelen;
            linelen = 0;
         }
      }
      if( linelen > maxlen ) maxlen = linelen;

/* Rewind the file, so that we can read it again from the start. */
      rewind( fd );

/* For safety, increase the max line length by a few. */
      maxlen += 5;

/* Allocate a buffer (inclusing room for a terminating null), and store
   all the information needed by the source function. */
      sd.buf = astMalloc( maxlen + 1 );
      sd.buflen = maxlen;
      sd.fd = fd;

/* Store the file descriptor in the Channel. */
      astPutChannelData( ch, &sd );

/* Read the Object from the Channel. */
      result = astRead( ch );

/* Annull the Channel. */
      ch = astAnnul( ch );

/* Clear the source data structure. */
      sd.buf = astFree( sd.buf );
      sd.buflen = 0;
      sd.fd = NULL;

/* Close the output file. */
      fclose( fd );
   }

/* Make AST use its original status variable. */
   astWatch( old_status );

/* Return the Object pointer. */
   return result;
}

/* Source function for reading a line of text from the input file. */
static const char *atlSource1( void ){
   SourceData *sd;

/* Get the a pointer to the structure holding the data needed by this
   source function. Do not use static global variables to pass this data
   since that would not be thread-safe. */
   sd = astChannelData;

/* Read a line from the file and store it in the buffer. Return a pointer
   to the buffer. */
   return fgets( sd->buf, sd->buflen + 1, sd->fd );

}


