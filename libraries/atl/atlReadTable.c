#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>

AstTable *atlReadTable( const char *fname, int *status ) {
/*
*+
*  Name:
*     atlReadTable

*  Purpose:
*     Create an AST Table from a text file.

*  Language:
*     C.

*  Invocation:
*     AstTable *atlReadTable( const char *fname, int *status )

*  Description:
*     This function creates an AST Table by reading the contents of a
*     given text file. The text file format matches that of TOPCAT's
*     "ASCII" format, except that any comment lines before the first row
*     of column values that are of the form "# name = value" or
*     "! name = value" are used to create Table parameters. Here "name"
*     is a contiguous block of alphanumeric characters, and "value"
*     represents all characters following the equals sign, up to the end
*     of the line, excluding leading and trailing white space. If value
*     is a scalar integer or double, it is stored as such in the Table.
*     Otherwise, it is stored as a string.

*  Arguments:
*     fname
*        The file name.
*     status
*        Pointer to the global status variable.

*  Returned Value:
*     A pointer to the Table read from the file, or NULL if an error occurs.

*  Notes:
*     - All columns in the returned Table will hold scalar values.

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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-MAY-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstTable *result;
   FILE *fd;
   char **cols;
   char **words;
   char *last_com;
   char *line;
   char *p;
   char key[ 200 ];
   double dval;
   int *old_status;
   int c;
   int first;
   int icol;
   int iline;
   int irow;
   int ival;
   int iword;
   int line_len;
   int max_line_len;
   int more;
   int nc;
   int ncol;
   int nword;
   int skip;
   int type;
   size_t len;

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
      errRep( "", "atlReadTable: Failed to open input text file: \"^F\".",
              status );

/* Otherwise... */
   } else {

/* Create an empty Table. */
      result = astTable( " " );

/* Allocate a buffer for one one line of text. This will be increased in
   size as required. */
      max_line_len = 80;
      line = astMalloc( max_line_len*sizeof( *line ) );

/* Read each line of text from the file. */
      skip = 1;
      line_len = 0;
      more = 1;
      last_com = NULL;
      cols = NULL;
      first = 1;
      iline = 0;
      irow = 0;

      while( more && *status == SAI__OK ) {
         iline++;
         line_len = 0;

/* Loop reading characters from the file until a newline or the end of file is
   reached. */
         while( ( c = fgetc( fd ) ) != EOF && c != '\n' ) {

/* Increment the current line length, and double the size of the line buffer
   if it is full. */
            if( ++line_len >= max_line_len ) {
               max_line_len *= 2;
               line = astRealloc( line, max_line_len*sizeof( *line ) );
               if( *status != SAI__OK ) break;
            }

/* Store the character. Ignore leading white space. */
            if( skip ) {
               if( ! isspace( c ) ) {
                  line[ line_len - 1 ] = c;
                  skip = 0;
               } else {
                  line_len--;
               }
            } else {
               line[ line_len - 1 ] = c;
            }

         }

/* If the end-of-file was reached indicate that we should leave the main
   loop after processing the current line. */
         if( c == EOF ) more = 0;

/* Terminate the line. */
         line[ line_len ] = 0;

/* Terminate it again to exclude trailing white space. */
         line[ astChrLen( line ) ] = 0;

/* Skip blank lines. */
         if( line[ 0 ] ) {

/* If the line starts with a comment character... */
            if( line[ 0 ] == '#' || line[ 0 ] == '!' ) {

/* Get a pointer to the first non-space/tab character after the comment
   character. */
               p = line + 1;
               while( *p == ' ' || *p == '\t' ) p++;

/* Skip blank comment lines. */
               if( *p ) {

/* Does it look like a  parameter assignment... */
                  words = astChrSplitRE( p, "^\\w+ *= *(.*)$", &nword, NULL );
                  if( words ) {

/* Add a parameter declaration to the Table. */
                     astAddParameter( result, words[ 0 ] );

/* Store the parameter value, using an appropriate data type. */
                     len = strlen( words[ 1 ] );
                     if( nc = 0, ( 1 == astSscanf( words[ 1 ], "%d%n", &ival, &nc ) )
                         && ( nc >= len ) ) {
                        astMapPut0I( result, words[ 0 ], ival, NULL );

                     } else if( nc = 0, ( 1 == astSscanf( words[ 1 ], "%lg%n", &dval, &nc ) )
                                && ( nc >= len ) ) {
                        astMapPut0D( result, words[ 0 ], dval, NULL );

                     } else {
                        astMapPut0C( result, words[ 0 ], words[ 1 ], NULL );

                     }

/* Free the words returned by astChrSplitRE. */
                     for( iword = 0; iword < nword; iword++ ) {
                        words[ iword ] = astFree( words[ iword ] );
                     }
                     words = astFree( words );

/* If it does not look like a parameter assignment... */
                  } else {

/* Save a copy of it in case it turns out to be the last comment line
   before the row data. */
                     last_com = astStore( last_com, line, strlen( line ) + 1 );

                  }
               }

/* If the line does not start with a comment character... */
            } else {

/* Get the words from the row. */
               words = astChrSplit( line, &nword );

/* If this is the first non-comment line, get the column names from the
   previous comment line. */
               if( first ) {
                  first = 0;
                  cols = astChrSplit( last_com, &ncol );

/* Find the data types of the columns by looking at the words from the
   current line. */
                  for( iword = 0; iword < nword; nword++ ) {
                     if( iword < ncol ) {

                        len = strlen( words[ 1 ] );
                        if( nc = 0, ( 1 == astSscanf( words[ 1 ], "%d%n", &ival, &nc ) )
                            && ( nc >= len ) ) {
                           type = AST__INTTYPE;
                        } else if( nc = 0, ( 1 == astSscanf( words[ 1 ], "%lg%n", &dval, &nc ) )
                                   && ( nc >= len ) ) {
                           type = AST__DOUBLETYPE;
                        } else {
                           type = AST__STRINGTYPE;
                        }

/* Create the column definition within the returned Table. */
                        astAddColumn( result, cols[ iword ], type, 0, NULL, " " );
                     }
                  }
               }

/* Report an error if the line has the wrong number of values. */
               if( nword != ncol ) {
                  *status = SAI__ERROR;
                  msgSeti( "N", nword );
                  msgSeti( "I", iline );
                  msgSeti( "M", ncol );
                  msgSetc( "F", fname );
                  errRep( " ", " Wrong number of values (^N) at line ^I in "
                          "file ^F (should be ^M).", status );

/* Store each value in the table. */
               } else {
                  irow++;

                  for( iword = 0; iword < nword; iword++ ) {
                     sprintf( key, "%s(%d)", cols[ iword ], irow );
                     astMapPut0C( result, key, words[ iword ], NULL );
                  }
              }

/* Free the words returned by astChrSplit. */
               for( iword = 0; iword < nword; iword++ ) {
                  words[ iword ] = astFree( words[ iword ] );
               }
               words = astFree( words );

            }
         }
      }

/* Free resources. */
      line = astFree( line );
      last_com = astFree( last_com );
      if( cols ) {
         for( icol = 0; icol < ncol; icol++ ) {
            cols[ icol ] = astFree( cols[ icol ] );
         }
         cols = astFree( cols );
      }

/* Close the output file. */
      fclose( fd );
   }

/* Make AST use its original status variable. */
   astWatch( old_status );

/* Return the Table pointer. */
   return result;
}

