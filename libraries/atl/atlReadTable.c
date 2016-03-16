#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>

static AstTable *ReadNextTable( FILE *fd, const char *fname, int *iline,
                                int *status );

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
*
*     In addition, any row that consists just of two or more minus signs,
*     with no leading spaces, is taken to mark the end of the catalogue.
*     Any subsequent lines in the file are assumed to form a whole new
*     table that is read in exactly the same way as the first. This
*     second Table is stored as a parameter of the first Table using the
*     key "SubTable".

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
*     Copyright (C) 2016 East Asian Observatory.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-MAY-2011 (DSB):
*        Original version.
*     16-MAR-2016 (DSB):
*        Added the SubTable facility.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstTable *result;
   FILE *fd;
   int iline;
   int *old_status;

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

/* Create the Table by reading text from the start of the file. */
      iline = 0;
      result = ReadNextTable( fd, fname, &iline, status );

/* Close the output file. */
      fclose( fd );
   }

/* Make AST use its original status variable. */
   astWatch( old_status );

/* Return the Table pointer. */
   return result;
}



static AstTable *ReadNextTable( FILE *fd, const char *fname, int *iline,
                                int *status ) {
/*
*  Name:
*     ReadOneTable

*  Purpose:
*     Reads a single Table from a text file.

*  Description:
*     This function reads text from the supplied file descriptor until it
*     reaches the end of file or encounters an end-of-table marker (a line
*     consisting just of two or more minus signs with no leading spaces).
*     It creates an AstTable from the text and returns a pointer to it.

*  Arguments:
*     fd
*        The file descriptor.
*     fname
*        The file name - used for error messages.
*     iline
*        Pointer to an int holding the number of lines read from the
*        file so far. Updated on exit to include the lines read by the
*        invocation of this function.
*     status
*        Pointer to the global status variable.

*  Returned Value:
*     A pointer to the Table read from the file, or NULL if an error occurs.

*/

/* Local Variables: */
   AstTable *result;
   AstTable *subtable;
   char **cols;
   char **words;
   char *last_com;
   char *line;
   char *p;
   char *tname;
   char key[ 200 ];
   const char *cval;
   const char *oldname;
   const char *newname;
   double dval;
   int *types;
   int blank;
   int c;
   int com;
   int eot;
   int first;
   int icol;
   int irow;
   int ival;
   int iword;
   int line_len;
   int max_line_len;
   int more;
   int nc;
   int ncol;
   int nrow;
   int nword;
   int skip;
   size_t len;

/* Initialise */
   result = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Create an empty Table. */
   result = astTable( " " );

/* Allocate a buffer for one one line of text. This will be increased in
   size as required. */
   max_line_len = 80;
   line = astMalloc( max_line_len*sizeof( *line ) );

/* Read each line of text from the file. */
   eot = 0;
   skip = 1;
   line_len = 0;
   more = 1;
   last_com = NULL;
   cols = NULL;
   first = 1;
   irow = 0;
   types = NULL;

   while( more && *status == SAI__OK ) {
      (*iline)++;
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

/* Assume the line is a blank non-comment, and store a pointer to the first
   character to use. */
      blank = 1;
      com = 0;
      p = line;

/* Skip blank lines. */
      if( line[ 0 ] ) {

/* If the line starts with a comment character... */
         if( line[ 0 ] == '#' || line[ 0 ] == '!' ) {
            com = 1;

/* Get a pointer to the first non-space/tab character after the comment
   character. */
            p = line + 1;
            while( *p == ' ' || *p == '\t' ) p++;

/* Note if it is blank. */
            if( *p ) blank = 0;

/* If it is not a comment line, then the line is not blank. */
         } else {
            blank = 0;

/* See if it is the end-of-table marker - a line containing just two or
   more minus signs with no leading spaces. */
            eot = ( strspn( line, "-" ) > 1 );
         }
      }

/* Skip blank lines, whether comment or not. */
      if( ! blank ) {

/* First handle comment lines. */
         if( com ) {

/* Does it look like a  parameter assignment... */
            words = astChrSplitRE( p, "^\\s*(\\w+)\\s*=\\s*(.*)$", &nword, NULL );
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

/* Save a copy of it in case it turns out to be the last non-blank comment
   line before the first row of data values (in which case it should
   contain the column names). */
               last_com = astStore( last_com, p, strlen( p ) + 1 );
            }

/* If the line is not a comment see if it is an end of table marker. If so
   indicate that we should leave the loop. */
         } else if( eot ) {
            more = 0;

/* If the line is not a comment or an end of table marker ... */
         } else {

/* Get the words from the row. */
            words = astChrSplit( p, &nword );

/* If this is the first non-blank non-comment line, get the column names from
   the previous non-blank comment line. */
            if( first ) {
               first = 0;
               cols = astChrSplit( last_com, &ncol );

/* Create an array to hold the data type for each colum, and initialise
   them to "integer". */
               types = astMalloc( ncol*sizeof( int ) ) ;
               for( iword = 0; iword < nword && astOK; iword++ ) {
                  if( iword < ncol ) {
                     types[ iword ] = AST__INTTYPE;

/* The columns are stored initially using interim names which have "T_"
   prepended to the names given in the file. */
                     tname = NULL;
                     nc = 0;
                     tname = astAppendString( tname, &nc, "T_" );
                     tname = astAppendString( tname, &nc, cols[ iword ] );
                     astFree( cols[ iword ] );
                     cols[ iword ] = tname;

/* Create the column definition within the returned Table. We store them
   initially as strings and then convert to the appropriate column data type
   later (once all rows have been read and the the data types are known). */
                     astAddColumn( result, cols[ iword ], AST__STRINGTYPE,
                                   0, NULL, " " );
                  }
               }
            }

/* Report an error if the line has the wrong number of values. */
            if( nword != ncol ) {
               *status = SAI__ERROR;
               msgSeti( "N", nword );
               msgSeti( "I", (*iline) );
               msgSeti( "M", ncol );
               msgSetc( "F", fname );
               errRep( " ", "Wrong number of values (^N) at line ^I in "
                       "file ^F (should be ^M).", status );

/* Otherwise increment the number of rows read. */
            } else {
               irow++;

/* Store each string value in the table, excluding "null" strings. Also check
   the data type of each string an dupdate the column data types if necessary. */
               for( iword = 0; iword < nword && *status == SAI__OK; iword++ ) {
                  if( strcmp( words[ iword ], "null" ) ) {
                     sprintf( key, "%s(%d)", cols[ iword ], irow );
                     astMapPut0C( result, key, words[ iword ], NULL );

/* If the column is currently thought to hold integers, check that the
   current word looks like an integer. If not, down-grade the column type
   to double. */
                     len = strlen( words[ iword ] );
                     if( types[ iword ] == AST__INTTYPE ) {
                        if( nc = 0, ( 1 != astSscanf( words[ iword ], "%d%n",
                                                      &ival, &nc ) ) ||
                                    ( nc < len ) ) {
                           types[ iword ] = AST__DOUBLETYPE;
                        }
                     }

/* If the column is currently thought to hold doubles, check that the
   current word looks like an doubler. If not, down-grade the column type
   to string. */
                     if( types[ iword ] == AST__DOUBLETYPE ) {
                        if( nc = 0, ( 1 != astSscanf( words[ iword ], "%lg%n",
                                                      &dval, &nc ) ) ||
                                    ( nc < len ) ) {
                           types[ iword ] = AST__STRINGTYPE;
                        }
                     }
                  }
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

/* The entire file has now been read, and a Table created in which every
   column holds strings. We also have flags indicating whether the values
   in each column are all integers or doubles. Modify the type of the
   column within the Table to match these flags. */
   nrow = astGetI( result, "Nrow" );
   for( icol = 0; icol < ncol && *status == SAI__OK; icol++ ) {

/* The column will be re-named from "T_<name>" to "<name>". */
      oldname = cols[ icol ];
      newname = oldname + 2;

/* First convert string columns to integer columns if all the values in
   the column look like integers. */
      if( types[ icol ] == AST__INTTYPE ) {

/* Create the new column */
         astAddColumn( result, newname, AST__INTTYPE, 0, NULL, " " );

/* Copy each cell of the current column, converting from string to integer. */
         for( irow = 1; irow <= nrow; irow++ ) {
            sprintf( key, "%s(%d)", oldname, irow );
            if( astMapGet0I( result, key, &ival ) ) {
               sprintf( key, "%s(%d)", newname, irow );
               astMapPut0I( result, key, ival, NULL );
            }
         }

/* Now do double columns in the same way. */
      } else if( types[ icol ] == AST__DOUBLETYPE ) {
         astAddColumn( result, newname, AST__DOUBLETYPE, 0, NULL, " " );
         for( irow = 1; irow <= nrow; irow++ ) {
            sprintf( key, "%s(%d)", oldname, irow );
            if( astMapGet0D( result, key, &dval ) ) {
               sprintf( key, "%s(%d)", newname, irow );
               astMapPut0D( result, key, dval, NULL );
            }
         }

/* Copy string values without change. */
      } else {
         astAddColumn( result, newname, AST__STRINGTYPE, 0, NULL, " " );
         for( irow = 1; irow <= nrow; irow++ ) {
            sprintf( key, "%s(%d)", oldname, irow );
            if( astMapGet0C( result, key, &cval ) ) {
               sprintf( key, "%s(%d)", newname, irow );
               astMapPut0C( result, key, cval, NULL );
            }
         }
      }

/* Remove the old column. */
      astRemoveColumn( result, oldname );

   }

/* Free resources. */
   line = astFree( line );
   last_com = astFree( last_com );
   types = astFree( types );
   if( cols ) {
      for( icol = 0; icol < ncol; icol++ ) {
         cols[ icol ] = astFree( cols[ icol ] );
      }
      cols = astFree( cols );
   }

/* If the table ended with an end-of-table marker, there may be another
   Table in the file. Call this function recursively to read it. */
   if( eot ) {
      subtable = ReadNextTable( fd, fname, iline, status );

/* Store the subtable as a table parameter in the returned table. */
      if( subtable ) {
         astAddParameter( result, "SubTable" );
         astMapPut0A( result, "SubTable", subtable, NULL );

/* The Table clones the pointer, so we must annull our local copy of it. */
         subtable = astAnnul( subtable );
      }
   }

/* Return the Table pointer. */
   return result;
}

