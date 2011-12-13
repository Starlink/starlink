#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include <stdio.h>

AstLutMap *atlTablelutMap( AstTable *table, const char *column, int *status ) {
/*
*+
*  Name:
*     atlTableLutMap

*  Purpose:
*     Create a LutMap from a column of an AST Table.

*  Language:
*     C.

*  Invocation:
*     AstLutMap *atlTablelutMap( AstTable *table, const char *column,
*                                int *status )

*  Description:
*     This function creates a LutMap containing the values stored in a
*     specified column of a Table.

*  Arguments:
*     table
*        The Table.
*     column
*        The name of the column to use. The column must hold scalar
*        numerical values.
*     status
*        Pointer to the global status variable.

*  Returned Value:
*     A pointer to the LutMap. The input value corresponds to (one-based)
*     row number in the table, and the output value corresponds to the value
*     of the requested column.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     17-MAY-2011 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstLutMap *result;
   char attr[ 50 ];
   char key[ 50 ];
   double *lut;
   int *old_status;
   int irow;
   int nrow;
   int type;

/* Initialise */
   result = NULL;

/* Check the inherited status. */
   if( *status != SAI__OK ) return result;

/* Make AST use the Fortran status variable. */
   old_status = astWatch( status );

/* Check the column exists. */
   if( !astHasColumn( table, column ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "C", column );
      errRep( " ", "atlTableLutMap: Supplied Table does not contain a "
              "column called '^C'.", status );
   }

/* Check the column holds numerical values */
   sprintf( attr, "ColumnType(%s)", column );
   type = astGetI( table, attr );
   if( type != AST__INTTYPE &&
       type != AST__SINTTYPE &&
       type != AST__BYTETYPE &&
       type != AST__DOUBLETYPE &&
       type != AST__FLOATTYPE && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "C", column );
      errRep( " ", "atlTableLutMap: Column '^C' holds non-numerical values.",
              status );
   }

/* Check the column holds scalar values */
   sprintf( attr, "ColumnLength(%s)", column );
   if( astGetI( table, attr ) != 1 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      msgSetc( "C", column );
      errRep( " ", "atlTableLutMap: Column '^C' holds non-scalar values.",
              status );
   }

/* Get the number of rows in the table, and allocate a double array of this
   length. */
   nrow = astGetI( table, "Nrow" );
   lut = astMalloc( nrow*sizeof( double ) );
   if( *status == SAI__OK ) {

/* Read all the column values into this array. Store AST__BAD for
   missing values. */
      for( irow = 0; irow < nrow; irow++ ) {
         sprintf( key, "%s(%d)", column, irow + 1 );
         if( !astMapGet0D( table, key, lut + irow ) ) {
            lut[ irow ] = AST__BAD;
         }
      }

/* Create the LutMap. */
      result = astLutMap( nrow, lut, 1.0, 1.0, " " );
   }

/* Free resources */
   lut = astFree( lut );

/* Make AST use its original status variable. */
   astWatch( old_status );

/* Return the LutMap pointer. */
   return result;
}

