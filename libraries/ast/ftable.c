/*
*+
*  Name:
*     ftable.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Table class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Table class.

*  Routines Defined:
*     AST_ISATABLE
*     AST_TABLE

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Authors:
*     DSB: David S.Berry (Starlink)

*  History:
*     22-NOV-2010 (DSB):
*        Original version.
*/

/* Define the astFORTRAN77 macro which prevents error messages from
   AST C functions from reporting the file and line number where the
   error occurred (since these would refer to this file, they would
   not be useful). */
#define astFORTRAN77

/* Header files. */
/* ============= */
#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "c2f77.h"               /* F77 <-> C support functions/macros */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory handling facilities */
#include "table.h"              /* C interface to the Table class */

F77_LOGICAL_FUNCTION(ast_isatable)( INTEGER(THIS),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astWatchSTATUS(
   astAt( "AST_ISATABLE", NULL, 0 );
      RESULT = astIsATable( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_table)( CHARACTER(OPTIONS),
                                 INTEGER(STATUS)
                                 TRAIL(OPTIONS) ) {
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   int i;
   char *options;

   astAt( "AST_TABLE", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Truncate the options string to exlucde any trailing spaces. */
      astChrTrunc( options );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astTable( "%s", options ) );
      astFree( options );
   )
   return RESULT;
}

F77_SUBROUTINE(ast_addcolumn)( INTEGER(THIS),
                               CHARACTER(NAME),
                               INTEGER(TYPE),
                               INTEGER(NDIM),
                               INTEGER_ARRAY(DIMS),
                               CHARACTER(UNIT),
                               INTEGER(STATUS)
                               TRAIL(NAME)
                               TRAIL(UNIT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(NAME)
   GENPTR_INTEGER(TYPE)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(DIMS)
   GENPTR_CHARACTER(UNIT)
   char *name, *unit;

   astAt( "AST_ADDCOLUMN", NULL, 0 );
   astWatchSTATUS(
      name = astString( NAME, NAME_length );
      unit = astString( UNIT, UNIT_length );
      astAddColumn( astI2P( *THIS ), name, *TYPE, *NDIM, DIMS, unit );
      astFree( name );
      astFree( unit );
   )
}

F77_SUBROUTINE(ast_addparameter)( INTEGER(THIS),
                                  CHARACTER(NAME),
                                  INTEGER(STATUS)
                                  TRAIL(NAME) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(NAME)
   char *name;

   astAt( "AST_ADDPARAMETER", NULL, 0 );
   astWatchSTATUS(
      name = astString( NAME, NAME_length );
      astAddParameter( astI2P( *THIS ), name );
      astFree( name );
   )
}

F77_SUBROUTINE(ast_removecolumn)( INTEGER(THIS),
                                  CHARACTER(NAME),
                                  INTEGER(STATUS)
                                  TRAIL(NAME) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(NAME)
   char *name;

   astAt( "AST_REMOVECOLUMN", NULL, 0 );
   astWatchSTATUS(
      name = astString( NAME, NAME_length );
      astRemoveColumn( astI2P( *THIS ), name );
      astFree( name );
   )
}

F77_SUBROUTINE(ast_removeparameter)( INTEGER(THIS),
                                     CHARACTER(NAME),
                                     INTEGER(STATUS)
                                     TRAIL(NAME) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(NAME)
   char *name;

   astAt( "AST_REMOVEPARAMETER", NULL, 0 );
   astWatchSTATUS(
      name = astString( NAME, NAME_length );
      astRemoveParameter( astI2P( *THIS ), name );
      astFree( name );
   )
}

F77_SUBROUTINE(ast_removerow)( INTEGER(THIS),
                               INTEGER(INDEX),
                               INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(INDEX)

   astAt( "AST_REMOVEROW", NULL, 0 );
   astWatchSTATUS(
      astRemoveRow( astI2P( *THIS ), *INDEX );
   )
}

F77_SUBROUTINE(ast_purgerows)( INTEGER(THIS),
                               INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)

   astAt( "AST_PURGEROWS", NULL, 0 );
   astWatchSTATUS(
      astPurgeRows( astI2P( *THIS ) );
   )
}


/* NO_CHAR_FUNCTION indicates that the f77.h method of returning a
   character result doesn't work, so add an extra argument instead and
   wrap this function up in a normal FORTRAN 77 function (in the file
   keymap.f). */
#if NO_CHAR_FUNCTION
F77_SUBROUTINE(ast_columnname_a)( CHARACTER(RESULT),
#else
F77_SUBROUTINE(ast_columnname)( CHARACTER_RETURN_VALUE(RESULT),
#endif
                          INTEGER(THIS),
                          INTEGER(INDEX),
#if NO_CHAR_FUNCTION
                          INTEGER(STATUS)
                          TRAIL(RESULT) ) {
#else
                          INTEGER(STATUS) ) {
#endif
   GENPTR_CHARACTER(RESULT)
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(INDEX)
   const char *result;
   int i;

   astAt( "AST_COLUMNNAME", NULL, 0 );
   astWatchSTATUS(
      result = astColumnName( astI2P( *THIS ), *INDEX );
      i = 0;
      if ( astOK ) {             /* Copy result */
         for ( ; result[ i ] && i < RESULT_length; i++ ) {
            RESULT[ i ] = result[ i ];
         }
      }
      while ( i < RESULT_length ) RESULT[ i++ ] = ' '; /* Pad with blanks */
   )
}

/* NO_CHAR_FUNCTION indicates that the f77.h method of returning a
   character result doesn't work, so add an extra argument instead and
   wrap this function up in a normal FORTRAN 77 function (in the file
   keymap.f). */
#if NO_CHAR_FUNCTION
F77_SUBROUTINE(ast_parametername_a)( CHARACTER(RESULT),
#else
F77_SUBROUTINE(ast_parametername)( CHARACTER_RETURN_VALUE(RESULT),
#endif
                          INTEGER(THIS),
                          INTEGER(INDEX),
#if NO_CHAR_FUNCTION
                          INTEGER(STATUS)
                          TRAIL(RESULT) ) {
#else
                          INTEGER(STATUS) ) {
#endif
   GENPTR_CHARACTER(RESULT)
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(INDEX)
   const char *result;
   int i;

   astAt( "AST_PARAMETERNAME", NULL, 0 );
   astWatchSTATUS(
      result = astParameterName( astI2P( *THIS ), *INDEX );
      i = 0;
      if ( astOK ) {             /* Copy result */
         for ( ; result[ i ] && i < RESULT_length; i++ ) {
            RESULT[ i ] = result[ i ];
         }
      }
      while ( i < RESULT_length ) RESULT[ i++ ] = ' '; /* Pad with blanks */
   )
}


F77_SUBROUTINE(ast_columnshape)( INTEGER(THIS),
                                 CHARACTER(COLUMN),
                                 INTEGER(MXDIM),
                                 INTEGER(NDIM),
                                 INTEGER_ARRAY(DIMS),
                                 INTEGER(STATUS)
                                 TRAIL(COLUMN) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(COLUMN)
   GENPTR_INTEGER(MXDIM)
   GENPTR_INTEGER(NDIM)
   GENPTR_INTEGER_ARRAY(DIMS)
   char *column;

   astAt( "AST_COLUMNSHAPE", NULL, 0 );
   astWatchSTATUS(
      column = astString( COLUMN, COLUMN_length );
      astColumnShape( astI2P( *THIS ), column, *MXDIM, NDIM, DIMS );
      astFree( column );
   )
}


F77_LOGICAL_FUNCTION(ast_hascolumn)( INTEGER(THIS),
                                     CHARACTER(COLUMN),
                                     INTEGER(STATUS)
                                     TRAIL(COLUMN) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(COLUMN)
   F77_LOGICAL_TYPE(RESULT);
   char *column;

   astWatchSTATUS(
   astAt( "AST_HASCOLUMN", NULL, 0 );
      column = astString( COLUMN, COLUMN_length );
      RESULT = astHasColumn( astI2P( *THIS ), column ) ? F77_TRUE : F77_FALSE;
      astFree( column );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_hasparameter)( INTEGER(THIS),
                                        CHARACTER(PARAM),
                                        INTEGER(STATUS)
                                        TRAIL(PARAM) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(PARAM)
   F77_LOGICAL_TYPE(RESULT);
   char *param;

   astWatchSTATUS(
   astAt( "AST_HASPARAMETER", NULL, 0 );
      param = astString( PARAM, PARAM_length );
      RESULT = astHasParameter( astI2P( *THIS ), param ) ? F77_TRUE : F77_FALSE;
      astFree( param );
   )
   return RESULT;
}

