/*
*+
*  Name:
*     fkeymap.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST KeyMap class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the KeyMap class.

*  Routines Defined:
*     AST_ISAKEYMAP
*     AST_KEYMAP
*     AST_MAPCOPY
*     AST_MAPPUT0<X>
*     AST_MAPPUT1<X>
*     AST_MAPPUTU
*     AST_MAPGET0<X>
*     AST_MAPGET1<X>
*     AST_MAPGETELEM<X>
*     AST_MAPPUTELEM<X>
*     AST_MAPREMOVE
*     AST_MAPRENAME
*     AST_MAPSIZE
*     AST_MAPLENGTH
*     AST_MAPLENC
*     AST_MAPTYPE
*     AST_MAPKEY

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     13-NOV-2004 (DSB):
*        Original version.
*     5-JUN-2006 (DSB):
*        Added support for single precision entries.
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
#include "keymap.h"                 /* C interface to the KeyMap class */

F77_LOGICAL_FUNCTION(ast_isakeymap)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAKEYMAP", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAKeyMap( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_keymap)(    CHARACTER(OPTIONS),
                                  INTEGER(STATUS)
                                  TRAIL(OPTIONS) ) {
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_KEYMAP", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astKeyMap( "%s", options ) );
      astFree( options );
   )
   return RESULT;
}

F77_SUBROUTINE(ast_mapput0a)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT0A", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0A( astI2P( *THIS ), key, astI2P( *VALUE ), comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput0c)( INTEGER(THIS),
                              CHARACTER(KEY),
                              CHARACTER(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(VALUE)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_CHARACTER(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *value, *key;

   astAt( "AST_MAPPUT0C", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      value = astString( VALUE, VALUE_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0C( astI2P( *THIS ), key, value, comment );
      astFree( key );
      astFree( value );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput0i)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT0I", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0I( astI2P( *THIS ), key, *VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput0s)( INTEGER(THIS),
                              CHARACTER(KEY),
                              WORD(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_WORD(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT0W", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0S( astI2P( *THIS ), key, *VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput0b)( INTEGER(THIS),
                              CHARACTER(KEY),
                              UBYTE(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_UBYTE(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT0B", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0B( astI2P( *THIS ), key, *VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput0d)( INTEGER(THIS),
                              CHARACTER(KEY),
                              DOUBLE(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_DOUBLE(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT0D", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0D( astI2P( *THIS ), key, *VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput0r)( INTEGER(THIS),
                              CHARACTER(KEY),
                              REAL(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_REAL(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT0R", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut0F( astI2P( *THIS ), key, *VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput1a)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              INTEGER_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_INTEGER_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;
   AstObject **values;
   int i;

   astAt( "AST_MAPPUT1A", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );

      values = astMalloc( sizeof( AstObject * )*(size_t)( *SIZE ));
      if( astOK ) {
         for( i = 0; i < *SIZE; i++ ) {
            values[ i ] = astI2P( VALUE[ i ] );
         }
      }

      astMapPut1A( astI2P( *THIS ), key, *SIZE, values, comment );
      astFree( values );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput1c)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              CHARACTER_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(VALUE)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_CHARACTER_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;
   const char **values;
   int i;

   astAt( "AST_MAPPUT1C", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );

      values = astMalloc( sizeof( const char * )*(size_t)( *SIZE ));
      if( astOK ) {
         for( i = 0; i < *SIZE; i++ ) {
            values[ i ] = astString( VALUE + i*VALUE_length, VALUE_length );
         }
      }

      astMapPut1C( astI2P( *THIS ), key, *SIZE, values, comment );

      if( astOK ) {
         for( i = 0; i < *SIZE; i++ ) astFree( (void *) values[ i ] );
      }
      astFree( (void *) values );
      astFree( key );
      astFree( comment );
   )
}



F77_SUBROUTINE(ast_mapput1i)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              INTEGER_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_INTEGER_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT1I", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut1I( astI2P( *THIS ), key, *SIZE, VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}


F77_SUBROUTINE(ast_mapput1s)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              WORD_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_WORD_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT1W", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut1S( astI2P( *THIS ), key, *SIZE, VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput1b)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              UBYTE_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_UBYTE_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT1B", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut1B( astI2P( *THIS ), key, *SIZE, VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}


F77_SUBROUTINE(ast_mapput1d)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              DOUBLE_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_DOUBLE_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT1D", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut1D( astI2P( *THIS ), key, *SIZE, VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapput1r)( INTEGER(THIS),
                              CHARACTER(KEY),
                              INTEGER(SIZE),
                              REAL_ARRAY(VALUE),
                              CHARACTER(COMMENT),
                              INTEGER(STATUS)
                              TRAIL(KEY)
                              TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(SIZE)
   GENPTR_REAL_ARRAY(VALUE)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUT1R", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPut1F( astI2P( *THIS ), key, *SIZE, VALUE, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_SUBROUTINE(ast_mapputu)( INTEGER(THIS),
                             CHARACTER(KEY),
                             CHARACTER(COMMENT),
                             INTEGER(STATUS)
                             TRAIL(KEY)
                             TRAIL(COMMENT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_CHARACTER(COMMENT)
   char *comment, *key;

   astAt( "AST_MAPPUTU", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      comment = astString( COMMENT, COMMENT_length );
      astMapPutU( astI2P( *THIS ), key, comment );
      astFree( key );
      astFree( comment );
   )
}

F77_LOGICAL_FUNCTION(ast_mapget0i)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET0I", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet0I( astI2P( *THIS ), key, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget0s)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    WORD(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_WORD(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET0W", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet0S( astI2P( *THIS ), key, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget0b)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    UBYTE(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_UBYTE(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET0W", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet0B( astI2P( *THIS ), key, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget0d)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    DOUBLE(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_DOUBLE(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET0D", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet0D( astI2P( *THIS ), key, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget0r)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    REAL(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_REAL(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET0R", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet0F( astI2P( *THIS ), key, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget0c)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    CHARACTER(VALUE),
                                    INTEGER(L),
                                    INTEGER(STATUS)
                                    TRAIL(KEY)
                                    TRAIL(VALUE) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_CHARACTER(VALUE)
   GENPTR_INTEGER(L)
   F77_LOGICAL_TYPE(RESULT);
   char *key;
   const char *value;
   int i;

   astAt( "AST_MAPGET0C", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      value = NULL;
      RESULT = astMapGet0C( astI2P( *THIS ), key, &value ) ? F77_TRUE : F77_FALSE;
      astFree( key );
      i = 0;
      if( value ) {
         for( ; value[ i ] && ( i < VALUE_length ); i++ ) {
            VALUE[ i ] = value[ i ];
         }
         *L = i;
      } else {
         *L = 0;
      }

      if( VALUE ) {
         for( ; i < VALUE_length; i++ ) {
            VALUE[ i ] = ' ';
         }
      }
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget0a)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;
   AstObject *value;

   astAt( "AST_MAPGET0A", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet0A( astI2P( *THIS ), key, &value ) ? F77_TRUE : F77_FALSE;
      astFree( key );
      *VALUE = astP2I( value );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget1i)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    INTEGER_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_INTEGER_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET1I", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1I( astI2P( *THIS ), key, *MXVAL, NVAL, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}


F77_LOGICAL_FUNCTION(ast_mapget1d)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    DOUBLE_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_DOUBLE_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET1D", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1D( astI2P( *THIS ), key, *MXVAL, NVAL, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget1s)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    WORD_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_WORD_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET1W", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1S( astI2P( *THIS ), key, *MXVAL, NVAL, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget1b)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    UBYTE_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_UBYTE_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET1B", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1B( astI2P( *THIS ), key, *MXVAL, NVAL, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapget1r)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    REAL_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_REAL_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGET1R", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1F( astI2P( *THIS ), key, *MXVAL, NVAL, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}


F77_LOGICAL_FUNCTION(ast_mapget1a)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    INTEGER_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_INTEGER_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;
   AstObject **values;
   int i;

   astAt( "AST_MAPGET1A", NULL, 0 );
   astWatchSTATUS(
      values = astMalloc( sizeof( AstObject *)*(size_t) *MXVAL );
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1A( astI2P( *THIS ), key, *MXVAL, NVAL, values ) ? F77_TRUE : F77_FALSE;
      astFree( key );
      if( astOK ) {
         for( i = 0; i < *NVAL; i++ ) VALUE[ i ] = astP2I( values[ i ] );
      }
      astFree( values );

   )
   return RESULT;
}


F77_LOGICAL_FUNCTION(ast_mapget1c)( INTEGER(THIS),
                                    CHARACTER(KEY),
                                    INTEGER(MXVAL),
                                    INTEGER(NVAL),
                                    CHARACTER_ARRAY(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(KEY)
                                    TRAIL(VALUE) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(MXVAL)
   GENPTR_INTEGER(NVAL)
   GENPTR_CHARACTER_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;
   char *values, *c, *d;
   int i, j, term;

   astAt( "AST_MAPGET1C", NULL, 0 );
   astWatchSTATUS(
      values = astMalloc( sizeof( char )*(size_t) (*MXVAL)*( VALUE_length + 1 ) );
      key = astString( KEY, KEY_length );
      RESULT = astMapGet1C( astI2P( *THIS ), key, VALUE_length + 1, *MXVAL,
                            NVAL, values ) ? F77_TRUE : F77_FALSE;
      astFree( key );

/* Loop round each string value returned in the array */
      if( astOK ) {
         c = values;
         d = VALUE;
         for( i = 0; i < *NVAL; i++ ) {

/* Loop round each of character in the "i"th element of the returned
   array. Copy characters from the work array until a terminating null is
   found. Replace this null by a space and replace all subsequent
   characters by spaces up to the end of the returned array element. */
            term = 0;
            for( j = 0; j < VALUE_length; j++, d++, c++ ) {
               if( term ) {
                  *d = ' ';
               } else if( (*d = *c) == 0 ) {
                  *d = ' ';
                  term = 1;
               }
            }

/* Skip over the extra character at the end of each element in the work
   array. */
            c++;
         }
      }
      astFree( values );
   )
   return RESULT;
}


F77_SUBROUTINE(ast_mapremove)( INTEGER(THIS),
                               CHARACTER(KEY),
                               INTEGER(STATUS)
                               TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   char *key;

   astAt( "AST_MAPREMOVE", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapRemove( astI2P( *THIS ), key );
      astFree( key );
   )
}

F77_SUBROUTINE(ast_maprename)( INTEGER(THIS),
                               CHARACTER(OLDKEY),
                               CHARACTER(NEWKEY),
                               INTEGER(STATUS)
                               TRAIL(OLDKEY)
                               TRAIL(NEWKEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(OLDKEY)
   GENPTR_CHARACTER(NEWKEY)
   char *oldkey, *newkey;

   astAt( "AST_MAPRENAME", NULL, 0 );
   astWatchSTATUS(
      oldkey = astString( OLDKEY, OLDKEY_length );
      newkey = astString( NEWKEY, NEWKEY_length );
      astMapRename( astI2P( *THIS ), oldkey, newkey );
      astFree( oldkey );
      astFree( newkey );
   )
}

F77_SUBROUTINE(ast_mapcopy)( INTEGER(THIS),
                             INTEGER(THAT),
                             INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(THAT)

   astAt( "AST_MAPCOPY", NULL, 0 );
   astWatchSTATUS(
      astMapCopy( astI2P( *THIS ), astI2P( *THAT ) );
   )
}

F77_INTEGER_FUNCTION(ast_mapsize)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_MAPSIZE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astMapSize( astI2P( *THIS ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_maplength)( INTEGER(THIS),
                                     CHARACTER(KEY),
                                     INTEGER(STATUS)
                                     TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   F77_INTEGER_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPLENGTH", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapLength( astI2P( *THIS ), key );
      astFree( key );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_maplenc)( INTEGER(THIS),
                                   CHARACTER(KEY),
                                   INTEGER(STATUS)
                                   TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   F77_INTEGER_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPLENGTH", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapLenC( astI2P( *THIS ), key );
      astFree( key );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_maptype)( INTEGER(THIS),
                                   CHARACTER(KEY),
                                   INTEGER(STATUS)
                                   TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   F77_INTEGER_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPTYPE", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapType( astI2P( *THIS ), key );
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_maphaskey)( INTEGER(THIS),
                                     CHARACTER(KEY),
                                     INTEGER(STATUS)
                                     TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPHASKEY", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapHasKey( astI2P( *THIS ), key ) ? F77_TRUE : F77_FALSE;

      astFree( key );
   )
   return RESULT;
}

/* NO_CHAR_FUNCTION indicates that the f77.h method of returning a
   character result doesn't work, so add an extra argument instead and
   wrap this function up in a normal FORTRAN 77 function (in the file
   keymap.f). */
#if NO_CHAR_FUNCTION
F77_SUBROUTINE(ast_mapkey_a)( CHARACTER(RESULT),
#else
F77_SUBROUTINE(ast_mapkey)( CHARACTER_RETURN_VALUE(RESULT),
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

   astAt( "AST_MAPKEY", NULL, 0 );
   astWatchSTATUS(
      result = astMapKey( astI2P( *THIS ), *INDEX - 1 );
      i = 0;
      if ( astOK ) {             /* Copy result */
         for ( ; result[ i ] && i < RESULT_length; i++ ) {
            RESULT[ i ] = result[ i ];
         }
      }
      while ( i < RESULT_length ) RESULT[ i++ ] = ' '; /* Pad with blanks */
   )
}


F77_LOGICAL_FUNCTION(ast_mapgetelemi)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       INTEGER_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_INTEGER_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGETELEMI", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemI( astI2P( *THIS ), key, *ELEM - 1, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}


F77_LOGICAL_FUNCTION(ast_mapgetelemd)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       DOUBLE_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_DOUBLE_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGETELEMD", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemD( astI2P( *THIS ), key, *ELEM - 1, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapgetelems)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       WORD_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_WORD_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGETELEMW", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemS( astI2P( *THIS ), key, *ELEM - 1, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapgetelemb)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       UBYTE_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_UBYTE_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGETELEMB", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemB( astI2P( *THIS ), key, *ELEM - 1, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_mapgetelemr)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       REAL_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_REAL_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;

   astAt( "AST_MAPGETELEMR", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemF( astI2P( *THIS ), key, *ELEM - 1, VALUE ) ? F77_TRUE : F77_FALSE;
      astFree( key );
   )
   return RESULT;
}


F77_LOGICAL_FUNCTION(ast_mapgetelema)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       INTEGER_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_INTEGER_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;
   AstObject *ptr;

   astAt( "AST_MAPGETELEMA", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemA( astI2P( *THIS ), key, *ELEM - 1, &ptr ) ? F77_TRUE : F77_FALSE;
      astFree( key );
      if( astOK ) *VALUE = astP2I( ptr );
   )
   return RESULT;
}


F77_LOGICAL_FUNCTION(ast_mapgetelemc)( INTEGER(THIS),
                                       CHARACTER(KEY),
                                       INTEGER(ELEM),
                                       CHARACTER_ARRAY(VALUE),
                                       INTEGER(STATUS)
                                       TRAIL(KEY)
                                       TRAIL(VALUE) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_CHARACTER_ARRAY(VALUE)
   F77_LOGICAL_TYPE(RESULT);
   char *key;
   char *values, *c, *d;
   int j;

   astAt( "AST_MAPGETELEMC", NULL, 0 );
   astWatchSTATUS(
      values = astMalloc( sizeof( char )*(size_t) ( VALUE_length + 1 ) );
      key = astString( KEY, KEY_length );
      RESULT = astMapGetElemC( astI2P( *THIS ), key, VALUE_length + 1, *ELEM - 1,
                               values ) ? F77_TRUE : F77_FALSE;
      astFree( key );

/* Copy characters from the work array until a terminating null is
   found. Replace this null by a space and replace all subsequent
   characters by spaces up to the end of the returned array element. */
      if( astOK ) {
         c = values;
         d = VALUE;

         for( j = 0; j < VALUE_length; j++, d++, c++ ) {
            if( (*d = *c) == 0 ) {
               *d = ' ';
               break;
            }
         }

         for( ; j < VALUE_length; j++, d++ ) *d = ' ';

      }
      astFree( values );
   )
   return RESULT;
}


F77_SUBROUTINE(ast_mapputelemi)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 INTEGER(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_INTEGER(VALUE)
   char *key;

   astAt( "AST_MAPPUTELEMI", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapPutElemI( astI2P( *THIS ), key, *ELEM - 1, *VALUE );
      astFree( key );
   )
}


F77_SUBROUTINE(ast_mapputelems)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 WORD(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_WORD(VALUE)
   char *key;

   astAt( "AST_MAPPUTELEMW", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapPutElemS( astI2P( *THIS ), key, *ELEM - 1, *VALUE );
      astFree( key );
   )
}

F77_SUBROUTINE(ast_mapputelemb)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 UBYTE(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_UBYTE(VALUE)
   char *key;

   astAt( "AST_MAPPUTELEMB", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapPutElemB( astI2P( *THIS ), key, *ELEM - 1, *VALUE );
      astFree( key );
   )
}

F77_SUBROUTINE(ast_mapputelemd)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 DOUBLE(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_DOUBLE(VALUE)
   char *key;

   astAt( "AST_MAPPUTELEMD", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapPutElemD( astI2P( *THIS ), key, *ELEM - 1, *VALUE );
      astFree( key );
   )
}

F77_SUBROUTINE(ast_mapputelemr)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 REAL(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_REAL(VALUE)
   char *key;

   astAt( "AST_MAPPUTELEMR", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapPutElemF( astI2P( *THIS ), key, *ELEM - 1, *VALUE );
      astFree( key );
   )
}


F77_SUBROUTINE(ast_mapputelema)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 INTEGER(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_INTEGER(VALUE)
   char *key;

   astAt( "AST_MAPPUTELEMA", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      astMapPutElemA( astI2P( *THIS ), key, *ELEM - 1, astI2P( *VALUE ) );
      astFree( key );
   )
}


F77_SUBROUTINE(ast_mapputelemc)( INTEGER(THIS),
                                 CHARACTER(KEY),
                                 INTEGER(ELEM),
                                 CHARACTER(VALUE),
                                 INTEGER(STATUS)
                                 TRAIL(KEY)
                                 TRAIL(VALUE) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(KEY)
   GENPTR_INTEGER(ELEM)
   GENPTR_CHARACTER(VALUE)
   char *key;
   char *value;

   astAt( "AST_MAPPUTELEMC", NULL, 0 );
   astWatchSTATUS(
      key = astString( KEY, KEY_length );
      value = astString( VALUE, VALUE_length );
      astMapPutElemC( astI2P( *THIS ), key, *ELEM - 1, value );
      astFree( key );
      astFree( value );
   )
}

