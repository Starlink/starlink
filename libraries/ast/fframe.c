/*
*+
*  Name:
*     fframe.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Frame class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Frame class.

*  Routines Defined:
*     AST_CONVERT
*     AST_DISTANCE
*     AST_FORMAT
*     AST_FRAME
*     AST_ISAFRAME
*     AST_NORM
*     AST_OFFSET
*     AST_PERMAXES
*     AST_PICKAXES
*     AST_UNFORMAT

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     23-JUL-1996 (RFWS):
*        Original version.
*     16-SEP-1996 (RFWS):
*        Added AST_DISTANCE and AST_OFFSET.
*     25-FEB-1998 (RFWS):
*        Added AST_UNFORMAT.
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
#include "mapping.h"             /* C interface to the Mapping class */
#include "frame.h"               /* C interface to the Frame class */


F77_INTEGER_FUNCTION(ast_convert)( INTEGER(FROM),
                                   INTEGER(TO),
                                   CHARACTER(NAMELIST),
                                   INTEGER(STATUS)
                                   TRAIL(NAMELIST) ) {
   GENPTR_INTEGER(FROM)
   GENPTR_INTEGER(TO)
   GENPTR_INTEGER(NAMELIST)
   F77_INTEGER_TYPE(RESULT);
   char *namelist;

   astAt( "AST_CONVERT", NULL, 0 );
   astWatchSTATUS(
      namelist = astString( NAMELIST, NAMELIST_length );
      RESULT = astP2I( astConvert( astI2P( *FROM ), astI2P( *TO ),
                                   namelist ) );
   )
   return RESULT;
}

F77_DOUBLE_FUNCTION(ast_distance)( INTEGER(THIS),
                                   DOUBLE_ARRAY(POINT1),
                                   DOUBLE_ARRAY(POINT2),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(POINT1)
   GENPTR_DOUBLE_ARRAY(POINT2)
   F77_DOUBLE_TYPE(RESULT);

   astAt( "AST_DISTANCE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astDistance( astI2P( *THIS ), POINT1, POINT2 );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_findframe)( INTEGER(TARGET),
                                     INTEGER(TEMPLATE),
                                     CHARACTER(NAMELIST),
                                     INTEGER(STATUS)
                                     TRAIL(NAMELIST) ) {
   GENPTR_INTEGER(TARGET)
   GENPTR_INTEGER(TEMPLATE)
   GENPTR_INTEGER(NAMELIST)
   F77_INTEGER_TYPE(RESULT);
   char *namelist;

   astAt( "AST_FINDFRAME", NULL, 0 );
   astWatchSTATUS(
      namelist = astString( NAMELIST, NAMELIST_length );
      RESULT = astP2I( astFindFrame( astI2P( *TARGET ), astI2P( *TEMPLATE ),
                                     namelist ) );
   )
   return RESULT;
}

/* NO_CHAR_FUNCTION indicates that the f77.h method of returning a
   character result doesn't work, so add an extra argument instead and
   wrap this function up in a normal FORTRAN 77 function (in the file
   frame.f). */
#if NO_CHAR_FUNCTION
F77_SUBROUTINE(ast_format_a)( CHARACTER(RESULT),
#else
F77_SUBROUTINE(ast_format)( CHARACTER_RETURN_VALUE(RESULT),
#endif
                            INTEGER(THIS),
                            INTEGER(AXIS),
                            DOUBLE(VALUE),
                            INTEGER(STATUS)
#if NO_CHAR_FUNCTION
                            TRAIL(RESULT)
#endif
                              ) {
   GENPTR_CHARACTER(RESULT)
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(AXIS)
   GENPTR_DOUBLE(VALUE)
   const char *result;
   int i;

   astAt( "AST_FORMAT", NULL, 0 );
   astWatchSTATUS(
      result = astFormat( astI2P( *THIS ), *AXIS, *VALUE );
      i = 0;
      if ( astOK ) {             /* Copy result */
         for ( ; result[ i ] && i < RESULT_length; i++ ) {
            RESULT[ i ] = result[ i ];
         }
      }
      while ( i < RESULT_length ) RESULT[ i++ ] = ' '; /* Pad with blanks */
   )
}

F77_INTEGER_FUNCTION(ast_frame)( INTEGER(NAXES),
                                 CHARACTER(OPTIONS),
                                 INTEGER(STATUS)
                                 TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(NAXES)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_FRAME", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astFrame( *NAXES, "%s", options ) );
      (void) astFree( options );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_isaframe)( INTEGER(THIS),
                                    INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAFRAME", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAFrame( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_SUBROUTINE(ast_norm)( INTEGER(THIS),
                          DOUBLE_ARRAY(VALUE),
                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(VALUE)

   astAt( "AST_NORM", NULL, 0 );
   astWatchSTATUS(
      astNorm( astI2P( *THIS ), VALUE );   
   )
}

F77_SUBROUTINE(ast_offset)( INTEGER(THIS),
                            DOUBLE_ARRAY(POINT1),
                            DOUBLE_ARRAY(POINT2),
                            DOUBLE(OFFSET),
                            DOUBLE_ARRAY(POINT3),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(POINT1)
   GENPTR_DOUBLE_ARRAY(POINT2)
   GENPTR_DOUBLE(OFFSET)
   GENPTR_DOUBLE_ARRAY(POINT3)

   astAt( "AST_OFFSET", NULL, 0 );
   astWatchSTATUS(
      astOffset( astI2P( *THIS ), POINT1, POINT2, *OFFSET, POINT3 );
   )
}

F77_SUBROUTINE(ast_permaxes)( INTEGER(THIS),
                              INTEGER_ARRAY(PERM),
                              INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER_ARRAY(PERM)

   astAt( "AST_PERMAXES", NULL, 0 );
   astWatchSTATUS(
      astPermAxes( astI2P( *THIS ), PERM );
   )
}

F77_INTEGER_FUNCTION(ast_pickaxes)( INTEGER(THIS),
                                    INTEGER(NAXES),
                                    INTEGER_ARRAY(AXES),
                                    INTEGER(MAP),
                                    INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(NAXES)
   GENPTR_INTEGER_ARRAY(AXES)
   GENPTR_INTEGER(MAP)
   F77_INTEGER_TYPE(RESULT);
   AstMapping *map;

   astAt( "AST_PICKAXES", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astPickAxes( astI2P( *THIS ), *NAXES, AXES, &map ) );
      *MAP = astP2I( map );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_unformat)( INTEGER(THIS),
                                    INTEGER(AXIS),
                                    CHARACTER(STRING),
                                    DOUBLE(VALUE),
                                    INTEGER(STATUS)
                                    TRAIL(STRING) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(AXIS)
   GENPTR_CHARACTER(STRING)
   GENPTR_DOUBLE(VALUE)
   GENPTR_INTEGER(STATUS)
   F77_INTEGER_TYPE(RESULT);
   char *string;
   double value;

   astAt( "AST_UNFORMAT", NULL, 0 );
   astWatchSTATUS(
      string = astString( STRING, STRING_length );
      
      RESULT = astUnformat( astI2P( *THIS ), *AXIS, string, &value );
      *VALUE = value;
      (void) astFree( string );
   )
   return RESULT;
}
