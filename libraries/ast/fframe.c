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
*     AST_ANGLE
*     AST_AXANGLE
*     AST_AXDISTANCE
*     AST_AXOFFSET
*     AST_CONVERT
*     AST_DISTANCE
*     AST_FORMAT
*     AST_FRAME
*     AST_GETACTIVEUNIT
*     AST_INTERSECT
*     AST_ISAFRAME
*     AST_NORM
*     AST_OFFSET
*     AST_OFFSET2
*     AST_PERMAXES
*     AST_PICKAXES
*     AST_RESOLVE
*     AST_SETACTIVEUNIT
*     AST_UNFORMAT

*  Copyright:
*     Copyright (C) 1997-2009 Council for the Central Laboratory of the
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
*     RFWS: R.F. Warren-Smith (Starlink)
*     DSB: David S. Berry (Starlink)

*  History:
*     23-JUL-1996 (RFWS):
*        Original version.
*     16-SEP-1996 (RFWS):
*        Added AST_DISTANCE and AST_OFFSET.
*     25-FEB-1998 (RFWS):
*        Added AST_UNFORMAT.
*     21-JUN-2001 (DSB):
*        Added AST_ANGLE and AST_OFFSET2.
*     29-AUG-2001 (DSB):
*        Added AST_AXDISTANCE and AST_AXOFFSET.
*     9-SEP-2001 (DSB):
*        Added AST_RESOLVE and AST_BEAR.
*     21-SEP-2001 (DSB):
*        Replaced AST_BEAR by AST_AXANGLE.
*     17-DEC-2002 (DSB):
*        Added AST_GETACTIVEUNIT and AST_SETACTIVEUNIT.
*     14-JAN-2009 (DSB):
*        Added AST_INTERSECT.
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
      namelist = astFree( namelist );
   )
   return RESULT;
}

F77_DOUBLE_FUNCTION(ast_angle)( INTEGER(THIS),
                                   DOUBLE_ARRAY(A),
                                   DOUBLE_ARRAY(B),
                                   DOUBLE_ARRAY(C),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(A)
   GENPTR_DOUBLE_ARRAY(B)
   GENPTR_DOUBLE_ARRAY(C)
   F77_DOUBLE_TYPE(RESULT);

   astAt( "AST_ANGLE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astAngle( astI2P( *THIS ), A, B, C );
   )
   return RESULT;
}

F77_DOUBLE_FUNCTION(ast_axangle)( INTEGER(THIS),
                                  DOUBLE_ARRAY(A),
                                  DOUBLE_ARRAY(B),
                                  INTEGER(AXIS),
                                  INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(A)
   GENPTR_DOUBLE_ARRAY(B)
   GENPTR_INTEGER(AXIS)
   F77_DOUBLE_TYPE(RESULT);

   astAt( "AST_AXANGLE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astAxAngle( astI2P( *THIS ), A, B, *AXIS );
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

F77_DOUBLE_FUNCTION(ast_axdistance)( INTEGER(THIS),
                                     INTEGER(AXIS),
                                     DOUBLE(V1),
                                     DOUBLE(V2),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(AXIS)
   GENPTR_DOUBLE(V1)
   GENPTR_DOUBLE(V2)
   F77_DOUBLE_TYPE(RESULT);

   astAt( "AST_AXDISTANCE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astAxDistance( astI2P( *THIS ), *AXIS, *V1, *V2 );
   )
   return RESULT;
}

F77_DOUBLE_FUNCTION(ast_axoffset)( INTEGER(THIS),
                                   INTEGER(AXIS),
                                   DOUBLE(V1),
                                   DOUBLE(DIST),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(AXIS)
   GENPTR_DOUBLE(V1)
   GENPTR_DOUBLE(DIST)
   F77_DOUBLE_TYPE(RESULT);

   astAt( "AST_AXOFFSET", NULL, 0 );
   astWatchSTATUS(
      RESULT = astAxOffset( astI2P( *THIS ), *AXIS, *V1, *DIST );
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

F77_SUBROUTINE(ast_matchaxes)( INTEGER(FRM1),
                               INTEGER(FRM2),
                               INTEGER_ARRAY(AXES),
                               INTEGER(STATUS) ) {
   GENPTR_INTEGER(FRM1)
   GENPTR_INTEGER(FRM2)
   GENPTR_INTEGER_ARRAY(AXES)

   astAt( "AST_MATCHAXES", NULL, 0 );
   astWatchSTATUS(
      astMatchAxes( astI2P( *FRM1 ), astI2P( *FRM2 ), AXES );
   )
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

F77_LOGICAL_FUNCTION(ast_getactiveunit)( INTEGER(THIS),
                                         INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_GETACTIVEUNIT", NULL, 0 );
   astWatchSTATUS(
      RESULT = astGetActiveUnit( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_SUBROUTINE(ast_setactiveunit)( INTEGER(THIS),
                                   LOGICAL(VALUE),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_LOGICAL(VALUE)

   astAt( "AST_SETACTIVEUNIT", NULL, 0 );
   astWatchSTATUS(
      astSetActiveUnit( astI2P( *THIS ), F77_ISTRUE( *VALUE ) ? 1 : 0 );
   )
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

F77_DOUBLE_FUNCTION(ast_offset2)( INTEGER(THIS),
                             DOUBLE_ARRAY(POINT1),
                             DOUBLE(ANGLE),
                             DOUBLE(OFFSET),
                             DOUBLE_ARRAY(POINT2),
                             INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(POINT1)
   GENPTR_DOUBLE(ANGLE)
   GENPTR_DOUBLE(OFFSET)
   GENPTR_DOUBLE_ARRAY(POINT2)
   F77_DOUBLE_TYPE(RESULT);

   astAt( "AST_OFFSET2", NULL, 0 );
   astWatchSTATUS(
      RESULT = astOffset2( astI2P( *THIS ), POINT1, *ANGLE, *OFFSET, POINT2 );
   )
   return RESULT;
}

F77_SUBROUTINE(ast_resolve)( INTEGER(THIS),
                             DOUBLE_ARRAY(POINT1),
                             DOUBLE_ARRAY(POINT2),
                             DOUBLE_ARRAY(POINT3),
                             DOUBLE_ARRAY(POINT4),
                             DOUBLE(D1),
                             DOUBLE(D2),
                             INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(POINT1)
   GENPTR_DOUBLE_ARRAY(POINT2)
   GENPTR_DOUBLE_ARRAY(POINT3)
   GENPTR_DOUBLE_ARRAY(POINT4)
   GENPTR_DOUBLE(D1)
   GENPTR_DOUBLE(D2)

   astAt( "AST_RESOLVE", NULL, 0 );
   astWatchSTATUS(
      astResolve( astI2P( *THIS ), POINT1, POINT2, POINT3, POINT4, D1, D2 );
   )
}

F77_SUBROUTINE(ast_intersect)( INTEGER(THIS),
                               DOUBLE_ARRAY(A1),
                               DOUBLE_ARRAY(A2),
                               DOUBLE_ARRAY(B1),
                               DOUBLE_ARRAY(B2),
                               DOUBLE_ARRAY(CROSS),
                               INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(A1)
   GENPTR_DOUBLE_ARRAY(A2)
   GENPTR_DOUBLE_ARRAY(B1)
   GENPTR_DOUBLE_ARRAY(B2)
   GENPTR_DOUBLE_ARRAY(CROSS)

   astAt( "AST_INTERSECT", NULL, 0 );
   astWatchSTATUS(
      astIntersect( astI2P( *THIS ), A1, A2, B1, B2, CROSS );
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
