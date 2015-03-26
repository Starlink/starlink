/*
*+
*  Name:
*     fpolygon.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Polygon class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Polygon class.

*  Routines Defined:
*     AST_ISAPOLYGON
*     AST_POLYGON
*     AST_DOWNSIZE

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (Starlink)

*  History:
*     27-OCT-2004 (DSB):
*        Original version.
*     28-MAY-2009 (DSB):
*        Added AST_DOWNSIZE.
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
#include "polygon.h"             /* C interface to the Polygon class */

F77_LOGICAL_FUNCTION(ast_isapolygon)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAPOLYGON", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAPolygon( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_polygon)( INTEGER(FRAME),
                                   INTEGER(NPNT),
                                   INTEGER(INDIM),
                                   DOUBLE_ARRAY(POINTS),
                                   INTEGER(UNC),
                                   CHARACTER(OPTIONS),
                                   INTEGER(STATUS)
                                   TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(FRAME)
   GENPTR_INTEGER(NPNT)
   GENPTR_INTEGER(INDIM)
   GENPTR_DOUBLE_ARRAY(POINTS)
   GENPTR_INTEGER(UNC)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_POLYGON", NULL, 0 );
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

      RESULT = astP2I( astPolygon( astI2P( *FRAME ), *NPNT, *INDIM, POINTS,
                                   astI2P( *UNC ), "%s", options ) );
      astFree( options );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_downsize)( INTEGER(THIS),
                                    DOUBLE(MAXERR),
                                    INTEGER(MAXVERT),
                                    INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE(MAXERR)
   GENPTR_INTEGER(MAXVERT)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_DOWNSIZE", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astDownsize( astI2P( *THIS ), *MAXERR, *MAXVERT ) );
   )
   return RESULT;
}


/* AST_OUTLINE<X> requires a function for each possible data type, so
   define it via a macro. */
#define MAKE_AST_OUTLINE(f,F,Ftype,X,Xtype) \
F77_INTEGER_FUNCTION(ast_outline##f)( Ftype(VALUE), \
                                      INTEGER(OPER), \
                                      Ftype##_ARRAY(ARRAY), \
                                      INTEGER_ARRAY(LBND), \
                                      INTEGER_ARRAY(UBND), \
                                      DOUBLE(MAXERR), \
                                      INTEGER(MAXVERT), \
                                      INTEGER_ARRAY(INSIDE), \
                                      LOGICAL(STARPIX), \
                                      INTEGER(STATUS) ) { \
   GENPTR_##Ftype(VALUE) \
   GENPTR_INTEGER(OPER) \
   GENPTR_##Ftype##_ARRAY(ARRAY) \
   GENPTR_INTEGER_ARRAY(LBND) \
   GENPTR_INTEGER_ARRAY(UBND) \
   GENPTR_DOUBLE(MAXERR) \
   GENPTR_INTEGER(MAXVERT) \
   GENPTR_INTEGER_ARRAY(INSIDE) \
   GENPTR_LOGICAL(STARPIX) \
   GENPTR_INTEGER(STATUS) \
\
   F77_INTEGER_TYPE RESULT; \
\
   astAt( "AST_OUTLINE"#F, NULL, 0 ); \
   astWatchSTATUS( \
      RESULT = astP2I( astOutline##X( *VALUE, *OPER, (Xtype *) ARRAY, LBND, \
                                       UBND, *MAXERR, *MAXVERT, INSIDE, \
                                       F77_ISTRUE( *STARPIX ) ? 1 : 0 ) ); \
   ) \
   return RESULT; \
}

/* Invoke the above macro to define a function for each data
   type. Include synonyms for some functions. */
MAKE_AST_OUTLINE(d,D,DOUBLE,D,double)
MAKE_AST_OUTLINE(r,R,REAL,F,float)
MAKE_AST_OUTLINE(i,I,INTEGER,I,int)
MAKE_AST_OUTLINE(ui,UI,INTEGER,UI,unsigned int)
MAKE_AST_OUTLINE(s,S,WORD,S,short int)
MAKE_AST_OUTLINE(us,US,UWORD,US,unsigned short int)
MAKE_AST_OUTLINE(w,W,WORD,S,short int)
MAKE_AST_OUTLINE(uw,UW,UWORD,US,unsigned short int)
MAKE_AST_OUTLINE(b,B,BYTE,B,signed char)
MAKE_AST_OUTLINE(ub,UB,UBYTE,UB,unsigned char)
#undef MAKE_AST_OUTLINE


/* AST_CONVEX<X> requires a function for each possible data type, so
   define it via a macro. */
#define MAKE_AST_CONVEX(f,F,Ftype,X,Xtype) \
F77_INTEGER_FUNCTION(ast_convex##f)( Ftype(VALUE), \
                                      INTEGER(OPER), \
                                      Ftype##_ARRAY(ARRAY), \
                                      INTEGER_ARRAY(LBND), \
                                      INTEGER_ARRAY(UBND), \
                                      LOGICAL(STARPIX), \
                                      INTEGER(STATUS) ) { \
   GENPTR_##Ftype(VALUE) \
   GENPTR_INTEGER(OPER) \
   GENPTR_##Ftype##_ARRAY(ARRAY) \
   GENPTR_INTEGER_ARRAY(LBND) \
   GENPTR_INTEGER_ARRAY(UBND) \
   GENPTR_LOGICAL(STARPIX) \
   GENPTR_INTEGER(STATUS) \
\
   F77_INTEGER_TYPE RESULT; \
\
   astAt( "AST_CONVEX"#F, NULL, 0 ); \
   astWatchSTATUS( \
      RESULT = astP2I( astConvex##X( *VALUE, *OPER, (Xtype *) ARRAY, LBND, \
                                     UBND, F77_ISTRUE( *STARPIX ) ? 1 : 0 ) ); \
   ) \
   return RESULT; \
}

/* Invoke the above macro to define a function for each data
   type. Include synonyms for some functions. */
MAKE_AST_CONVEX(d,D,DOUBLE,D,double)
MAKE_AST_CONVEX(r,R,REAL,F,float)
MAKE_AST_CONVEX(i,I,INTEGER,I,int)
MAKE_AST_CONVEX(ui,UI,INTEGER,UI,unsigned int)
MAKE_AST_CONVEX(s,S,WORD,S,short int)
MAKE_AST_CONVEX(us,US,UWORD,US,unsigned short int)
MAKE_AST_CONVEX(w,W,WORD,S,short int)
MAKE_AST_CONVEX(uw,UW,UWORD,US,unsigned short int)
MAKE_AST_CONVEX(b,B,BYTE,B,signed char)
MAKE_AST_CONVEX(ub,UB,UBYTE,UB,unsigned char)
#undef MAKE_AST_CONVEX


