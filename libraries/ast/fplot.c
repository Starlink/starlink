/*
*+
*  Name:
*     fplot.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Plot class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Plot class.

*  Routines Defined:
*     AST_BORDER
*     AST_CLIP
*     AST_CURVE
*     AST_GRID
*     AST_GRIDLINE
*     AST_ISAPLOT
*     AST_MARK
*     AST_PLOT
*     AST_TEXT   

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: D.S. Berry (Starlink)

*  History:
*     23-OCT-1996 (DSB):
*        Original version.
*     14-NOV-1996 (DSB):
*        Method names shortened. CrvBreak removed.
*     21-NOV-1996 (DSB):
*        Method names changed, CLIP argument NBND removed.
*     18-DEC-1996 (DSB):
*        Argument UP changed to single precision and NCOORD removed 
*        in AST_TEXT.
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
#include "plot.h"                /* C interface to the Plot class */

F77_LOGICAL_FUNCTION(ast_isaplot)( INTEGER(THIS),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAPLOT", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAPlot( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_plot)( INTEGER(FRAME),
                                REAL_ARRAY(GRAPHBOX),
                                DOUBLE_ARRAY(BASEBOX),
                                CHARACTER(OPTIONS),
                                INTEGER(STATUS)
                                TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(FRAME)
   GENPTR_REAL_ARRAY(GRAPHBOX)
   GENPTR_DOUBLE_ARRAY(BASEBOX)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_PLOT", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astPlot( astI2P( *FRAME ), GRAPHBOX, BASEBOX, 
                                "%s", options ) );
      astFree( options );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_border)( INTEGER(THIS),
                                  INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_BORDER", NULL, 0 );
   astWatchSTATUS(
      RESULT = astBorder( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_SUBROUTINE(ast_clip)( INTEGER(THIS),
                          INTEGER(IFRAME),
                          DOUBLE_ARRAY(LBND),
                          DOUBLE_ARRAY(UBND),
                          INTEGER(STATUS) ){
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(IFRAME)
   GENPTR_DOUBLE_ARRAY(LBND)
   GENPTR_DOUBLE_ARRAY(UBND)

   astAt( "AST_CLIP", NULL, 0 );
   astWatchSTATUS(
      astClip( astI2P( *THIS ), *IFRAME, LBND, UBND );
   )
}

F77_SUBROUTINE(ast_gridline)( INTEGER(THIS),
                              INTEGER(AXIS),
                              DOUBLE_ARRAY(START),
                              DOUBLE(LENGTH),
                              INTEGER(STATUS) ){
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(AXIS)
   GENPTR_DOUBLE_ARRAY(START)
   GENPTR_DOUBLE(LENGTH)

   astAt( "AST_GRIDLINE", NULL, 0 );
   astWatchSTATUS(
      astGridLine( astI2P( *THIS ), *AXIS, START, *LENGTH );
   )
}

F77_SUBROUTINE(ast_curve)( INTEGER(THIS),
                           DOUBLE_ARRAY(START),
                           DOUBLE_ARRAY(FINISH),
                           INTEGER(STATUS) ){
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(START)
   GENPTR_DOUBLE_ARRAY(FINISH)

   astAt( "AST_CURVE", NULL, 0 );
   astWatchSTATUS(
      astCurve( astI2P( *THIS ), START, FINISH );
   )
}

F77_SUBROUTINE(ast_grid)( INTEGER(THIS),
                          INTEGER(STATUS) ){
   GENPTR_INTEGER(THIS)

   astAt( "AST_GRID", NULL, 0 );
   astWatchSTATUS(
      astGrid( astI2P( *THIS ) );
   )
}

F77_SUBROUTINE(ast_mark)( INTEGER(THIS),
                          INTEGER(NMARK),
                          INTEGER(NCOORD),
                          INTEGER(INDIM),
                          DOUBLE_ARRAY(IN),
                          INTEGER(TYPE),
                          INTEGER(STATUS) ){
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(NMARK)
   GENPTR_INTEGER(NCOORD)
   GENPTR_INTEGER(INDIM)
   GENPTR_DOUBLE_ARRAY(IN)
   GENPTR_INTEGER(TYPE)

   astAt( "AST_MARK", NULL, 0 );
   astWatchSTATUS(
      astMark( astI2P( *THIS ), *NMARK, *NCOORD, *INDIM,
               (const double (*)[])IN, *TYPE );
   )
}

F77_SUBROUTINE(ast_text)( INTEGER(THIS),
                          CHARACTER(TEXT), 
                          DOUBLE_ARRAY(POS),
                          REAL_ARRAY(UP),
                          CHARACTER(JUST),
                          INTEGER(STATUS)
                          TRAIL(TEXT)
                          TRAIL(JUST) ){
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(TEXT)
   GENPTR_DOUBLE_ARRAY(POS)
   GENPTR_REAL_ARRAY(UP)
   GENPTR_CHARACTER(JUST) 
   char *text, *just;

   astAt( "AST_TEXT", NULL, 0 );
   astWatchSTATUS(
      text = astString( TEXT, TEXT_length );
      just = astString( JUST, JUST_length );
      astText( astI2P( *THIS ), text, POS, UP, just );
      (void) astFree( (void *) text );
      (void) astFree( (void *) just );
   )
}
