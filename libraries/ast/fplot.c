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
*     AST_BOUNDINGBOX
*     AST_CLIP
*     AST_CURVE
*     AST_GENCURVE
*     AST_GRID
*     AST_GRIDLINE
*     AST_ISAPLOT
*     AST_MARK
*     AST_PLOT
*     AST_POLYCURVE
*     AST_REGIONOUTLINE
*     AST_TEXT
*     AST_GRFSET
*     AST_GRFPUSH
*     AST_GRFPOP
*     AST_STRIPESCAPES
*     AST_GETGRFCONTEXT

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     11-AUG-1998 (DSB):
*        Added AST_POLYCURVE.
*     9-JAN-2001 (DSB):
*        Change argument "in" for astMark and astPolyCurve from type
*        "const double (*)[]" to "const double *".
*     13-JUN-2001 (DSB):
*        Modified to add support for astGenCurve, astGrfSet, astGrfPop,
*        astGrfPush and EXTERNAL grf functions.
*     14-AUG-2002 (DSB):
*        Added AST_BOUNDINGBOX.
*     8-JAN-2003 (DSB):
*        Include "string.h".
*     10-JUL-2006 (DSB):
*        Add AST_STRIPESCAPES
*     21-JUN-2007 (DSB):
*        - Avoid use of protected astGetGrfContext function.
*        - Change data type of GrfContext from integer to AST Object pointer.
*     29-JUN-2007 (DSB):
*        Added astGetGrfCOntext and removed astSetGrfContext.
*     30-AUG-2007 (DSB):
*        Use astGrfConID to get the identifier for the graphics context
*        KeyMap.
*/

/* Define the astFORTRAN77 macro which prevents error messages from
   AST C functions from reporting the file and line number where the
   error occurred (since these would refer to this file, they would
   not be useful). */
#define astFORTRAN77

#define MXSTRLEN 80              /* String length at which truncation starts
                                    within pgqtxt and pgptxt. */
/* Header files. */
/* ============= */
#include "string.h"
#include "ast_err.h"             /* AST error codes */
#include "f77.h"                 /* FORTRAN <-> C interface macros (SUN/209) */
#include "c2f77.h"               /* F77 <-> C support functions/macros */
#include "error.h"               /* Error reporting facilities */
#include "memory.h"              /* Memory handling facilities */
#include "plot.h"                /* C interface to the Plot class */
#include "grf.h"                 /* Low-level graphics interface */

/* Prototypes for external functions. */
/* ================================== */
/* This is the null function defined by the FORTRAN interface in
fobject.c. */
F77_SUBROUTINE(ast_null)( void );

static int FGAttrWrapper( AstPlot *, int, double, double *, int );
static int FGBBufWrapper( AstPlot * );
static int FGEBufWrapper( AstPlot * );
static int FGFlushWrapper( AstPlot * );
static int FGLineWrapper( AstPlot *, int, const float *, const float * );
static int FGMarkWrapper( AstPlot *, int, const float *, const float *, int );
static int FGTextWrapper( AstPlot *, const char *, float, float, const char *, float, float );
static int FGTxExtWrapper( AstPlot *, const char *, float, float, const char *, float, float, float *, float * );
static int FGCapWrapper( AstPlot *, int, int );
static int FGQchWrapper( AstPlot *, float *, float * );
static int FGScalesWrapper( AstPlot *, float *, float * );

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

/* Truncate the options string to exlucde any trailing spaces. */
      astChrTrunc( options );

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

F77_SUBROUTINE(ast_boundingbox)( INTEGER(THIS),
                          REAL_ARRAY(LBND),
                          REAL_ARRAY(UBND),
                          INTEGER(STATUS) ){
   GENPTR_INTEGER(THIS)
   GENPTR_REAL_ARRAY(LBND)
   GENPTR_REAL_ARRAY(UBND)

   astAt( "AST_BOUNDINGBOX", NULL, 0 );
   astWatchSTATUS(
      astBoundingBox( astI2P( *THIS ), LBND, UBND );
   )
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
               (const double *)IN, *TYPE );
   )
}

F77_SUBROUTINE(ast_polycurve)( INTEGER(THIS),
                               INTEGER(NPOINT),
                               INTEGER(NCOORD),
                               INTEGER(INDIM),
                               DOUBLE_ARRAY(IN),
                               INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(NPOINT)
   GENPTR_INTEGER(NCOORD)
   GENPTR_INTEGER(INDIM)
   GENPTR_DOUBLE_ARRAY(IN)

   astAt( "AST_POLYCURVE", NULL, 0 );
   astWatchSTATUS(
      astPolyCurve( astI2P( *THIS ), *NPOINT, *NCOORD, *INDIM,
                (const double *)IN );
   )
}

F77_SUBROUTINE(ast_regionoutline)( INTEGER(THIS),
                                   INTEGER(REGION),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(REGION)

   astAt( "AST_REGIONOUTLINE", NULL, 0 );
   astWatchSTATUS(
      astRegionOutline( astI2P( *THIS ), astI2P( *REGION ) );
   )
}

F77_SUBROUTINE(ast_gencurve)( INTEGER(THIS),
                              INTEGER(MAP),
                              INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(MAP)

   astAt( "AST_GENCURVE", NULL, 0 );
   astWatchSTATUS(
      astGenCurve( astI2P( *THIS ), astI2P( *MAP ) );
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

F77_SUBROUTINE(ast_grfpush)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   astAt( "AST_GRFPUSH", NULL, 0 );
   astWatchSTATUS(
      astGrfPush( astI2P( *THIS ) );
   )
}

F77_SUBROUTINE(ast_grfpop)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   astAt( "AST_GRFPOP", NULL, 0 );
   astWatchSTATUS(
      astGrfPop( astI2P( *THIS ) );
   )
}

F77_SUBROUTINE(ast_bbuf)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   astAt( "AST_BBUF", NULL, 0 );
   astWatchSTATUS(
      astBBuf( astI2P( *THIS ) );
   )
}

F77_SUBROUTINE(ast_ebuf)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   astAt( "AST_EBUF", NULL, 0 );
   astWatchSTATUS(
      astEBuf( astI2P( *THIS ) );
   )
}

F77_SUBROUTINE(ast_grfset)( INTEGER(THIS), CHARACTER(NAME),
                               AstGrfFun FUN, INTEGER(STATUS)
                               TRAIL(NAME) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(NAME)
   char *name;
   AstGrfFun fun;
   const char *class;      /* Object class */
   const char *method;     /* Current method */
   int ifun;               /* Index into grf function list */
   AstGrfWrap wrapper;     /* Wrapper function for C Grf routine*/

   method = "AST_GRFSET";
   class = "Plot";

   astAt( method, NULL, 0 );
   astWatchSTATUS(

/* Set the function pointer to NULL if a pointer to
   the null routine AST_NULL has been supplied. */
      fun = FUN;
      if ( fun == (AstGrfFun) F77_EXTERNAL_NAME(ast_null) ) {
         fun = NULL;
      }

      name = astString( NAME, NAME_length );
      astGrfSet( astI2P( *THIS ), name, fun );

      ifun = astGrfFunID( name, method, class );

      if( ifun == AST__GATTR ) {
         wrapper = (AstGrfWrap) FGAttrWrapper;

      } else if( ifun == AST__GBBUF ) {
         wrapper = (AstGrfWrap) FGBBufWrapper;

      } else if( ifun == AST__GEBUF ) {
         wrapper = (AstGrfWrap) FGEBufWrapper;

      } else if( ifun == AST__GFLUSH ) {
         wrapper = (AstGrfWrap) FGFlushWrapper;

      } else if( ifun == AST__GLINE ) {
         wrapper = (AstGrfWrap) FGLineWrapper;

      } else if( ifun == AST__GMARK ) {
         wrapper = (AstGrfWrap) FGMarkWrapper;

      } else if( ifun == AST__GTEXT ) {
         wrapper = (AstGrfWrap) FGTextWrapper;

      } else if( ifun == AST__GCAP ) {
         wrapper = (AstGrfWrap) FGCapWrapper;

      } else if( ifun == AST__GTXEXT ) {
         wrapper = (AstGrfWrap) FGTxExtWrapper;

      } else if( ifun == AST__GQCH ) {
         wrapper = (AstGrfWrap) FGQchWrapper;

      } else if( ifun == AST__GSCALES ) {
         wrapper = (AstGrfWrap) FGScalesWrapper;

      } else {
         wrapper = (AstGrfWrap) FGFlushWrapper;
         if( astOK ) astError( AST__INTER, "%s(%s): AST internal programming "
                   "error - Grf function id %d not yet supported.", status,
                   method, class, ifun );
      }
      astGrfWrapper( astI2P( *THIS ), name, wrapper );
   )
}

static int FGAttrWrapper( AstPlot *this, int attr, double value,
                          double *old_value, int prim ) {
   DECLARE_DOUBLE(OLDVAL);
   int ret;
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;

   if ( !astOK ) return 0;

   GRFCON = astP2I( astGrfConID( this ) );
   ret = ( *(int (*)( INTEGER(grfcon), INTEGER(attr), DOUBLE(value),
                      DOUBLE(old_value), INTEGER(prim) ))
                  this->grffun[ AST__GATTR ])(  INTEGER_ARG(&GRFCON),
                                                INTEGER_ARG(&attr),
                                                DOUBLE_ARG(&value),
                                                DOUBLE_ARG(&OLDVAL),
                                                INTEGER_ARG(&prim) );
   if( old_value ) *old_value = OLDVAL;
   return ret;
}

static int FGBBufWrapper( AstPlot *this ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)(INTEGER(grfcon))) this->grffun[ AST__GBBUF ])(INTEGER_ARG(&GRFCON));
}

static int FGEBufWrapper( AstPlot *this ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)(INTEGER(grfcon))) this->grffun[ AST__GEBUF ])(INTEGER_ARG(&GRFCON));
}

static int FGFlushWrapper( AstPlot *this ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)(INTEGER(grfcon))) this->grffun[ AST__GFLUSH ])(INTEGER_ARG(&GRFCON));
}

static int FGLineWrapper( AstPlot *this, int n, const float *x,
                          const float *y ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)( INTEGER(grfcon), INTEGER(n), REAL_ARRAY(x), REAL_ARRAY(y) ))
                  this->grffun[ AST__GLINE ])(  INTEGER_ARG(&GRFCON),
                                                INTEGER_ARG(&n),
                                                REAL_ARRAY_ARG(x),
                                                REAL_ARRAY_ARG(y) );
}

static int FGMarkWrapper( AstPlot *this, int n, const float *x,
                          const float *y, int type ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)( INTEGER(grfcon), INTEGER(n), REAL_ARRAY(x), REAL_ARRAY(y),
                       INTEGER(type) ))
                  this->grffun[ AST__GMARK ])(  INTEGER_ARG(&GRFCON),
                                                INTEGER_ARG(&n),
                                                REAL_ARRAY_ARG(x),
                                                REAL_ARRAY_ARG(y),
                                                INTEGER_ARG(&type) );
}

static int FGTextWrapper( AstPlot *this, const char *text, float x, float y,
                          const char *just, float upx, float upy ) {

   DECLARE_CHARACTER(LTEXT,MXSTRLEN);
   DECLARE_CHARACTER(LJUST,MXSTRLEN);
   int ftext_length;
   int fjust_length;

   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );

   ftext_length = strlen( text );
   if( ftext_length > LTEXT_length ) ftext_length = LTEXT_length;
   astStringExport( text, LTEXT, ftext_length );

   fjust_length = strlen( just );
   if( fjust_length > LJUST_length ) fjust_length = LJUST_length;
   astStringExport( just, LJUST, fjust_length );

   return ( *(int (*)( INTEGER(grfcon), CHARACTER(LTEXT), REAL(x), REAL(y),
                       CHARACTER(LJUST), REAL(upx), REAL(upy)
                       TRAIL(ftext) TRAIL(fjust) ) )
                  this->grffun[ AST__GTEXT ])(
                                      INTEGER_ARG(&GRFCON),
                                      CHARACTER_ARG(LTEXT),
                                      REAL_ARG(&x),
                                      REAL_ARG(&y),
                                      CHARACTER_ARG(LJUST),
                                      REAL_ARG(&upx),
                                      REAL_ARG(&upy)
                                      TRAIL_ARG(ftext)
                                      TRAIL_ARG(fjust) );
}

static int FGCapWrapper( AstPlot *this, int cap, int value ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)( INTEGER(grfcon), INTEGER(cap), INTEGER(value) ) )
                  this->grffun[ AST__GCAP ])(
                                      INTEGER_ARG(&GRFCON),
                                      INTEGER_ARG(&cap),
                                      INTEGER_ARG(&value) );
}

static int FGTxExtWrapper( AstPlot *this, const char *text, float x, float y,
                           const char *just, float upx, float upy, float *xb,
                           float *yb ) {
   DECLARE_CHARACTER(LTEXT,MXSTRLEN);
   DECLARE_CHARACTER(LJUST,MXSTRLEN);
   int ftext_length;
   int fjust_length;

   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );

   ftext_length = strlen( text );
   if( ftext_length > LTEXT_length ) ftext_length = LTEXT_length;
   astStringExport( text, LTEXT, ftext_length );

   fjust_length = strlen( just );
   if( fjust_length > LJUST_length ) fjust_length = LJUST_length;
   astStringExport( just, LJUST, fjust_length );

   return ( *(int (*)( INTEGER(grfcon), CHARACTER(LTEXT), REAL(x), REAL(y),
                       CHARACTER(LJUST), REAL(upx), REAL(upy),
                       REAL_ARRAY(xb), REAL_ARRAY(yb) TRAIL(ftext)
                       TRAIL(fjust) ) )
                  this->grffun[ AST__GTXEXT ])(
                                                INTEGER_ARG(&GRFCON),
                                                CHARACTER_ARG(LTEXT),
                                                REAL_ARG(&x),
                                                REAL_ARG(&y),
                                                CHARACTER_ARG(LJUST),
                                                REAL_ARG(&upx),
                                                REAL_ARG(&upy),
                                                REAL_ARRAY_ARG(xb),
                                                REAL_ARRAY_ARG(yb)
                                                TRAIL_ARG(ftext)
                                                TRAIL_ARG(fjust) );
}

static int FGQchWrapper( AstPlot *this, float *chv, float *chh ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)( INTEGER(grfcon), REAL(chv), REAL(chh) ) )
                  this->grffun[ AST__GQCH ])( INTEGER_ARG(&GRFCON), REAL_ARG(chv), REAL_ARG(chh) );
}

static int FGScalesWrapper( AstPlot *this, float *alpha, float *beta ) {
   F77_INTEGER_TYPE(GRFCON);
   int *status = astGetStatusPtr;
   if ( !astOK ) return 0;
   GRFCON = astP2I( astGrfConID( this ) );
   return ( *(int (*)( INTEGER(grfcon), REAL(alpha), REAL(beta) ) )
                  this->grffun[ AST__GSCALES ])( INTEGER_ARG(&GRFCON), REAL_ARG(alpha), REAL_ARG(beta) );
}


/* NO_CHAR_FUNCTION indicates that the f77.h method of returning a
   character result doesn't work, so add an extra argument instead and
   wrap this function up in a normal FORTRAN 77 function (in the file
   plot.f). */
#if NO_CHAR_FUNCTION
F77_SUBROUTINE(ast_stripescapes_a)( CHARACTER(RESULT),
#else
F77_SUBROUTINE(ast_stripescapes)( CHARACTER_RETURN_VALUE(RESULT),
#endif
                          CHARACTER(TEXT),
                          INTEGER(STATUS)
#if NO_CHAR_FUNCTION
                          TRAIL(RESULT)
#endif
                          TRAIL(TEXT) ) {
   GENPTR_CHARACTER(RESULT)
   GENPTR_CHARACTER(TEXT)
   char *text;
   const char *result;
   int i;

   astAt( "AST_STRIPESCAPES", NULL, 0 );
   astWatchSTATUS(
      text = astString( TEXT, TEXT_length );
      result = astStripEscapes( text );
      i = 0;
      if ( astOK ) {             /* Copy result */
         for ( ; result[ i ] && i < RESULT_length; i++ ) {
            RESULT[ i ] = result[ i ];
         }
      }
      while ( i < RESULT_length ) RESULT[ i++ ] = ' '; /* Pad with blanks */
      astFree( text );
   )
}

F77_LOGICAL_FUNCTION(ast_getgrfcontext)( INTEGER(THIS),
                                         INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETGRFCONTEXT", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astGetGrfContext( astI2P( *THIS ) ) );
   )
   return RESULT;
}




