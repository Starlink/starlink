/*
*+
*  Name:
*     fmapping.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Mapping class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Mapping class.

*  Routines Defined:
*     AST_INVERT
*     AST_ISAMAPPING
*     AST_SIMPLIFY
*     AST_TRAN1
*     AST_TRAN2
*     AST_TRANN

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     11-JUL-1996 (RFWS):
*        Original version.
*     13-DEC-1996 (RFWS)
*        Added AST_SIMPLIFY.
*     28-MAY-1998 (RFWS):
*        Added AST_MAPBOX.
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
#include "mapping.h"             /* C interface to the Mapping class */

/* ---------------- */
#include <stdio.h>
F77_INTEGER_FUNCTION(ast__nearest)( ) { return 0; }
F77_INTEGER_FUNCTION(ast__linear)( ) { return 0; }
static F77_INTEGER_TYPE (* ast_resample_METHOD)();

#define MAKE_AST_RESAMPLE_METHOD(cabbrev,ctype,ftype) \
static int ast_resample_method##cabbrev( int ndim, \
                                         const int *lbnd, const int *ubnd, \
                                         const ctype *in, \
                                         const ctype *in_var, int npoint, \
                                         const int *offset, double *coord, \
                                         int flags, ctype badval, \
                                         const double *params, \
                                         ctype *out, ctype *out_var ) { \
   DECLARE_INTEGER(STATUS); \
\
   STATUS = astStatus; \
   return ( *ast_resample_METHOD )( INTEGER_ARG(&ndim), \
                                    INTEGER_ARRAY_ARG(lbnd), \
                                    INTEGER_ARRAY_ARG(ubnd), \
                                    ftype##_ARRAY_ARG(in), \
                                    ftype##_ARRAY_ARG(in_var), \
                                    INTEGER_ARG(&npoint), \
                                    INTEGER_ARRAY_ARG(offset), \
                                    DOUBLE_ARRAY_ARG(coord), \
                                    INTEGER_ARG(flags), \
                                    ftype##_ARG(&badval), \
                                    ftype##_ARRAY_ARG(out), \
                                    ftype##_ARRAY_ARG(out_var), \
                                    INTEGER_ARG(&STATUS) ); \
   astSetStatus( STATUS ); \
}
MAKE_AST_RESAMPLE_METHOD(D,double,DOUBLE)
MAKE_AST_RESAMPLE_METHOD(F,float,REAL)
MAKE_AST_RESAMPLE_METHOD(I,int,INTEGER)
MAKE_AST_RESAMPLE_METHOD(S,short int,WORD)
MAKE_AST_RESAMPLE_METHOD(US,unsigned short int,UWORD)
MAKE_AST_RESAMPLE_METHOD(B,signed char,BYTE)
MAKE_AST_RESAMPLE_METHOD(UB,unsigned char,UBYTE)
#undef MAKE_AST_RESAMPLE_METHOD

#define MAKE_AST_RESAMPLE(fabbrev,FABBREV,ftype,cabbrev) \
F77_INTEGER_FUNCTION(ast_resample##fabbrev)( INTEGER(THIS), \
                                             INTEGER(NDIM_IN), \
                                             INTEGER_ARRAY(LBND_IN), \
                                             INTEGER_ARRAY(UBND_IN), \
                                             ftype##_ARRAY(IN), \
                                             ftype##_ARRAY(IN_VAR), \
                                             F77_INTEGER_TYPE (* METHOD)(), \
                                             DOUBLE(ACC), \
                                             INTEGER(GRIDSIZE), \
                                             INTEGER(FLAGS), \
                                             ftype(BADVAL), \
                                             INTEGER(NDIM_OUT), \
                                             INTEGER_ARRAY(LBND_OUT), \
                                             INTEGER_ARRAY(UBND_OUT), \
                                             INTEGER_ARRAY(LBND), \
                                             INTEGER_ARRAY(UBND), \
                                             ftype##_ARRAY(OUT), \
                                             ftype##_ARRAY(OUT_VAR), \
                                             INTEGER(STATUS) ) { \
   GENPTR_INTEGER(THIS) \
   GENPTR_INTEGER(NDIM_IN) \
   GENPTR_INTEGER_ARRAY(LBND_IN) \
   GENPTR_INTEGER_ARRAY(UBND_IN) \
   GENPTR_##ftype##_ARRAY(IN) \
   GENPTR_##ftype##_ARRAY(IN_VAR) \
   GENPTR_DOUBLE(ACC) \
   GENPTR_INTEGER(GRIDSIZE) \
   GENPTR_INTEGER(FLAGS) \
   GENPTR_##ftype(BADVAL) \
   GENPTR_INTEGER(NDIM_OUT) \
   GENPTR_INTEGER_ARRAY(LBND_OUT) \
   GENPTR_INTEGER_ARRAY(UBND_OUT) \
   GENPTR_INTEGER_ARRAY(LBND) \
   GENPTR_INTEGER_ARRAY(UBND) \
   GENPTR_##ftype##_ARRAY(OUT) \
   GENPTR_##ftype##_ARRAY(OUT_VAR) \
   GENPTR_INTEGER(STATUS) \
   AstInterpolate##cabbrev method; \
   F77_INTEGER_TYPE RESULT; \
\
   astAt( "AST_RESAMPLE"#FABBREV, NULL, 0 ); \
   astWatchSTATUS( \
      if ( METHOD == F77_EXTERNAL_NAME(ast__nearest) ) { \
         method = (AstInterpolate##cabbrev) AST__NEAREST; \
      } else if ( METHOD == F77_EXTERNAL_NAME(ast__linear) ) { \
         method = (AstInterpolate##cabbrev) AST__LINEAR; \
      } else { \
         ast_resample_METHOD = METHOD; \
         method = ast_resample_method##cabbrev; \
      } \
      if ( *FLAGS & AST__USEVAR ) { \
printf("variance\n" );\
         RESULT = astResample##cabbrev( astI2P( *THIS ), *NDIM_IN, \
                                        LBND_IN, UBND_IN, IN, IN_VAR, \
                                        method, *ACC, *GRIDSIZE, \
                                        *FLAGS, *BADVAL, NULL, \
                                        *NDIM_OUT, LBND_OUT, UBND_OUT, \
                                        LBND, UBND, OUT, OUT_VAR ); \
      } else { \
printf("no variance\n" );\
         RESULT = astResample##cabbrev( astI2P( *THIS ), *NDIM_IN, \
                                        LBND_IN, UBND_IN, IN, NULL, \
                                        method, *ACC, *GRIDSIZE, \
                                        *FLAGS, *BADVAL, NULL, \
                                        *NDIM_OUT, LBND_OUT, UBND_OUT, \
                                        LBND, UBND, OUT, NULL ); \
      } \
   ) \
   return RESULT; \
}
MAKE_AST_RESAMPLE(d,D,DOUBLE,D)
MAKE_AST_RESAMPLE(r,R,REAL,F)
MAKE_AST_RESAMPLE(i,I,INTEGER,I)
MAKE_AST_RESAMPLE(s,S,WORD,S)
MAKE_AST_RESAMPLE(us,US,UWORD,US)
MAKE_AST_RESAMPLE(w,W,WORD,S)
MAKE_AST_RESAMPLE(uw,UW,UWORD,US)
MAKE_AST_RESAMPLE(b,B,BYTE,B)
MAKE_AST_RESAMPLE(ub,UB,UBYTE,UB)
#undef MAKE_AST_RESAMPLE

F77_SUBROUTINE(ast_invert)( INTEGER(THIS),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)

   astAt( "AST_INVERT", NULL, 0 );
   astWatchSTATUS(
      astInvert( astI2P( *THIS ) );
   )
}

F77_LOGICAL_FUNCTION(ast_isamapping)( INTEGER(THIS),
                                      INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAMAPPING", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAMapping( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_SUBROUTINE(ast_mapbox)( INTEGER(THIS),
                            DOUBLE_ARRAY(LBND_IN),
                            DOUBLE_ARRAY(UBND_IN),
                            LOGICAL(FORWARD),
                            INTEGER(COORD_OUT),
                            DOUBLE(LBND_OUT),
                            DOUBLE(UBND_OUT),
                            DOUBLE_ARRAY(XL),
                            DOUBLE_ARRAY(XU),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE_ARRAY(LBND_IN)
   GENPTR_DOUBLE_ARRAY(UBND_IN)
   GENPTR_LOGICAL(FORWARD)
   GENPTR_INTEGER(COORD_OUT)
   GENPTR_DOUBLE(LBND_OUT)
   GENPTR_DOUBLE(UBND_OUT)
   GENPTR_DOUBLE_ARRAY(XL)
   GENPTR_DOUBLE_ARRAY(XU)
   double lbnd_out;
   double ubnd_out;

   astAt( "AST_MAPBOX", NULL, 0 );
   astWatchSTATUS(
      astMapBox( astI2P( *THIS ), LBND_IN, UBND_IN, F77_ISTRUE( *FORWARD ),
                 *COORD_OUT, &lbnd_out, &ubnd_out, XL, XU );
      *LBND_OUT = lbnd_out;
      *UBND_OUT = ubnd_out;
   )
}

F77_INTEGER_FUNCTION(ast_simplify)( INTEGER(THIS),
                                    INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_SIMPLIFY", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astSimplify( astI2P( *THIS ) ) );
   )
   return RESULT;
}

F77_SUBROUTINE(ast_tran1)( INTEGER(THIS),
                           INTEGER(NPOINT),
                           DOUBLE(XIN),
                           LOGICAL(FORWARD),
                           DOUBLE(XOUT),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(NPOINT)
   GENPTR_DOUBLE(XIN)
   GENPTR_LOGICAL(FORWARD)
   GENPTR_DOUBLE(XOUT)

   astAt( "AST_TRAN1", NULL, 0 );
   astWatchSTATUS(
      astTran1( astI2P( *THIS ), *NPOINT, XIN, F77_ISTRUE( *FORWARD ), XOUT );
   )
}

F77_SUBROUTINE(ast_tran2)( INTEGER(THIS),
                           INTEGER(NPOINT),
                           DOUBLE(XIN),
                           DOUBLE(YIN),
                           LOGICAL(FORWARD),
                           DOUBLE(XOUT),
                           DOUBLE(YOUT),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(NPOINT)
   GENPTR_DOUBLE(XIN)
   GENPTR_DOUBLE(YIN)
   GENPTR_LOGICAL(FORWARD)
   GENPTR_DOUBLE(XOUT)
   GENPTR_DOUBLE(YOUT)

   astAt( "AST_TRAN2", NULL, 0 );
   astWatchSTATUS(
      astTran2( astI2P( *THIS ), *NPOINT, XIN, YIN,
                F77_ISTRUE( *FORWARD ), XOUT, YOUT );
   )
}

F77_SUBROUTINE(ast_trann)( INTEGER(THIS),
                           INTEGER(NPOINT),
                           INTEGER(NCOORD_IN),
                           INTEGER(INDIM),
                           DOUBLE_ARRAY(IN),
                           LOGICAL(FORWARD),
                           INTEGER(NCOORD_OUT),
                           INTEGER(OUTDIM),
                           DOUBLE_ARRAY(OUT),
                           INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(NPOINT)
   GENPTR_INTEGER(NCOORD_IN)
   GENPTR_INTEGER(INDIM)
   GENPTR_DOUBLE_ARRAY(IN)
   GENPTR_LOGICAL(FORWARD)
   GENPTR_INTEGER(NCOORD_OUT)
   GENPTR_INTEGER(OUTDIM)
   GENPTR_DOUBLE_ARRAY(OUT)

   astAt( "AST_TRANN", NULL, 0 );
   astWatchSTATUS(
      astTranN( astI2P( *THIS ), *NPOINT, *NCOORD_IN, *INDIM,
                (const double (*)[])IN, F77_ISTRUE( *FORWARD ),
                *NCOORD_OUT, *OUTDIM, (double (*)[]) OUT );
   )
}
