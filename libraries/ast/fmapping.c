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
