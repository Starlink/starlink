/*
*+
*  Name:
*     ftimemap.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST TimeMap class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the TimeMap class.

*  Routines Defined:
*     AST_ISATIMEMAP
*     AST_TIMEADD
*     AST_TIMEMAP

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     NG: Norman Gray (Starlink)

*  History:
*     08-Sep-2003 (NG):
*        Original version (heavily based on fspecmap.c)
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
#include "timemap.h"             /* C interface to the TimeMap class */

F77_LOGICAL_FUNCTION(ast_isatimemap)( INTEGER(THIS),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISATIMEMAP", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsATimeMap( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_SUBROUTINE(ast_timeadd)( INTEGER(THIS),
                            CHARACTER(CVT),
                            DOUBLE_ARRAY(ARGS),
                            INTEGER(STATUS)
                            TRAIL(CVT) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_CHARACTER(CVT)
   GENPTR_DOUBLE_ARRAY(ARGS)
   char *cvt;

   astAt( "AST_TIMEADD", NULL, 0 );
   astWatchSTATUS(
      cvt = astString( CVT, CVT_length );
      astTimeAdd( astI2P( *THIS ), cvt, ARGS );
      astFree( cvt );
   )
}

F77_INTEGER_FUNCTION(ast_timemap)( INTEGER(FLAGS),
                                  CHARACTER(OPTIONS),
                                  INTEGER(STATUS)
                                  TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(FLAGS)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   int i;
   char *options;

   astAt( "AST_TIMEMAP", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }
      RESULT = astP2I( astTimeMap( *FLAGS, "%s", options ) );
      astFree( options );
   )
   return RESULT;
}
