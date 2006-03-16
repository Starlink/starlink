/*
*+
*  Name:
*     fselectormap.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST SelectorMap class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the SelectorMap class.

*  Routines Defined:
*     AST_ISASELECTORMAP
*     AST_SELECTORMAP

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S.Berry (Starlink)

*  History:
*     14-MAR-2006 (DSB):
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
#include "selectormap.h"         /* C interface to the SelectorMap class */

F77_LOGICAL_FUNCTION(ast_isaselectormap)( INTEGER(THIS),
                                        INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astWatchSTATUS(
   astAt( "AST_ISASELECTORMAP", NULL, 0 );
      RESULT = astIsASelectorMap( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_selectormap)( INTEGER(NREG),
                                       INTEGER_ARRAY(REGS),
                                       CHARACTER(OPTIONS),
                                       INTEGER(STATUS)
                                       TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(NREG)
   GENPTR_INTEGER_ARRAY(REGS)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   int i;
   char *options;
   AstObject **regs;

   astAt( "AST_SELECTORMAP", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );
      regs = astMalloc( sizeof(AstObject *) * (*NREG) );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }

         for ( i = 0; i < *NREG; i++ ) {
            regs[ i ] = astI2P( REGS[ i ] );
         }
      }


      RESULT = astP2I( astSelectorMap( *NREG, (void **) regs, "%s", 
                                       options ) );
      astFree( regs );
      astFree( options );
   )
   return RESULT;
}
