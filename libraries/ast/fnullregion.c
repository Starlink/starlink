/*
*+
*  Name:
*     fnullregion.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST NullRegion class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the NullRegion class.

*  Routines Defined:
*     AST_ISANULLREGION
*     AST_NULLREGION

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     12-OCT-2004 (DSB):
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
#include "nullregion.h"          /* C interface to the NullRegion class */


F77_LOGICAL_FUNCTION(ast_isanullregion)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISANULLREGION", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsANullRegion( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_nullregion)( INTEGER(FRAME),
                                      INTEGER(UNC),
                                      CHARACTER(OPTIONS),
                                      INTEGER(STATUS)
                                      TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(FRAME)
   GENPTR_INTEGER(UNC)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_NULLREGION", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }

      RESULT = astP2I( astNullRegion( astI2P( *FRAME ), astI2P( *UNC ), "%s", options ) );
      astFree( options );
   )
   return RESULT;
}
