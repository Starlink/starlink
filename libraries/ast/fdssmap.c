/*
*+
*  Name:
*     fdssmap.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST DssMap class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the DssMap class.

*  Routines Defined:
*     AST_ISADSSMAP

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     DSB: D.S. Berry (Starlink)
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     19-FEB-1997 (DSB):
*        Original version.
*     5-SEP-1997 (RFWS)
*        Removed the AST_DSSMAP function (now protected, so not
*        required in the Fortran interface).
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
#include "dssmap.h"              /* C interface to the DssMap class */

F77_LOGICAL_FUNCTION(ast_isadssmap)( INTEGER(THIS),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISADSSMAP", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsADssMap( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}
