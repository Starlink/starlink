/*
*+
*  Name:
*     ferror.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Error module.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Error module.

*  Routines Defined:
*     None.

*  Copyright:
*     <COPYRIGHT_STATEMENT>

*  Authors:
*     RFWS: R.F. Warren-Smith (Starlink)

*  History:
*     15-JUL-1996 (RFWS):
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
#include "error.h"               /* C interface to the Error module */

/* At present there are no Fortran callable routines in this module. */
