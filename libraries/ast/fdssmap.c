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
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

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
