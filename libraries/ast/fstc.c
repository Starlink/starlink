/*
*+
*  Name:
*     fstc.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Stc class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Stc class.

*  Routines Defined:
*     AST_ISASTC
*     AST_GETSTCREGION
*     AST_GETSTCCOORD
*     AST_GETSTCNCOORD

*  Copyright:
*     Copyright (C) 1997-2006 Council for the Central Laboratory of the
*     Research Councils

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public Licence as
*     published by the Free Software Foundation; either version 2 of
*     the Licence, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public Licence for more details.
*
*     You should have received a copy of the GNU General Public Licence
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (Starlink)

*  History:
*     22-NOV-2004 (DSB):
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
#include "stc.h"                 /* C interface to the Stc class */


F77_INTEGER_FUNCTION(ast_getstcncoord)( INTEGER(THIS),
                                        INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETSTCNCOORD", NULL, 0 );
   astWatchSTATUS(
      RESULT = astGetStcNCoord( astI2P( *THIS ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_getstccoord)( INTEGER(THIS),
                                       INTEGER(ICOORD),
                                       INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(ICOORD)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETSTCCOORD", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astGetStcCoord( astI2P( *THIS ), *ICOORD ) );
   )
   return RESULT;
}

F77_LOGICAL_FUNCTION(ast_isastc)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISASTC", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAStc( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_getstcregion)( INTEGER(THIS),
                                        INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETSTCREGION", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astGetStcRegion( astI2P( *THIS ) ) );
   )
   return RESULT;
}


