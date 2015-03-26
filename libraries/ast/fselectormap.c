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
                                       DOUBLE(BADVAL),
                                       CHARACTER(OPTIONS),
                                       INTEGER(STATUS)
                                       TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(NREG)
   GENPTR_INTEGER_ARRAY(REGS)
   GENPTR_DOUBLE(BADVAL)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   int i;
   char *options;
   AstObject **regs;

   astAt( "AST_SELECTORMAP", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );
      regs = astMalloc( sizeof(AstObject *) * (*NREG) );

/* Truncate the options string to exlucde any trailing spaces. */
      astChrTrunc( options );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }

         for ( i = 0; i < *NREG; i++ ) {
            regs[ i ] = astI2P( REGS[ i ] );
         }
      }


      RESULT = astP2I( astSelectorMap( *NREG, (void **) regs, *BADVAL, "%s",
                                       options ) );
      astFree( regs );
      astFree( options );
   )
   return RESULT;
}
