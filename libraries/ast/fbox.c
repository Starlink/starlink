/*
*+
*  Name:
*     fbox.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Box class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Box class.

*  Routines Defined:
*     AST_ISABOX
*     AST_BOX

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
*     DSB: David S. Berry (Starlink)

*  History:
*     22-MAR-2004 (DSB):
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
#include "box.h"                 /* C interface to the Box class */

F77_LOGICAL_FUNCTION(ast_isabox)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISABOX", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsABox( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_box)( INTEGER(FRAME),
                               INTEGER(FORM),
                               DOUBLE_ARRAY(POINT1),
                               DOUBLE_ARRAY(POINT2),
                               INTEGER(UNC),
                               CHARACTER(OPTIONS),
                               INTEGER(STATUS)
                               TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(FRAME)
   GENPTR_INTEGER(FORM)
   GENPTR_DOUBLE_ARRAY(POINT1)
   GENPTR_DOUBLE_ARRAY(POINT2)
   GENPTR_INTEGER(UNC)
   GENPTR_CHARACTER(OPTIONS)
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_BOX", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Truncate the options string to exlucde any trailing spaces. */
      astChrTrunc( options );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }

      RESULT = astP2I( astBox( astI2P( *FRAME ), *FORM, POINT1, POINT2,
                               astI2P( *UNC ), "%s", options ) );
      astFree( options );
   )
   return RESULT;
}
