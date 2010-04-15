/*
*+
*  Name:
*     fstcsearchlocation.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST StcSearchLocation class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the StcSearchLocation class.

*  Routines Defined:
*     AST_ISASTCSEARCHLOCATION
*     AST_STCSEARCHLOCATION

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
#include "stcsearchlocation.h" /* C interface to the StcSearchLocation class */


F77_LOGICAL_FUNCTION(ast_isastcsearchlocation)( INTEGER(THIS), INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISASTCSEARCHLOCATION", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsAStcSearchLocation( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_stcsearchlocation)( INTEGER(REG),
                               INTEGER(NCOORDS),
                               INTEGER_ARRAY(COORDS),
                               CHARACTER(OPTIONS),
                               INTEGER(STATUS)
                               TRAIL(OPTIONS) ) {
   GENPTR_INTEGER(REG)
   GENPTR_INTEGER(NCOORDS)
   GENPTR_CHARACTER(OPTIONS)
   GENPTR_INTEGER_ARRAY(COORDS)
   AstKeyMap **coords;
   F77_INTEGER_TYPE(RESULT);
   char *options;
   int i;

   astAt( "AST_STCSEARCHLOCATION", NULL, 0 );
   astWatchSTATUS(
      options = astString( OPTIONS, OPTIONS_length );

/* Change ',' to '\n' (see AST_SET in fobject.c for why). */
      if ( astOK ) {
         for ( i = 0; options[ i ]; i++ ) {
            if ( options[ i ] == ',' ) options[ i ] = '\n';
         }
      }

/* Convert supplied integers to pointers. */
      coords = astMalloc( sizeof( AstKeyMap * )*(size_t)( *NCOORDS ));
      if( astOK ) {
         for( i = 0; i < *NCOORDS; i++ ) {
            coords[ i ] = (AstKeyMap *) astMakePointer( astI2P( COORDS[ i ] ));
         }
      }

      RESULT = astP2I( astStcSearchLocation( astI2P( *REG ), *NCOORDS,
                                                   coords, "%s", options ) );
      astFree( coords );
      astFree( options );
   )
   return RESULT;
}
