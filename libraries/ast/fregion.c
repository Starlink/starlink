/*
*+
*  Name:
*     fregion.c

*  Purpose:
*     Define a FORTRAN 77 interface to the AST Region class.

*  Type of Module:
*     C source file.

*  Description:
*     This file defines FORTRAN 77-callable C functions which provide
*     a public FORTRAN 77 interface to the Region class.

*  Routines Defined:
*     AST_NEGATE
*     AST_ISAREGION
*     AST_MAPREGION
*     AST_GETREGIONBOUNDS
*     AST_GETREGIONFRAME
*     AST_GETREGIONFRAMESET
*     AST_OVERLAP
*     AST_SETUNC
*     AST_GETUNC
*     AST_SHOWMESH

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
#include "memory.h"              /* Memory management facilities */
#include "region.h"              /* C interface to the Region class */

/* FORTRAN interface functions. */
/* ============================ */
/* These functions implement the remainder of the FORTRAN interface. */

F77_SUBROUTINE(ast_negate)( INTEGER(THIS),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)

   astAt( "AST_NEGATE", NULL, 0 );
   astWatchSTATUS(
      astNegate( astI2P( *THIS ) );
   )
}

F77_SUBROUTINE(ast_setunc)( INTEGER(THIS),
                            INTEGER(UNC),
                            INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(UNC)

   astAt( "AST_SETUNC", NULL, 0 );
   astWatchSTATUS(
      astSetUnc( astI2P( *THIS ), astI2P( *UNC ) );
   )
}

F77_LOGICAL_FUNCTION(ast_isaregion)( INTEGER(THIS),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_LOGICAL_TYPE(RESULT);

   astAt( "AST_ISAREGION", NULL, 0 );
   astWatchSTATUS(
      RESULT = astIsARegion( astI2P( *THIS ) ) ? F77_TRUE : F77_FALSE;
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_getregionframe)( INTEGER(THIS),
                                          INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETREGIONFRAME", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astGetRegionFrame( astI2P( *THIS ) ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_getregionframeset)( INTEGER(THIS),
                                             INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETREGIONFRAMESET", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astGetRegionFrameSet( astI2P( *THIS ) ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_getunc)( INTEGER(THIS),
                                  LOGICAL(DEF),
                                  INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_LOGICAL(DEF)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_GETUNC", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astGetUnc( astI2P( *THIS ), F77_ISTRUE( *DEF ) ? 1 : 0 ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_mapregion)( INTEGER(REG),
                                     INTEGER(MAP),
                                     INTEGER(FRM),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(REG)
   GENPTR_INTEGER(MAP)
   GENPTR_INTEGER(FRM)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_MAPREGION", NULL, 0 );
   astWatchSTATUS(
      RESULT = astP2I( astMapRegion( astI2P( *REG ), astI2P( *MAP ),
                                     astI2P( *FRM ) ) );
   )
   return RESULT;
}

F77_INTEGER_FUNCTION(ast_overlap)( INTEGER(THIS),
                                   INTEGER(THAT),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(THAT)
   F77_INTEGER_TYPE(RESULT);

   astAt( "AST_OVERLAP", NULL, 0 );
   astWatchSTATUS(
      RESULT = astOverlap( astI2P( *THIS ), astI2P( *THAT ) );
   )
   return RESULT;
}

/* AST_MASK<X> requires a function for each possible data type, so
   define it via a macro. */
#define MAKE_AST_MASK(f,F,Ftype,X,Xtype) \
F77_INTEGER_FUNCTION(ast_mask##f)( INTEGER(THIS), \
                                   INTEGER(MAP), \
                                   LOGICAL(INSIDE), \
                                   INTEGER(NDIM), \
                                   INTEGER_ARRAY(LBND), \
                                   INTEGER_ARRAY(UBND), \
                                   Ftype##_ARRAY(IN), \
                                   Ftype(VAL), \
                                   INTEGER(STATUS) ) { \
   GENPTR_INTEGER(THIS) \
   GENPTR_INTEGER(MAP) \
   GENPTR_LOGICAL(INSIDE) \
   GENPTR_INTEGER(NDIM) \
   GENPTR_INTEGER_ARRAY(LBND) \
   GENPTR_INTEGER_ARRAY(UBND) \
   GENPTR_##Ftype##_ARRAY(IN) \
   GENPTR_##Ftype(VAL) \
   GENPTR_INTEGER(STATUS) \
\
   F77_INTEGER_TYPE RESULT; \
\
   astAt( "AST_MASK"#F, NULL, 0 ); \
   astWatchSTATUS( \
\
      RESULT = astMask##X( astI2P( *THIS ), astI2P( *MAP ), \
                           F77_ISTRUE( *INSIDE ) ? 1 : 0, *NDIM, \
                           LBND, UBND, (Xtype *) IN, *VAL ); \
   ) \
   return RESULT; \
}

/* Invoke the above macro to define a function for each data
   type. Include synonyms for some functions. */
MAKE_AST_MASK(d,D,DOUBLE,D,double)
MAKE_AST_MASK(r,R,REAL,F,float)
MAKE_AST_MASK(i,I,INTEGER,I,int)
MAKE_AST_MASK(ui,UI,INTEGER,UI,unsigned int)
MAKE_AST_MASK(s,S,WORD,S,short int)
MAKE_AST_MASK(us,US,UWORD,US,unsigned short int)
MAKE_AST_MASK(w,W,WORD,S,short int)
MAKE_AST_MASK(uw,UW,UWORD,US,unsigned short int)
MAKE_AST_MASK(b,B,BYTE,B,signed char)
MAKE_AST_MASK(ub,UB,UBYTE,UB,unsigned char)
#undef MAKE_AST_MASK

F77_SUBROUTINE(ast_getregionbounds)( INTEGER(THIS),
                                     DOUBLE(LBND),
                                     DOUBLE(UBND),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_DOUBLE(XOUT)
   GENPTR_DOUBLE(YOUT)

   astAt( "AST_GETREGIONBOUNDS", NULL, 0 );
   astWatchSTATUS(
      astGetRegionBounds( astI2P( *THIS ), LBND, UBND );
   )
}

F77_SUBROUTINE(ast_showmesh)( INTEGER(THIS),
                              LOGICAL(FORMAT),
                              CHARACTER(TTL),
                              INTEGER(STATUS)
                              TRAIL(TTL) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_LOGICAL(FORMAT)
   GENPTR_CHARACTER(TTL)
   char *ttl;

   astAt( "AST_SHOWMESH", NULL, 0 );
   astWatchSTATUS(
      ttl = astString( TTL, TTL_length );
      astShowMesh( astI2P( *THIS ), F77_ISTRUE( *FORMAT ) ? 1 : 0, ttl );
      ttl = astFree( ttl );
   )
}

F77_SUBROUTINE(ast_getregionpoints)( INTEGER(THIS),
                                     INTEGER(MAXPOINT),
                                     INTEGER(MAXCOORD),
                                     INTEGER(NPOINT),
                                     DOUBLE_ARRAY(POINTS),
                                     INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_INTEGER(MAXPOINT)
   GENPTR_INTEGER(MAXCOORD)
   GENPTR_INTEGER(NPOINT)
   GENPTR_DOUBLE_ARRAY(POINTS)

   astAt( "AST_GETREGIONPOINT", NULL, 0 );
   astWatchSTATUS(
      astGetRegionPoints( astI2P( *THIS ), *MAXPOINT, *MAXCOORD, NPOINT,
                          POINTS );
   )
}

F77_SUBROUTINE(ast_getregionmesh)( INTEGER(THIS),
                                   LOGICAL(SURFACE),
                                   INTEGER(MAXPOINT),
                                   INTEGER(MAXCOORD),
                                   INTEGER(NPOINT),
                                   DOUBLE_ARRAY(POINTS),
                                   INTEGER(STATUS) ) {
   GENPTR_INTEGER(THIS)
   GENPTR_LOGICAL(SURFACE)
   GENPTR_INTEGER(MAXPOINT)
   GENPTR_INTEGER(MAXCOORD)
   GENPTR_INTEGER(NPOINT)
   GENPTR_DOUBLE_ARRAY(POINTS)

   astAt( "AST_GETREGIONMESH", NULL, 0 );
   astWatchSTATUS(
      astGetRegionMesh( astI2P( *THIS ), F77_ISTRUE( *SURFACE ), *MAXPOINT,
                        *MAXCOORD, NPOINT, POINTS );
   )
}


