/*
*  Name:
*     atl.c

*  Purpose:
*     Implement the C interface to the ATL library.

*  Description:
*     This module implements C-callable wrappers for the public
*     routines in the ATL library. The interface to these wrappers
*     is defined in atl.h.

*  Authors:
*     DSB: David S Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2006 (DSB):
*        Original version.
*     02-JUN-2006 (TIMJ):
*        Include prototypes.
*     7-FEB-2007 (DSB):
*        Added atlMgfts.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*/

/* Header files. */
/* ============= */
#include "f77.h"                 
#include "ast.h"
#include "atl.h"

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(atl_axtrm)( INTEGER(IWCS), 
                           INTEGER_ARRAY(AXES),
                           INTEGER_ARRAY(LBND),
                           INTEGER_ARRAY(UBND),
                           DOUBLE_ARRAY(WORK),
                           INTEGER(STATUS) );

void atlAxtrm( AstFrameSet *iwcs, int *axes, int *lbnd, int *ubnd, 
               double *work, int *status ){
   DECLARE_INTEGER(IWCS);
   DECLARE_INTEGER_ARRAY_DYN(AXES);
   DECLARE_INTEGER_ARRAY_DYN(LBND);
   DECLARE_INTEGER_ARRAY_DYN(UBND);
   DECLARE_INTEGER(STATUS);
   int ndim;

   if( !astOK ) return;

   ndim = astGetI( iwcs, "Nin" );

   F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
   F77_CREATE_INTEGER_ARRAY( AXES, ndim );
   F77_EXPORT_INTEGER_ARRAY( axes, AXES, ndim );
   F77_CREATE_INTEGER_ARRAY( LBND, ndim );
   F77_EXPORT_INTEGER_ARRAY( lbnd, LBND, ndim );
   F77_CREATE_INTEGER_ARRAY( UBND, ndim );
   F77_EXPORT_INTEGER_ARRAY( ubnd, UBND, ndim );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(atl_axtrm)( INTEGER_ARG(&IWCS),
                        INTEGER_ARRAY_ARG(AXES),
                        INTEGER_ARRAY_ARG(LBND),
                        INTEGER_ARRAY_ARG(UBND),
                        DOUBLE_ARRAY_ARG(work),
                        INTEGER_ARG(&STATUS) );

   F77_FREE_INTEGER( AXES );
   F77_FREE_INTEGER( LBND );
   F77_FREE_INTEGER( UBND );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(atl_plroi)( INTEGER(IPLOT), 
                           INTEGER(RPLOTS),
                           INTEGER(STATUS) );

void atlPlroi( AstPlot *iplot, AstKeyMap **rplots, int *status ){
   DECLARE_INTEGER(IPLOT);
   DECLARE_INTEGER(RPLOTS);
   DECLARE_INTEGER(STATUS);
   int irplots;

   if( !astOK ) return;

   F77_EXPORT_INTEGER( astP2I( iplot ), IPLOT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(atl_plroi)( INTEGER_ARG(&IPLOT),
                        INTEGER_ARG(&RPLOTS),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( RPLOTS, irplots );
   *rplots = astI2P( irplots );

   return;
}



F77_SUBROUTINE(atl_mklut)( INTEGER(IX), 
                           INTEGER(IY),
                           INTEGER(NPNT),
                           INTEGER(NVAR),
                           INTEGER(FRM),
                           DOUBLE_ARRAY(TABLE),
                           INTEGER(MAP),
                           INTEGER(STATUS) );

void atlMklut( int ix, int iy, int npnt, int nvar, AstFrame *frm, 
               double *table, AstMapping **map, int *status ){
   DECLARE_INTEGER(IX);
   DECLARE_INTEGER(IY);
   DECLARE_INTEGER(NPNT);
   DECLARE_INTEGER(NVAR);
   DECLARE_INTEGER(FRM);
   DECLARE_INTEGER(MAP);
   DECLARE_INTEGER(STATUS);
   int iobj;

   if( !astOK ) return;

   F77_EXPORT_INTEGER( ix, IX );
   F77_EXPORT_INTEGER( iy, IY );
   F77_EXPORT_INTEGER( npnt, NPNT );
   F77_EXPORT_INTEGER( nvar, NVAR );
   F77_EXPORT_INTEGER( astP2I( frm ), FRM );

   F77_CALL(atl_mklut)( INTEGER_ARG(&IX),
                        INTEGER_ARG(&IY),
                        INTEGER_ARG(&NPNT),
                        INTEGER_ARG(&NVAR),
                        INTEGER_ARG(&FRM),
                        DOUBLE_ARRAY_ARG(table),
                        INTEGER_ARG(&MAP),
                        INTEGER_ARG(&STATUS) );


   if( astOK ) {
      F77_IMPORT_INTEGER( MAP, iobj );
      *map = astI2P( iobj );
   } else {
      *map = AST__NULL;
   }      

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(atl_mgfts)( INTEGER(METHOD),
                           INTEGER(FC1), 
                           INTEGER(FC2), 
                           INTEGER(FC3),
                           INTEGER(STATUS) );

void atlMgfts( int method, AstFitsChan *fc1, AstFitsChan *fc2, 
               AstFitsChan **fc3, int *status ){
   DECLARE_INTEGER(METHOD);
   DECLARE_INTEGER(FC1);
   DECLARE_INTEGER(FC2);
   DECLARE_INTEGER(FC3);
   DECLARE_INTEGER(STATUS);
   int ifc3;

   if( !astOK ) return;

   F77_EXPORT_INTEGER( method, METHOD );
   F77_EXPORT_INTEGER( astP2I( fc1 ), FC1 );
   F77_EXPORT_INTEGER( astP2I( fc2 ), FC2 );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_CALL(atl_mgfts)( INTEGER_ARG(&METHOD),
                        INTEGER_ARG(&FC1),
                        INTEGER_ARG(&FC2),
                        INTEGER_ARG(&FC3),
                        INTEGER_ARG(&STATUS) );

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( FC3, ifc3 );
   *fc3 = astI2P( ifc3 );

   return;
}



