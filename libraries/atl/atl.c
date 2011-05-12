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
*     BC: Brad Cavanagh (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     26-MAY-2006 (DSB):
*        Original version.
*     02-JUN-2006 (TIMJ):
*        Include prototypes.
*     7-FEB-2007 (DSB):
*        Added atlMgfts.
*     9-FEB-2007 (DSB):
*        Add atlPtftr
*     8-MAR-2007 (BC):
*        Add atlPtfti, atlPtfts
*     6-JUL-2009 (TIMJ):
*        Add atlRmblft
*     2010-05-07 (TIMJ):
*        Add atlPtftd
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009-2010 Science and Technology Facilities Council.
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

   F77_LOCK( F77_CALL(atl_axtrm)( INTEGER_ARG(&IWCS),
                        INTEGER_ARRAY_ARG(AXES),
                        INTEGER_ARRAY_ARG(LBND),
                        INTEGER_ARRAY_ARG(UBND),
                        DOUBLE_ARRAY_ARG(work),
                        INTEGER_ARG(&STATUS) ); )

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

   F77_LOCK( F77_CALL(atl_plroi)( INTEGER_ARG(&IPLOT),
                        INTEGER_ARG(&RPLOTS),
                        INTEGER_ARG(&STATUS) ); )

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

   F77_LOCK( F77_CALL(atl_mklut)( INTEGER_ARG(&IX),
                        INTEGER_ARG(&IY),
                        INTEGER_ARG(&NPNT),
                        INTEGER_ARG(&NVAR),
                        INTEGER_ARG(&FRM),
                        DOUBLE_ARRAY_ARG(table),
                        INTEGER_ARG(&MAP),
                        INTEGER_ARG(&STATUS) ); )


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

   F77_LOCK( F77_CALL(atl_mgfts)( INTEGER_ARG(&METHOD),
                        INTEGER_ARG(&FC1),
                        INTEGER_ARG(&FC2),
                        INTEGER_ARG(&FC3),
                        INTEGER_ARG(&STATUS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( FC3, ifc3 );
   *fc3 = astI2P( ifc3 );

   return;
}

F77_SUBROUTINE(atl_ptfti)( INTEGER(THIS),
                           CHARACTER(NAME),
                           INTEGER(VALUE),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(NAME)
                           TRAIL(COMMNT) );

void atlPtfti( AstFitsChan *this, const char *name, int value,
               const char *comment, int *status ){
   DECLARE_INTEGER(THIS);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_INTEGER(VALUE);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( this ), THIS );

   if( !astOK ) return;

   F77_CREATE_CHARACTER( NAME, strlen( name ) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );

   F77_EXPORT_INTEGER( value, VALUE );

   F77_CREATE_CHARACTER( COMMNT, strlen( comment ) );
   F77_EXPORT_CHARACTER( comment, COMMNT, COMMNT_length );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_ptfti)( INTEGER_ARG(&THIS),
                        CHARACTER_ARG(NAME),
                        INTEGER_ARG(&VALUE),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_FREE_CHARACTER( NAME );
   F77_FREE_CHARACTER( COMMNT );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(atl_ptftl)( INTEGER(THIS),
                           CHARACTER(NAME),
                           LOGICAL(VALUE),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(NAME)
                           TRAIL(COMMNT) );

void atlPtftl( AstFitsChan *this, const char *name, int value,
               const char *comment, int *status ){
   DECLARE_INTEGER(THIS);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_LOGICAL(VALUE);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( this ), THIS );

   if( !astOK ) return;

   F77_CREATE_CHARACTER( NAME, strlen( name ) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );

   F77_EXPORT_LOGICAL( value, VALUE );

   F77_CREATE_CHARACTER( COMMNT, strlen( comment ) );
   F77_EXPORT_CHARACTER( comment, COMMNT, COMMNT_length );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_ptftl)( INTEGER_ARG(&THIS),
                        CHARACTER_ARG(NAME),
                        LOGICAL_ARG(&VALUE),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_FREE_CHARACTER( NAME );
   F77_FREE_CHARACTER( COMMNT );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


F77_SUBROUTINE(atl_ptftr)( INTEGER(THIS),
                           CHARACTER(NAME),
                           REAL(VALUE),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(NAME)
                           TRAIL(COMMNT) );

void atlPtftr( AstFitsChan *this, const char *name, float value,
               const char *comment, int *status ){
   DECLARE_INTEGER(THIS);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_REAL(VALUE);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( this ), THIS );

   if( !astOK ) return;

   F77_CREATE_CHARACTER( NAME, strlen( name ) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );

   F77_EXPORT_REAL( value, VALUE );

   F77_CREATE_CHARACTER( COMMNT, strlen( comment ) );
   F77_EXPORT_CHARACTER( comment, COMMNT, COMMNT_length );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_ptftr)( INTEGER_ARG(&THIS),
                        CHARACTER_ARG(NAME),
                        REAL_ARG(&VALUE),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_FREE_CHARACTER( NAME );
   F77_FREE_CHARACTER( COMMNT );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(atl_ptftd)( INTEGER(THIS),
                           CHARACTER(NAME),
                           DOUBLE(VALUE),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(NAME)
                           TRAIL(COMMNT) );

void atlPtftd( AstFitsChan *this, const char *name, double value,
               const char *comment, int *status ){
   DECLARE_INTEGER(THIS);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_DOUBLE(VALUE);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( this ), THIS );

   if( !astOK ) return;

   F77_CREATE_CHARACTER( NAME, strlen( name ) );
   F77_EXPORT_CHARACTER( name, NAME, NAME_length );

   F77_EXPORT_DOUBLE( value, VALUE );

   F77_CREATE_CHARACTER( COMMNT, strlen( comment ) );
   F77_EXPORT_CHARACTER( comment, COMMNT, COMMNT_length );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_ptftd)( INTEGER_ARG(&THIS),
                        CHARACTER_ARG(NAME),
                        DOUBLE_ARG(&VALUE),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME)
                        TRAIL_ARG(COMMNT) ); )

   F77_FREE_CHARACTER( NAME );
   F77_FREE_CHARACTER( COMMNT );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(atl_ptfts)( INTEGER(THIS),
                           CHARACTER(NAME),
                           CHARACTER(VALUE),
                           CHARACTER(COMMNT),
                           INTEGER(STATUS)
                           TRAIL(NAME)
                           TRAIL(VALUE)
                           TRAIL(COMMNT) );

void atlPtfts( AstFitsChan *this, const char *name,
               const char *value, const char *comment, int *status ){
   DECLARE_INTEGER(THIS);
   DECLARE_CHARACTER_DYN(NAME);
   DECLARE_CHARACTER_DYN(VALUE);
   DECLARE_CHARACTER_DYN(COMMNT);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( astP2I( this ), THIS );

   if( !astOK ) return;

   F77_CREATE_EXPORT_CHARACTER( name, NAME );
   F77_CREATE_EXPORT_CHARACTER( value, VALUE );
   F77_CREATE_EXPORT_CHARACTER( comment, COMMNT );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_ptfts)( INTEGER_ARG(&THIS),
                        CHARACTER_ARG(NAME),
                        CHARACTER_ARG(VALUE),
                        CHARACTER_ARG(COMMNT),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(NAME)
                        TRAIL_ARG(VALUE)
                        TRAIL_ARG(COMMNT) ); )

   F77_FREE_CHARACTER( NAME );
   F77_FREE_CHARACTER( VALUE );
   F77_FREE_CHARACTER( COMMNT );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(atl_rmblft)( INTEGER(FC),
                            INTEGER(STATUS) );

void atlRmblft(  AstFitsChan * this, int *status ) {
  DECLARE_INTEGER(THIS);
  DECLARE_INTEGER(STATUS);

  F77_EXPORT_INTEGER( astP2I( this ), THIS );

  if( !astOK ) return;

  F77_EXPORT_INTEGER( *status, STATUS );

  F77_LOCK( F77_CALL(atl_rmblft)( INTEGER_ARG(&THIS),
                        INTEGER_ARG(&STATUS) ); )

  F77_IMPORT_INTEGER( STATUS, *status );
}


F77_SUBROUTINE(atl_tolut)( INTEGER(INMAP),
                           DOUBLE(XLO),
                           DOUBLE(XHI),
                           DOUBLE(DX),
                           CHARACTER(OPTS),
                           INTEGER(OUTMAP),
                           INTEGER(STATUS)
                           TRAIL(OPTS) );

void atlTolut( AstMapping *inmap, double xlo, double xhi, double dx,
               const char *opts, AstMapping **outmap, int *status ){

   DECLARE_INTEGER(INMAP);
   DECLARE_DOUBLE(XLO);
   DECLARE_DOUBLE(XHI);
   DECLARE_DOUBLE(DX);
   DECLARE_CHARACTER_DYN(OPTS);
   DECLARE_INTEGER(OUTMAP);
   DECLARE_INTEGER(STATUS);
   int ioutmap;

   F77_EXPORT_INTEGER( astP2I( inmap ), INMAP );

   if( !astOK ) return;

   F77_CREATE_EXPORT_CHARACTER( opts, OPTS );
   F77_EXPORT_DOUBLE( xlo, XLO );
   F77_EXPORT_DOUBLE( xhi, XHI );
   F77_EXPORT_DOUBLE( dx, DX );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_tolut)( INTEGER_ARG(&INMAP),
                        DOUBLE_ARG(&XLO),
                        DOUBLE_ARG(&XHI),
                        DOUBLE_ARG(&DX),
                        CHARACTER_ARG(OPTS),
                        INTEGER_ARG(&OUTMAP),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(OPTS) ); )

   F77_FREE_CHARACTER( OPTS );

   if( astOK ) {
      F77_IMPORT_INTEGER( OUTMAP, ioutmap );
      *outmap = astI2P( ioutmap );
   } else {
      *outmap = AST__NULL;
   }

   return;
}

F77_SUBROUTINE(atl_wcspx)( INTEGER(KM1),
                           INTEGER(KM2),
                           DOUBLE_ARRAY(CRPIX),
                           DOUBLE(OBSLON),
                           DOUBLE(OBSLAT),
                           INTEGER(IWCS),
                           INTEGER(STATUS) );

void atlWcspx( AstKeyMap *km1, AstKeyMap *km2, double crpix[3], double obslon,
               double obslat, AstFrameSet **iwcs, int *status ){

   DECLARE_INTEGER(KM1);
   DECLARE_INTEGER(KM2);
   DECLARE_DOUBLE_ARRAY(CRPIX,3);
   DECLARE_DOUBLE(OBSLON);
   DECLARE_DOUBLE(OBSLAT);
   DECLARE_INTEGER(IWCS);
   DECLARE_INTEGER(STATUS);
   int iiwcs, i;

   if( !astOK ) return;

   F77_EXPORT_INTEGER( astP2I( km1 ), KM1 );
   F77_EXPORT_INTEGER( astP2I( km2 ), KM2 );

   for( i = 0; i < 3; i++ ) CRPIX[ i ] = (F77_DOUBLE_TYPE) crpix[ i ];

   F77_EXPORT_DOUBLE( obslon, OBSLON );
   F77_EXPORT_DOUBLE( obslat, OBSLAT );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_wcspx)( INTEGER_ARG(&KM1),
                        INTEGER_ARG(&KM2),
                        DOUBLE_ARRAY_ARG(CRPIX),
                        DOUBLE_ARG(&OBSLON),
                        DOUBLE_ARG(&OBSLAT),
                        INTEGER_ARG(&IWCS),
                        INTEGER_ARG(&STATUS) ); )

   if( astOK ) {
      F77_IMPORT_INTEGER( IWCS, iiwcs );
      *iwcs = astI2P( iiwcs );
   } else {
      *iwcs = AST__NULL;
   }

   return;
}

F77_SUBROUTINE(atl_kychk)( INTEGER(KEYMAP),
                           CHARACTER(KEY),
                           CHARACTER(ERRMSG),
                           INTEGER(STATUS)
                           TRAIL(KEY)
                           TRAIL(ERRMSG) );

void atlKychk( AstKeyMap *keymap, const char *key, const char *errmsg,
               int *status ){
   DECLARE_INTEGER(KEYMAP);
   DECLARE_CHARACTER_DYN(KEY);
   DECLARE_CHARACTER_DYN(ERRMSG);
   DECLARE_INTEGER(STATUS);

   if( !astOK ) return;

   F77_EXPORT_INTEGER( astP2I( keymap ), KEYMAP );

   F77_CREATE_CHARACTER( KEY, strlen( key ) );
   F77_EXPORT_CHARACTER( key, KEY, KEY_length );

   F77_CREATE_CHARACTER( ERRMSG, strlen( errmsg ) );
   F77_EXPORT_CHARACTER( errmsg, ERRMSG, ERRMSG_length );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(atl_kychk)( INTEGER_ARG(&KEYMAP),
                        CHARACTER_ARG(KEY),
                        CHARACTER_ARG(ERRMSG),
                        INTEGER_ARG(&STATUS)
                        TRAIL_ARG(KEY)
                        TRAIL_ARG(ERRMSG) ); )

   F77_FREE_CHARACTER( KEY );
   F77_FREE_CHARACTER( ERRMSG );
   F77_IMPORT_INTEGER( STATUS, *status );
}




