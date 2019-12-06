/*
*-
*+
*  Name:
*     kaplibs_adam.c

*  Purpose:
*     Implement the C interface to the ADAM functions in KAPLIBS library.

*  Description:
*     This module implements C-callable wrappers for the public ADAM
*     routines in the KAPLIBS library. The interface to these wrappers
*     is defined in kaplibs.h.

*  Notes:
*     - Given the size of the KAPLIBS library, providing a complete C
*     interface is probably not worth the effort. Instead, I suggest that
*     people who want to use KAPLIBS from C extend this file (and
*     kaplibs.h) to include any functions which they need but which are
*     not already included.

*  Copyright:
*     Copyright (C) 2005, 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S Berry
*     TIMJ: Tim Jenness
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        Update GRP interface
*     7-MAR-2006 (DSB):
*        Added KPG1_RGNDF and KPG1_WGNDF.
*     29-NOV-2006 (DSB):
*        Added kpg1Gtaxv.
*     22-MAR-2007 (DSB):
*        Added kpg1Gilst.
*     15-JUL-2008 (TIMJ):
*        const and size_t to match Grp
*     24-JUL-2008 (TIMJ):
*        Use more robust F77_CREATE_EXPORT_CHARACTER.
*     {enter_further_changes_here}

*-
*/

/* Header files. */
/* ============= */
#include "sae_par.h"
#include "ast.h"
#include "f77.h"
#include "star/grp.h"
#include "kaplibs.h"
#include "kaplibs_private.h"
#include <string.h>

/* Wrapper function implementations. */
/* ================================= */

F77_SUBROUTINE(kpg1_asget)( INTEGER(INDF), INTEGER(NDIM), LOGICAL(EXACT),
                            LOGICAL(TRIM), LOGICAL(REQINV), INTEGER_ARRAY(SDIM),
                            INTEGER_ARRAY(SLBND), INTEGER_ARRAY(SUBND),
                            INTEGER(IWCS), INTEGER(STATUS) );

void kpg1Asget( int indf, int ndim, int exact, int trim, int reqinv,
                int *sdim, int *slbnd, int *subnd, AstFrameSet **iwcs,
                int *status ){
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(NDIM);
   DECLARE_LOGICAL(EXACT);
   DECLARE_LOGICAL(TRIM);
   DECLARE_LOGICAL(REQINV);
   DECLARE_INTEGER_ARRAY_DYN(SDIM);
   DECLARE_INTEGER_ARRAY_DYN(SLBND);
   DECLARE_INTEGER_ARRAY_DYN(SUBND);
   DECLARE_INTEGER(IWCS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( ndim, NDIM );
   F77_EXPORT_LOGICAL( exact, EXACT );
   F77_EXPORT_LOGICAL( trim, TRIM );
   F77_EXPORT_LOGICAL( reqinv, REQINV );
   F77_CREATE_INTEGER_ARRAY( SDIM, ndim );
   F77_ASSOC_INTEGER_ARRAY( SDIM, sdim );
   F77_CREATE_INTEGER_ARRAY( SLBND, ndim );
   F77_ASSOC_INTEGER_ARRAY( SLBND, slbnd );
   F77_CREATE_INTEGER_ARRAY( SUBND, ndim );
   F77_ASSOC_INTEGER_ARRAY( SUBND, subnd );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_asget)( INTEGER_ARG(&INDF),
                         INTEGER_ARG(&NDIM),
                         LOGICAL_ARG(&EXACT),
                         LOGICAL_ARG(&TRIM),
                         LOGICAL_ARG(&REQINV),
                         INTEGER_ARRAY_ARG(SDIM),
                         INTEGER_ARRAY_ARG(SLBND),
                         INTEGER_ARRAY_ARG(SUBND),
                         INTEGER_ARG(&IWCS),
                         INTEGER_ARG(&STATUS) ); )

   {
      int tmp;
      F77_IMPORT_INTEGER( IWCS, tmp );
      *iwcs = astI2P( tmp );
   }

   F77_IMPORT_INTEGER_ARRAY( SDIM, sdim, ndim );
   F77_FREE_INTEGER( SDIM );

   F77_IMPORT_INTEGER_ARRAY( SLBND, slbnd, ndim );
   F77_FREE_INTEGER( SLBND );

   F77_IMPORT_INTEGER_ARRAY( SUBND, subnd, ndim );
   F77_FREE_INTEGER( SUBND );

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

F77_SUBROUTINE(kpg1_asget8)( INTEGER(INDF), INTEGER(NDIM), LOGICAL(EXACT),
                             LOGICAL(TRIM), LOGICAL(REQINV), INTEGER_ARRAY(SDIM),
                             INTEGER8_ARRAY(SLBND), INTEGER8_ARRAY(SUBND),
                             INTEGER(IWCS), INTEGER(STATUS) );

void kpg1Asget8( int indf, int ndim, int exact, int trim, int reqinv,
                int *sdim, hdsdim *slbnd, hdsdim *subnd, AstFrameSet **iwcs,
                int *status ){
   DECLARE_INTEGER(INDF);
   DECLARE_INTEGER(NDIM);
   DECLARE_LOGICAL(EXACT);
   DECLARE_LOGICAL(TRIM);
   DECLARE_LOGICAL(REQINV);
   DECLARE_INTEGER_ARRAY_DYN(SDIM);
   DECLARE_INTEGER8_ARRAY_DYN(SLBND);
   DECLARE_INTEGER8_ARRAY_DYN(SUBND);
   DECLARE_INTEGER(IWCS);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( indf, INDF );
   F77_EXPORT_INTEGER( ndim, NDIM );
   F77_EXPORT_LOGICAL( exact, EXACT );
   F77_EXPORT_LOGICAL( trim, TRIM );
   F77_EXPORT_LOGICAL( reqinv, REQINV );
   F77_CREATE_INTEGER_ARRAY( SDIM, ndim );
   F77_ASSOC_INTEGER_ARRAY( SDIM, sdim );
   F77_CREATE_INTEGER8_ARRAY( SLBND, ndim );
   F77_ASSOC_INTEGER8_ARRAY( SLBND, slbnd );
   F77_CREATE_INTEGER8_ARRAY( SUBND, ndim );
   F77_ASSOC_INTEGER8_ARRAY( SUBND, subnd );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_asget8)( INTEGER_ARG(&INDF),
                         INTEGER_ARG(&NDIM),
                         LOGICAL_ARG(&EXACT),
                         LOGICAL_ARG(&TRIM),
                         LOGICAL_ARG(&REQINV),
                         INTEGER_ARRAY_ARG(SDIM),
                         INTEGER8_ARRAY_ARG(SLBND),
                         INTEGER8_ARRAY_ARG(SUBND),
                         INTEGER_ARG(&IWCS),
                         INTEGER_ARG(&STATUS) ); )

   {
      int tmp;
      F77_IMPORT_INTEGER( IWCS, tmp );
      *iwcs = astI2P( tmp );
   }

   F77_IMPORT_INTEGER_ARRAY( SDIM, sdim, ndim );
   F77_FREE_INTEGER( SDIM );

   F77_IMPORT_INTEGER8_ARRAY( SLBND, slbnd, ndim );
   F77_FREE_INTEGER8( SLBND );

   F77_IMPORT_INTEGER8_ARRAY( SUBND, subnd, ndim );
   F77_FREE_INTEGER8( SUBND );

   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_gtgrp)( CHARACTER(PARAM), INTEGER(IGRP), INTEGER(SIZE),
                            INTEGER(STATUS) TRAIL(PARAM) );

void kpg1Gtgrp( const char *param, Grp **grp, size_t *size, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   IGRP = grpC2F( *grp, status );
   if ( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(param,PARAM);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_LOCK( F77_CALL(kpg1_gtgrp)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&IGRP),
                         INTEGER_ARG(&SIZE),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM) ); )

   F77_FREE_CHARACTER(PARAM);
   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( SIZE, *size );

   /* make sure status is imported before we call a C routine
      capable of setting status */
   *grp = (Grp *) grpF2C( IGRP, status );

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_wrlst)( CHARACTER(PARAM), INTEGER(ARRDIM), INTEGER(NPOS),
                            INTEGER(NAX), DOUBLE_ARRAY(POS), INTEGER(IFRM),
                            INTEGER(IWCS), CHARACTER(TITLE), INTEGER(ID0),
                            INTEGER_ARRAY(IDENTS), LOGICAL(PNULL),
                            INTEGER(STATUS) TRAIL(PARAM) TRAIL(TITLE) );

void kpg1Wrlst( const char *param, int arrdim, int npos, int nax, double *pos,
                int ifrm, AstFrameSet *iwcs, const char *title, int id0,
                int *idents, int pnull, int *status ){

   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(ARRDIM);
   DECLARE_INTEGER(NPOS);
   DECLARE_INTEGER(NAX);
   DECLARE_DOUBLE_ARRAY_DYN(POS);
   DECLARE_INTEGER(IFRM);
   DECLARE_INTEGER(IWCS);
   DECLARE_CHARACTER_DYN(TITLE);
   DECLARE_INTEGER(ID0);
   DECLARE_INTEGER_ARRAY_DYN(IDENTS);
   DECLARE_LOGICAL(PNULL);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER(param, PARAM);
   F77_EXPORT_INTEGER(arrdim,ARRDIM);
   F77_EXPORT_INTEGER(npos,NPOS);
   F77_EXPORT_INTEGER(nax,NAX);
   F77_CREATE_DOUBLE_ARRAY( POS, arrdim*nax );
   F77_EXPORT_DOUBLE_ARRAY( pos, POS, arrdim*nax );
   F77_EXPORT_INTEGER(ifrm,IFRM);
   F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
   F77_CREATE_EXPORT_CHARACTER(title,TITLE);
   F77_EXPORT_INTEGER(id0,ID0);
   F77_CREATE_INTEGER_ARRAY( IDENTS, npos );
   F77_EXPORT_INTEGER_ARRAY( idents, IDENTS, npos );
   F77_EXPORT_LOGICAL(pnull,PNULL);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_LOCK( F77_CALL(kpg1_wrlst)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&ARRDIM),
                         INTEGER_ARG(&NPOS),
                         INTEGER_ARG(&NAX),
                         DOUBLE_ARRAY_ARG(POS),
                         INTEGER_ARG(&IFRM),
                         INTEGER_ARG(&IWCS),
                         CHARACTER_ARG(TITLE),
                         INTEGER_ARG(&ID0),
                         INTEGER_ARRAY_ARG(IDENTS),
                         LOGICAL_ARG(&PNULL),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(TITLE) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_CHARACTER(PARAM);
   F77_FREE_CHARACTER(TITLE);
   F77_FREE_DOUBLE(POS);
   F77_FREE_INTEGER(IDENTS);

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_wrtab)( CHARACTER(PARAM), INTEGER(ARRDIM), INTEGER(NPOS),
                            INTEGER(NAX), DOUBLE_ARRAY(POS), INTEGER(IFRM),
                            INTEGER(IWCS), CHARACTER(TITLE), INTEGER(ID0),
                            INTEGER_ARRAY(IDENTS), INTEGER(LABS),
                            INTEGER(HIST), LOGICAL(PNULL),
                            INTEGER(STATUS) TRAIL(PARAM) TRAIL(TITLE) );

void kpg1Wrtab( const char *param, int arrdim, int npos, int nax, double *pos,
                int ifrm, AstFrameSet *iwcs, const char *title, int id0,
                int *idents, Grp *labs, Grp *hist, int pnull, int *status ){

   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(ARRDIM);
   DECLARE_INTEGER(NPOS);
   DECLARE_INTEGER(NAX);
   DECLARE_DOUBLE_ARRAY_DYN(POS);
   DECLARE_INTEGER(IFRM);
   DECLARE_INTEGER(IWCS);
   DECLARE_CHARACTER_DYN(TITLE);
   DECLARE_INTEGER(ID0);
   DECLARE_INTEGER_ARRAY_DYN(IDENTS);
   DECLARE_INTEGER(LABS);
   DECLARE_INTEGER(HIST);
   DECLARE_LOGICAL(PNULL);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER(param,PARAM);
   F77_EXPORT_INTEGER(arrdim,ARRDIM);
   F77_EXPORT_INTEGER(npos,NPOS);
   F77_EXPORT_INTEGER(nax,NAX);
   F77_CREATE_DOUBLE_ARRAY( POS, arrdim*nax );
   F77_EXPORT_DOUBLE_ARRAY( pos, POS, arrdim*nax );
   F77_EXPORT_INTEGER(ifrm,IFRM);
   F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
   F77_CREATE_EXPORT_CHARACTER(title, TITLE);
   F77_EXPORT_INTEGER(id0,ID0);
   F77_CREATE_INTEGER_ARRAY( IDENTS, npos );
   F77_EXPORT_INTEGER_ARRAY( idents, IDENTS, npos );
   F77_EXPORT_INTEGER( grpC2F( labs, status ), LABS );
   F77_EXPORT_INTEGER( grpC2F( hist, status ), HIST );
   F77_EXPORT_LOGICAL(pnull,PNULL);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_LOCK( F77_CALL(kpg1_wrtab)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&ARRDIM),
                         INTEGER_ARG(&NPOS),
                         INTEGER_ARG(&NAX),
                         DOUBLE_ARRAY_ARG(POS),
                         INTEGER_ARG(&IFRM),
                         INTEGER_ARG(&IWCS),
                         CHARACTER_ARG(TITLE),
                         INTEGER_ARG(&ID0),
                         INTEGER_ARRAY_ARG(IDENTS),
                         INTEGER_ARG(&LABS),
                         INTEGER_ARG(&HIST),
                         LOGICAL_ARG(&PNULL),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(TITLE) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_CHARACTER(PARAM);
   F77_FREE_CHARACTER(TITLE);
   F77_FREE_DOUBLE(POS);
   F77_FREE_INTEGER(IDENTS);

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_wrcat)( CHARACTER(PARAM), INTEGER(ARRDIM), INTEGER(NPOS),
                            INTEGER(NAX), DOUBLE_ARRAY(POS), INTEGER(IFRM),
                            INTEGER(IWCS), CHARACTER(TITLE), INTEGER(ID0),
                            INTEGER_ARRAY(IDENTS), INTEGER(KEYMAP),
                            INTEGER(LABS), INTEGER(HIST), LOGICAL(PNULL),
                            INTEGER(STATUS) TRAIL(PARAM) TRAIL(TITLE) );

void kpg1Wrcat( const char *param, int arrdim, int npos, int nax, double *pos,
                int ifrm, AstFrameSet *iwcs, const char *title, int id0,
                int *idents, AstKeyMap *keymap, Grp *labs, Grp *hist,
                int pnull, int *status ){

   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(ARRDIM);
   DECLARE_INTEGER(NPOS);
   DECLARE_INTEGER(NAX);
   DECLARE_DOUBLE_ARRAY_DYN(POS);
   DECLARE_INTEGER(IFRM);
   DECLARE_INTEGER(IWCS);
   DECLARE_CHARACTER_DYN(TITLE);
   DECLARE_INTEGER(ID0);
   DECLARE_INTEGER_ARRAY_DYN(IDENTS);
   DECLARE_INTEGER(KEYMAP);
   DECLARE_INTEGER(LABS);
   DECLARE_INTEGER(HIST);
   DECLARE_LOGICAL(PNULL);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER(param,PARAM);
   F77_EXPORT_INTEGER(arrdim,ARRDIM);
   F77_EXPORT_INTEGER(npos,NPOS);
   F77_EXPORT_INTEGER(nax,NAX);
   F77_CREATE_DOUBLE_ARRAY( POS, arrdim*nax );
   F77_EXPORT_DOUBLE_ARRAY( pos, POS, arrdim*nax );
   F77_EXPORT_INTEGER(ifrm,IFRM);
   F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
   F77_CREATE_EXPORT_CHARACTER(title, TITLE);
   F77_EXPORT_INTEGER(id0,ID0);
   F77_CREATE_INTEGER_ARRAY( IDENTS, npos );
   F77_EXPORT_INTEGER_ARRAY( idents, IDENTS, npos );
   F77_EXPORT_INTEGER( astP2I( keymap ), KEYMAP );
   F77_EXPORT_INTEGER( grpC2F( labs, status ), LABS );
   F77_EXPORT_INTEGER( grpC2F( hist, status ), HIST );
   F77_EXPORT_LOGICAL(pnull,PNULL);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_LOCK( F77_CALL(kpg1_wrcat)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&ARRDIM),
                         INTEGER_ARG(&NPOS),
                         INTEGER_ARG(&NAX),
                         DOUBLE_ARRAY_ARG(POS),
                         INTEGER_ARG(&IFRM),
                         INTEGER_ARG(&IWCS),
                         CHARACTER_ARG(TITLE),
                         INTEGER_ARG(&ID0),
                         INTEGER_ARRAY_ARG(IDENTS),
                         INTEGER_ARG(&KEYMAP),
                         INTEGER_ARG(&LABS),
                         INTEGER_ARG(&HIST),
                         LOGICAL_ARG(&PNULL),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(TITLE) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_FREE_CHARACTER(PARAM);
   F77_FREE_CHARACTER(TITLE);
   F77_FREE_DOUBLE(POS);
   F77_FREE_INTEGER(IDENTS);

   return;
}

/* ----------------------------------------------- */

F77_SUBROUTINE(kpg1_rgndf)( CHARACTER(PARAM), INTEGER(MAXSIZ), INTEGER(MINSIZ),
                            CHARACTER(TEXT), INTEGER(IGRP), INTEGER(SIZE),
                            INTEGER(STATUS) TRAIL(PARAM) TRAIL(TEXT) );

void kpg1Rgndf( const char *param, size_t maxsiz, size_t minsiz,
                 const char *text, Grp **grp, size_t *size, int *status ){

   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(MAXSIZ);
   DECLARE_INTEGER(MINSIZ);
   DECLARE_CHARACTER_DYN(TEXT);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );

   F77_EXPORT_INTEGER( maxsiz, MAXSIZ );
   F77_EXPORT_INTEGER( minsiz, MINSIZ );

   F77_CREATE_EXPORT_CHARACTER( text, TEXT );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_rgndf)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&MAXSIZ),
                         INTEGER_ARG(&MINSIZ),
                         CHARACTER_ARG(TEXT),
                         INTEGER_ARG(&IGRP),
                         INTEGER_ARG(&SIZE),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(TEXT) ); )


   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( TEXT );
   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_INTEGER( STATUS, *status );

   *grp = grpF2C( IGRP, status );

}


/* ----------------------------------------------- */

F77_SUBROUTINE(kpg1_wgndf)( CHARACTER(PARAM), INTEGER(IGRP0), INTEGER(MAXSIZ),
                            INTEGER(MINSIZ), CHARACTER(TEXT), INTEGER(IGRP),
                            INTEGER(SIZE), INTEGER(STATUS) TRAIL(PARAM)
                            TRAIL(TEXT) );

void kpg1Wgndf( const char *param, const Grp *grp0, size_t maxsiz,
                size_t minsiz, const char *text, Grp **grp, size_t *size,
                int *status ){

   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(IGRP0);
   DECLARE_INTEGER(MAXSIZ);
   DECLARE_INTEGER(MINSIZ);
   DECLARE_CHARACTER_DYN(TEXT);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   F77_EXPORT_INTEGER( grpC2F( grp0, status ), IGRP0 );

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );

   F77_EXPORT_INTEGER( maxsiz, MAXSIZ );
   F77_EXPORT_INTEGER( minsiz, MINSIZ );

   F77_CREATE_EXPORT_CHARACTER( text, TEXT );

   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_wgndf)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&IGRP0),
                         INTEGER_ARG(&MAXSIZ),
                         INTEGER_ARG(&MINSIZ),
                         CHARACTER_ARG(TEXT),
                         INTEGER_ARG(&IGRP),
                         INTEGER_ARG(&SIZE),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(TEXT) ); )


   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( TEXT );
   F77_IMPORT_INTEGER( SIZE, *size );
   F77_IMPORT_INTEGER( STATUS, *status );

   *grp = grpF2C( IGRP, status );

}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_gtaxv)( CHARACTER(PARAM),
                            INTEGER(MXVAL),
                            LOGICAL(EXACT),
                            INTEGER(FRAME),
                            INTEGER(IAXIS),
                            DOUBLE_ARRAY(AXVAL),
                            INTEGER(NVAL),
                            INTEGER(STATUS)
                            TRAIL(PARAM) );

void kpg1Gtaxv( const char *param, int mxval, int exact, AstFrame *frame,
                int iaxis, double *axval, int *nval, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(MXVAL);
   DECLARE_LOGICAL(EXACT);
   DECLARE_INTEGER(FRAME);
   DECLARE_INTEGER(IAXIS);
   DECLARE_DOUBLE_ARRAY_DYN(AXVAL);
   DECLARE_INTEGER(NVAL);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM);
   F77_EXPORT_INTEGER( mxval, MXVAL );
   F77_EXPORT_LOGICAL( exact, EXACT );
   F77_EXPORT_INTEGER( astP2I( frame ), FRAME );
   F77_EXPORT_INTEGER( iaxis, IAXIS );
   F77_CREATE_DOUBLE_ARRAY( AXVAL, mxval );
   F77_ASSOC_DOUBLE_ARRAY( AXVAL, axval );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_gtaxv)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&MXVAL),
                         LOGICAL_ARG(&EXACT),
                         INTEGER_ARG(&FRAME),
                         INTEGER_ARG(&IAXIS),
                         DOUBLE_ARRAY_ARG(AXVAL),
                         INTEGER_ARG(&NVAL),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM) ); )

   F77_FREE_CHARACTER( PARAM );

   F77_IMPORT_INTEGER( NVAL, *nval );
   F77_IMPORT_DOUBLE_ARRAY( AXVAL, axval, *nval );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


/* ------------------------------- */

F77_SUBROUTINE(kpg1_gilst)( INTEGER(LONUM),
                            INTEGER(UPNUM),
                            INTEGER(MAXLIN),
                            CHARACTER(PARAM),
                            INTEGER_ARRAY(FLAG),
                            INTEGER_ARRAY(NUMBER),
                            INTEGER(NDISP),
                            INTEGER(STATUS)
                            TRAIL(PARAM) );


void kpg1Gilst( int lonum, int upnum, int maxlin, const char *param,
                int *flag, int *number, int *ndisp, int *status ){
   DECLARE_INTEGER(LONUM);
   DECLARE_INTEGER(UPNUM);
   DECLARE_INTEGER(MAXLIN);
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER_ARRAY_DYN(FLAG);
   DECLARE_INTEGER_ARRAY_DYN(NUMBER);
   DECLARE_INTEGER(NDISP);
   DECLARE_INTEGER(STATUS);
   int nflag;
   nflag = upnum - lonum + 1;

   F77_EXPORT_INTEGER( lonum, LONUM );
   F77_EXPORT_INTEGER( upnum, UPNUM );
   F77_EXPORT_INTEGER( maxlin, MAXLIN );
   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_CREATE_DOUBLE_ARRAY( FLAG, flag );
   F77_ASSOC_DOUBLE_ARRAY( FLAG, flag );
   F77_CREATE_DOUBLE_ARRAY( NUMBER, number );
   F77_ASSOC_DOUBLE_ARRAY( NUMBER, number );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_gilst)( INTEGER_ARG(&LONUM),
                         INTEGER_ARG(&UPNUM),
                         INTEGER_ARG(&MAXLIN),
                         CHARACTER_ARG(PARAM),
                         INTEGER_ARRAY_ARG(FLAG),
                         INTEGER_ARRAY_ARG(NUMBER),
                         INTEGER_ARG(&NDISP),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM) ); )

   F77_FREE_CHARACTER( PARAM );
   F77_IMPORT_INTEGER( NDISP, *ndisp );
   F77_IMPORT_INTEGER_ARRAY( FLAG, flag, nflag );
   F77_IMPORT_INTEGER_ARRAY( NUMBER, number, *ndisp );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


/* ------------------------------- */

F77_SUBROUTINE(kpg1_darad)( CHARACTER(PARAM),
                            INTEGER(EL),
                            DOUBLE_ARRAY(ARRAY),
                            CHARACTER(METHDS),
                            LOGICAL(BAD),
                            DOUBLE(LOWER),
                            DOUBLE(UPPER),
                            INTEGER(STATUS)
                            TRAIL(PARAM)
                            TRAIL(METHDS) );

void kpg1Darad( const char *param, int el, double *array, const char *methds,
                int *bad, double *lower, double *upper, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(EL);
   DECLARE_DOUBLE_ARRAY_DYN(ARRAY);
   DECLARE_CHARACTER_DYN(METHDS);
   DECLARE_LOGICAL(BAD);
   DECLARE_DOUBLE(LOWER);
   DECLARE_DOUBLE(UPPER);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_EXPORT_INTEGER( el, EL );
   F77_CREATE_DOUBLE_ARRAY( ARRAY, el );
   F77_EXPORT_DOUBLE_ARRAY( array, ARRAY, el );
   F77_CREATE_EXPORT_CHARACTER( methds, METHDS );
   F77_EXPORT_LOGICAL( *bad, BAD );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_darad)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&EL),
                         DOUBLE_ARRAY_ARG(ARRAY),
                         CHARACTER_ARG(METHDS),
                         LOGICAL_ARG(&BAD),
                         DOUBLE_ARG(&LOWER),
                         DOUBLE_ARG(&UPPER),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(METHDS) ); )

   F77_IMPORT_INTEGER( STATUS, *status );
   F77_IMPORT_INTEGER( LOWER, *lower );
   F77_IMPORT_INTEGER( UPPER, *upper );

   F77_FREE_CHARACTER(PARAM);
   F77_FREE_CHARACTER(METHDS);
   F77_FREE_DOUBLE(ARRAY);
}



/* ------------------------------- */

/* Due to the fact that F77 and C access AST objects differently, the
   supplied "isa" function pointer needs to be a pointer to the F77
   AST_ISA... function, not the C astIsa... function. The AST_ISA...
   function should be case to "void (*)( void )". */


F77_SUBROUTINE(kpg1_gtobj)( CHARACTER(PARAM),
                            CHARACTER(CLASS),
                            void (*ISA)( void ),
                            INTEGER(IAST),
                            INTEGER(STATUS)
                            TRAIL(PARAM)
                            TRAIL(CLASS) );

void kpg1Gtobj( const char *param, const char *class, void (*isa)( void ),
                AstObject **obj, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_CHARACTER_DYN(CLASS);
   DECLARE_INTEGER(IAST);
   DECLARE_INTEGER(STATUS);
   int tmp;

   F77_CREATE_EXPORT_CHARACTER( param, PARAM );
   F77_CREATE_EXPORT_CHARACTER( class, CLASS );
   F77_EXPORT_INTEGER( *status, STATUS );

   F77_LOCK( F77_CALL(kpg1_gtobj)( CHARACTER_ARG(PARAM),
                         CHARACTER_ARG(CLASS),
                         isa,
                         INTEGER_ARG(&IAST),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM)
                         TRAIL_ARG(CLASS) ); )
   F77_IMPORT_INTEGER( IAST, tmp );
   *obj = astI2P( tmp );

   F77_FREE_CHARACTER( PARAM );
   F77_FREE_CHARACTER( CLASS );
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}

/* ------------------------------- */

F77_SUBROUTINE(kpg1_lgcmd)( CHARACTER(APPN), CHARACTER(PACK),
                            INTEGER(STATUS) TRAIL(PARAM) TRAIL(PACK) );

void kpg1Lgcmd( const char *appn, const char *pack, int *status ){
   DECLARE_CHARACTER_DYN(APPN);
   DECLARE_CHARACTER_DYN(PACK);
   DECLARE_INTEGER(STATUS);

   if ( *status != SAI__OK ) return;

   F77_CREATE_EXPORT_CHARACTER(appn,APPN);
   F77_CREATE_EXPORT_CHARACTER(pack,PACK);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_LOCK( F77_CALL(kpg1_lgcmd)( CHARACTER_ARG(APPN),
                                   CHARACTER_ARG(PACK),
                                   INTEGER_ARG(&STATUS)
                                   TRAIL_ARG(APPN)
                                   TRAIL_ARG(PACK) ); )

   F77_FREE_CHARACTER(APPN);
   F77_FREE_CHARACTER(PACK);
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}
