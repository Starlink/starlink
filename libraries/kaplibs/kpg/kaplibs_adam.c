/*
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

*  Authors:
*     DSB: David S Berry
*     TIMJ: Tim Jenness
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     03-NOV-2005 (TIMJ):
*        Update GRP interface
*     {enter_further_changes_here}
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

   F77_CALL(kpg1_asget)( INTEGER_ARG(&INDF),
                         INTEGER_ARG(&NDIM),
                         LOGICAL_ARG(&EXACT),
                         LOGICAL_ARG(&TRIM),
                         LOGICAL_ARG(&REQINV),
                         INTEGER_ARRAY_ARG(SDIM),
                         INTEGER_ARRAY_ARG(SLBND),
                         INTEGER_ARRAY_ARG(SUBND),
                         INTEGER_ARG(&IWCS),
                         INTEGER_ARG(&STATUS) );

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

/* ------------------------------- */

F77_SUBROUTINE(kpg1_gtgrp)( CHARACTER(PARAM), INTEGER(IGRP), INTEGER(SIZE),
                            INTEGER(STATUS) TRAIL(PARAM) );

void kpg1Gtgrp( const char *param, Grp **igrp, int *size, int *status ){
   DECLARE_CHARACTER_DYN(PARAM);
   DECLARE_INTEGER(IGRP);
   DECLARE_INTEGER(SIZE);
   DECLARE_INTEGER(STATUS);

   F77_CREATE_CHARACTER(PARAM,strlen( param ));

   /* if *igrp is null we are creating the group from scratch */
   if (*igrp == NULL) {
     *igrp = grpInit( status );
   }
   IGRP = grp1Getid( *igrp, status );
   if ( *status != SAI__OK ) return;

   F77_EXPORT_CHARACTER(param,PARAM,PARAM_length);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_CALL(kpg1_gtgrp)( CHARACTER_ARG(PARAM),
                         INTEGER_ARG(&IGRP),
                         INTEGER_ARG(&SIZE),
                         INTEGER_ARG(&STATUS)
                         TRAIL_ARG(PARAM) );

   F77_FREE_CHARACTER(PARAM);
   F77_IMPORT_INTEGER( STATUS, *status );

   /* make sure status is imported before we call a C routine
      capable of setting status */
   grp1Setid( *igrp, IGRP, status );

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

   F77_CREATE_CHARACTER(PARAM,strlen( param ));
   F77_EXPORT_CHARACTER(param,PARAM,PARAM_length);
   F77_EXPORT_INTEGER(arrdim,ARRDIM);
   F77_EXPORT_INTEGER(npos,NPOS);
   F77_EXPORT_INTEGER(nax,NAX);
   F77_CREATE_DOUBLE_ARRAY( POS, arrdim*nax );
   F77_EXPORT_DOUBLE_ARRAY( pos, POS, arrdim*nax );
   F77_EXPORT_INTEGER(ifrm,IFRM);
   F77_EXPORT_INTEGER( astP2I( iwcs ), IWCS );
   F77_CREATE_CHARACTER(TITLE,strlen( title ));
   F77_EXPORT_CHARACTER(title,TITLE,TITLE_length);
   F77_EXPORT_INTEGER(id0,ID0);
   F77_CREATE_INTEGER_ARRAY( IDENTS, npos );
   F77_EXPORT_INTEGER_ARRAY( idents, IDENTS, npos );
   F77_EXPORT_LOGICAL(pnull,PNULL);
   F77_EXPORT_INTEGER(*status,STATUS);

   F77_CALL(kpg1_wrlst)( CHARACTER_ARG(PARAM),
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
                         TRAIL_ARG(TITLE) );

   F77_FREE_CHARACTER(PARAM);
   F77_FREE_CHARACTER(TITLE);
   F77_FREE_DOUBLE(POS);
   F77_FREE_INTEGER(IDENTS);
   F77_IMPORT_INTEGER( STATUS, *status );

   return;
}


