#include "f77.h"
#include "sae_par.h"
#include "ast.h"
#include "star/grp.h"
#include "mers.h"
#include "tcl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tcltalk.h"
#include "ccdaux.h"

   F77_SUBROUTINE(ccd1_tcurs)( CHARACTER_ARRAY(ndfnms), INTEGER(nndf),
                               CHARACTER(sname), CHARACTER(domain),
                               INTEGER_ARRAY(idi), DOUBLE_ARRAY(xipos),
                               DOUBLE_ARRAY(yipos), INTEGER(nipos),
                               LOGICAL(verbos),
                               DOUBLE_ARRAY(percnt), DOUBLE(zoom),
                               INTEGER(maxcanv), INTEGER(windim),
                               CHARACTER(mstyle), LOGICAL(centrd),
                               POINTER(ipio), POINTER(ipxo), POINTER(ipyo),
                               INTEGER(nopos), INTEGER(status)
                               TRAIL(ndfnms) TRAIL(sname) TRAIL(domain)
                               TRAIL(mstyle) ) {
/*
*+
*  Name:
*     CCD1_TCURS

*  Purpose:
*     Harness Tcl code to get points interactively from a displayed NDF.

*  Language:
*     ANSI C.

*  Invocation:
*     CALL CCD1_TCURS( NDFNMS, NNDF, SNAME, DOMAIN, IDI, XIPOS, YIPOS,
*                      NIPOS, VERBOS, PERCNT, ZOOM, MAXCANV, WINDIM,
*                      MSTYLE, CENTRD, IPIO, IPXO, IPYO, NOPOS, STATUS )

*  Description:
*     This routine calls a Tcl script which displays an NDF or Set
*     of NDFs in a window and allows the user to select points on it
*     using an intuitive graphical interface.  It returns a list of
*     the selected points to the calling routine.  If the list of
*     selected points is not empty on entry (if NIPOS is not zero)
*     then the list passed in to the routine will be used as a
*     starting point.

*  Arguments:
*     NDFNMS( * ) = CHARACTER * ( * ) (Given)
*        Name of an NDF to use.
*     NNDF = INTEGER (Given)
*        The number of NDFs in NDFNMS.  If greater than one, then they
*        will be treated as an NDF Set.
*     SNAME = CHARACTER * ( * ) (Given)
*        The name of the NDF Set.  If blank, a sensible name will
*        be used instead.
*     DOMAIN = CHARACTER * ( * ) (Given)
*        The AST domain in which the coordinates XPOS and YPOS are
*        given.
*     IDI( * ) = INTEGER (Given)
*        The index identifiers for the initial list of positions.
*     XIPOS( * ) = DOUBLE PRECISION (Given)
*        X coordinates of the initial list of positions, in DOMAIN
*        coordinates.
*     YIPOS( * ) = DOUBLE PRECISION (Given)
*        Y coordinates of the initial list of positions, in DOMAIN
*        coordinates.
*     NIPOS = INTEGER (Given)
*        Number of positions in the initial list (size of IDI, XIPOS,
*        YIPOS).
*     VERBOS = LOGICAL (Given)
*        If true, then all the postions will be written to the user
*        at the end.
*     PERCNT( 2 ) = DOUBLE PRECISION (Given and Returned)
*        Lower and higher percentiles to use in displaying the images.
*        They should satisfy 0 <= PERCNT( 0 ) <= PERCNT( 1 ) <= 100.
*     ZOOM = DOUBLE PRECISION (Given and Returned)
*        The zoom factor for the initial display (may be limited by
*        MAXCANV).
*     MAXCANV = INTEGER (Given and Returned)
*        The maximum X or Y dimension of canvas on which an initial NDF
*        is to be displayed (if zero there is no limit).
*     WINDIM( 2 ) = INTEGER (Given and Returned)
*        Dimensions of the window used for display.
*     MSTYLE = CHARACTER * ( * ) (Given and Returned)
*        A string indicating how markers are to be plotted on the image.
*     CENTRD = LOGICAL (Given and Returned)
*        Whether points should be centroided when they are added.
*     IPIO = INTEGER (Returned)
*        Pointer to the index idenfiers for the positions selected.
*     IPXO = DOUBLE PRECISION (Returned)
*        Pointer to the X coordinates of the positions selected, in
*        DOMAIN coordinates.
*     IPYO = DOUBLE PRECISION (Returned)
*        Pointer to the Y coordinates of the positions selected, in
*        DOMAIN coordinates.
*     NOPOS = INTEGER (Returned)
*        The number of X,Y positions selected (size of the arrays
*        pointed to by IPIO, IPXO, IPYO).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The pointers in the arrays IPIO, IPXO and IPYO are allocated
*     using CCD1_MALL by this routine, and should be freed by the
*     caller using CCD1_MFREE.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-APR-2000 (MBT):
*        Original version.
*     9-APR-2001 (MBT):
*        Upgraded for use with Sets.
*     11-APR-2005 (TIMJ):
*        Fix compiler warnings by including string.h and ccdaux.h
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given. */
      GENPTR_CHARACTER_ARRAY(ndfnms)
      GENPTR_INTEGER(nndf)
      GENPTR_CHARACTER(sname)
      GENPTR_CHARACTER(domain)
      GENPTR_INTEGER_ARRAY(idi)
      GENPTR_DOUBLE_ARRAY(xipos)
      GENPTR_DOUBLE_ARRAY(yipos)
      GENPTR_INTEGER(nipos)
      GENPTR_LOGICAL(verbos)

/* Arguments Given and Returned. */
      GENPTR_DOUBLE(zoom)
      GENPTR_DOUBLE_ARRAY(percnt)
      GENPTR_INTEGER(maxcanv)
      GENPTR_INTEGER_ARRAY(windim)
      GENPTR_CHARACTER(mstyle)
      GENPTR_LOGICAL(centrd)

/* Arguments Returned. */
      GENPTR_POINTER(ipio)
      GENPTR_POINTER(ipxo)
      GENPTR_POINTER(ipyo)
      GENPTR_INTEGER(nopos)

/* Global Status. */
      GENPTR_INTEGER(status)

/* Local variables. */
      ccdTcl_Interp *cinterp;
      char buffer[ CCDAUX_BUFLENG ];
      char cmstyle[ GRP__SZNAM + 1 ];
      char cdomain[ GRP__SZNAM + 1 ];
      char csname[ GRP__SZNAM + 1 ];
      char cndfname[ GRP__SZNAM + 1 ];
      char *errtext;
      F77_DOUBLE_TYPE *px;
      F77_DOUBLE_TYPE *py;
      F77_INTEGER_TYPE *pi;
      int i;

/* Test the global status. */
      if ( *status != SAI__OK ) return;

/* Initialise the Tcl interpreter. */
      cinterp = ccdTclStart( status );
      if ( *status != SAI__OK ) return;

/* Set the value of Tcl variables to be passed into the script. */
      cnfImprt( sname, sname_length, csname );
      cnfImprt( mstyle, mstyle_length, cmstyle );
      cnfImprt( domain, domain_length, cdomain );
      if ( *csname == '\0' || *nndf == 1 ) {
         strcpy( buffer, "{}" );
      }
      else {
         sprintf( buffer, "{set:%s}", csname );
      }
      ccdTclSetC( cinterp, "NDFSET", buffer, status );
      for ( i = 0; i < *nndf; i++ ) {
         cnfImprt( ndfnms + i * ndfnms_length, ndfnms_length, cndfname );
         ccdTclAppC( cinterp, "NDFSET", cndfname, status );
      }
      ccdTclSetC( cinterp, "DOMAIN", cdomain, status );
      ccdTclSetI( cinterp, "MAXPOS", 0, status );
      ccdTclSetD( cinterp, "ZOOM", *zoom, status );
      ccdTclSetD( cinterp, "PERCLO", percnt[ 0 ], status );
      ccdTclSetD( cinterp, "PERCHI", percnt[ 1 ], status );
      ccdTclSetI( cinterp, "MAXCANV", *maxcanv, status );
      ccdTclSetI( cinterp, "WINX", windim[ 0 ], status );
      ccdTclSetI( cinterp, "WINY", windim[ 1 ], status );
      ccdTclSetC( cinterp, "MARKSTYLE", cmstyle, status );
      ccdTclSetI( cinterp, "CENTROID", F77_ISTRUE(*centrd), status );
      ccdTclSetI( cinterp, "VERBOSE", F77_ISTRUE(*verbos), status );
      for ( i = 0; i < *nipos; i++ ) {
         sprintf( buffer, "%d %lf %lf", idi[ i ], xipos[ i ], yipos[ i ] );
         ccdTclAppC( cinterp, "POINTS", buffer, status );
      }

/* Execute the Tcl script. */
      ccdTclRun( cinterp, "idicurs.tcl", status );

/* Check for abnormal exit. */
      if ( *status == SAI__OK ) {
         errtext = ccdTclGetC( cinterp, "set ERROR", status );
         if ( *status == SAI__OK && *errtext ) {
            *status = SAI__ERROR;
            msgSetc( "TEXT", errtext );
            errRep( " ", "CCD1_TCURS_ERR: ^TEXT", status );
         }
         else if ( *status != SAI__OK ) {
            errAnnul( status );
         }
      }

/* Retrieve the other values generated by the script. */
      ccdTclGetI( cinterp, "llength $POINTS", nopos, status );
      if ( *nopos && *status == SAI__OK ) {
         px = (double *) ccdMall( "_DOUBLE", *nopos, status );
         py = (double *) ccdMall( "_DOUBLE", *nopos, status );
         pi = (int *) ccdMall( "_INTEGER", *nopos, status );
         *ipxo = cnfFptr( px );
         *ipyo = cnfFptr( py );
         *ipio = cnfFptr( pi );
         if ( *status == SAI__OK ) {
            char *fmt = "lindex [ lindex $POINTS %d ] %d";
            for ( i = 0; i < *nopos; i++ ) {
               sprintf( buffer, fmt, i, 0 );
               ccdTclGetI( cinterp, buffer, &pi[ i ], status );
               sprintf( buffer, fmt, i, 1 );
               ccdTclGetD( cinterp, buffer, &px[ i ], status );
               sprintf( buffer, fmt, i, 2 );
               ccdTclGetD( cinterp, buffer, &py[ i ], status );
            }
         }
         else {
            *ipxo = (F77_POINTER_TYPE) NULL;
            *ipyo = (F77_POINTER_TYPE) NULL;
            *ipio = (F77_POINTER_TYPE) NULL;
         }
      }
      if ( *status == SAI__OK ) {
         ccdTclGetD( cinterp, "set ZOOM", zoom, status );
         ccdTclGetD( cinterp, "set PERCLO", percnt, status );
         ccdTclGetD( cinterp, "set PERCHI", percnt + 1, status );
         ccdTclGetI( cinterp, "set MAXCANV", maxcanv, status );
         ccdTclGetI( cinterp, "set WINX", windim, status );
         ccdTclGetI( cinterp, "set WINY", windim + 1, status );
         ccdTclGetI( cinterp, "set CENTROID", centrd, status );
         *centrd = F77_ISTRUE(*centrd);
         cnfExprt( ccdTclGetC( cinterp, "set MARKSTYLE", status ),
                   mstyle, mstyle_length );
      }

/* Delete the Tcl interpreter. */
      ccdTclStop( cinterp, status );
   }

/* $Id$ */
