#include "f77.h"
#include "sae_par.h"
#include "mers.h"
#include "tcl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tcltalk.h"
#include "ccdaux.h"
#include "star/grp.h"

#define BUFLENG 200

F77_SUBROUTINE(grp_get)( INTEGER(IGRP), INTEGER(INDEX),
                         INTEGER(SIZE), CHARACTER(NAMES),
                         INTEGER(STATUS)
                         TRAIL(NAMES) );

   F77_SUBROUTINE(ccd1_algn)( INTEGER(ndfgr), INTEGER(nndf), INTEGER(nset),
                              INTEGER_ARRAY(imem), INTEGER_ARRAY(imemof),
                              INTEGER(snamgr), INTEGER(refset),
                              DOUBLE_ARRAY(percnt), DOUBLE(zoom),
                              INTEGER(maxcanv), INTEGER_ARRAY(windim),
                              CHARACTER(mstyle), INTEGER(npoint),
                              POINTER_ARRAY(ipxpos), POINTER_ARRAY(ipypos),
                              POINTER_ARRAY(ipind), INTEGER(status)
                              TRAIL(mstyle) ) {

/*
*+
*  Name:
*     CCD1_ALGN

*  Purpose:
*     Get position lists by user interaction for a set of NDFs.

*  Language:
*     ANSI C.

*  Invocation:
*     CALL CCD1_ALGN( NDFGR, NNDF, NSET, IMEM, IMEMOF, SNAMGR, REFSET,
*                     PERCNT, ZOOM, MAXCNV, WINDIM, MSTYLE,
*                     NPOINT, IPXPOS, IPYPOS, IPIND, STATUS )

*  Description:
*     This routine calls a Tcl script which displays a number of NDFs
*     and allows the user to select a matching set of positions on
*     each member of the set.  It returns the lists of points to
*     the calling routine.

*  Arguments:
*     NDFGR = INTEGER (Given)
*        A GRP identifier for the names of the NDFs to be processed.
*     NNDF = INTEGER (Given)
*        The number of members of NDFGR.
*     NSET = INTEGER (Given)
*        The number of Sets represented in the NDFGR group.
*     IMEM( NNDF ) = INTEGER (Given)
*        Indexes of the members of NDFGR in Set order.
*     IMEMOF( NSET + 1 ) = INTEGER (Given)
*        Array of pointers into the IMEM array.
*     SNAMGR = INTEGER (Given)
*        GRP identifier for a group giving the Set Names of the Sets.
*     REFSET = INTEGER (Given)
*        The index of the Set which represetns the reference image.
*     PERCNT( 2 ) = DOUBLE PRECISION (Given and Returned)
*        Lower and higher percentiles to use in displaying the images.
*        They should satisfy 0 <= PERCNT( 0 ) <= PERCNT( 1 ) <= 100.
*     ZOOM = DOUBLE PRECISION (Given and Returned)
*        The zoom factor for the initial display.  May be limited by MAXCANV.
*     MAXCANV = INTEGER (Given and Returned)
*        The maximum X or Y dimension of the intial NDF display.
*     WINDIM( 2 ) = INTEGER (Given and Returned)
*        Dimensions of the window used for display.
*     MSTYLE = CHARACTER * ( * ) (Given and Returned)
*        A string indicating how markers are to be plotted on the image.
*     NPOINT = INTEGER( NSET ) (Returned)
*        The number of points contained in each of the output position
*        lists.
*     IPXPOS( NSET ) = INTEGER (Returned)
*        Pointers to arrays of the X coordinates of the marked positions
*        for each Set.  The array at position I has NPOINT( I ) elements
*        of type DOUBLE PRECISION.
*     IPYPOS( NSET ) = INTEGER (Returned)
*        Pointers to arrays of the Y coordinates of the marked positions
*        for each Set.  The array at position I has NPOINT( I ) elements
*        of type DOUBLE PRECISION.
*     IPIND( NSET ) = INTEGER (Returned)
*        Pointers to arrays of the index label of the marked positions
*        for each Set.  The array at position I has NPOINT( I ) elements
*        of type INTEGER.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The pointers in the arrays IPXPOS, IPYPOS and IPIND are allocated
*     using CCD1_MALL in this routine, and should be freed by the calling
*     routine using CCD1_MFREE.

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
*     {enter_new_authors_here}

*  History:
*     21-JUL-2000 (MBT):
*        Original version.
*     5-APR-2001 (MBT):
*        Upgraded for use with Sets.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given. */
      GENPTR_INTEGER(ndfgr)
      GENPTR_INTEGER(nndf)
      GENPTR_INTEGER(nset)
      GENPTR_INTEGER_ARRAY(imem)
      GENPTR_INTEGER_ARRAY(imemof)
      GENPTR_INTEGER(snamgr)
      GENPTR_INTEGER(refset)

/* Arguments Given and Returned. */
      GENPTR_DOUBLE_ARRAY(percnt)
      GENPTR_DOUBLE(zoom)
      GENPTR_INTEGER(maxcanv)
      GENPTR_INTEGER_ARRAY(windim)
      GENPTR_CHARACTER(mstyle)

/* Arguments Returned. */
      GENPTR_INTEGER_ARRAY(npoint)
      GENPTR_POINTER_ARRAY(ipxpos)
      GENPTR_POINTER_ARRAY(ipypos)
      GENPTR_POINTER_ARRAY(ipind)

/* Global Status. */
      GENPTR_INTEGER(status)

/* Local Variables. */
      char *fmt;
      char *cmstyle;
      ccdTcl_Interp *cinterp;
      char buffer[ BUFLENG ];
      int i;
      int ix;
      int j;
      int leng;
      F77_INTEGER_TYPE *pi;
      F77_DOUBLE_TYPE *px;
      F77_DOUBLE_TYPE *py;
      const int one = 1;
      DECLARE_CHARACTER( fndfname, GRP__SZNAM );
      DECLARE_CHARACTER( fsetname, GRP__SZNAM );
      char ndfname[ GRP__SZNAM + 1 ];
      char setname[ GRP__SZNAM + 1 ];
      char *errtext;

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Initialise the Tcl interpreter. */
      cinterp = ccdTclStart( status );

/* Construct a list of NDF Set constructor lists available to the Tcl
   interpreter as the value of the NDFSETS variable. */
      for ( i = 1; i <= *nset; i++ ) {
         F77_CALL(grp_get)( INTEGER_ARG(snamgr), INTEGER_ARG(&i),
                            INTEGER_ARG(&one), CHARACTER_ARG(fsetname),
                            INTEGER_ARG(status)
                            TRAIL_ARG(fsetname) );
         cnfImprt( fsetname, GRP__SZNAM, setname );
         if ( *setname == '\0' || imemof[ i ] - imemof[ i - 1 ] == 1 ) {
            strcpy( buffer, "{}" );
         }
         else {
            sprintf( buffer, "{set:%s}", setname );
         }
         ccdTclSetC( cinterp, "tmplist", buffer, status );
         for ( j = imemof[ i - 1 ]; j <= imemof[ i ] - 1; j++ ) {
            F77_CALL(grp_get)( INTEGER_ARG(ndfgr), INTEGER_ARG(&imem[ j - 1 ]),
                               INTEGER_ARG(&one), CHARACTER_ARG(fndfname),
                               INTEGER_ARG(status)
                               TRAIL_ARG(fndfname) );
            cnfImprt( fndfname, GRP__SZNAM, ndfname );
            ccdTclAppC( cinterp, "tmplist", ndfname, status );
         }
         ccdTclDo( cinterp, "lappend NDFSETS $tmplist", status );
      }

/* Set the value of other Tcl variables to be passed into the script. */
      if ( ( cmstyle = malloc( mstyle_length + 1 ) ) == NULL ) {
         *status = SAI__ERROR;
         errRep( " ", "Memory allocation failed", status );
         return;
      }
      cnfImprt( mstyle, mstyle_length, cmstyle );
      ccdTclSetI( cinterp, "REFSET", *refset - 1, status );
      ccdTclSetI( cinterp, "MAXPOS", 0, status );
      ccdTclSetD( cinterp, "PERCLO", percnt[ 0 ], status );
      ccdTclSetD( cinterp, "PERCHI", percnt[ 1 ], status );
      ccdTclSetD( cinterp, "ZOOM", *zoom, status );
      ccdTclSetI( cinterp, "MAXCANV", *maxcanv, status );
      ccdTclSetI( cinterp, "WINX", windim[ 0 ], status );
      ccdTclSetI( cinterp, "WINY", windim[ 1 ], status );
      ccdTclSetC( cinterp, "MARKSTYLE", cmstyle, status );

/* Execute the Tcl script. */
      ccdTclRun( cinterp, "ccdalign.tcl", status );

/* Check for abnormal exit. */
      if ( *status == SAI__OK ) {
         errtext = ccdTclGetC( cinterp, "set ERROR", status );
         if ( *status == SAI__OK && *errtext ) {
            *status = SAI__ERROR;
            msgSetc( "TEXT", errtext );
            errRep( " ", "CCD1_ALGN_ERR: ^TEXT", status );
         }
         else if ( *status != SAI__OK ) {
            errAnnul( status );
         }
      }

/* Retrieve the other variables set by the script. */
      if ( *status == SAI__OK ) {
         fmt = "lindex [ lindex [ lindex $POINTS %d ] %d ] %d";
         for ( i = 0; i < *nset; i++ ) {
            sprintf( buffer, "lindex $NPOINT %d", i );
            ccdTclGetI( cinterp, buffer, &npoint[ i ], status );
            px = (double *) ccdMall( "_DOUBLE", npoint[ i ], status );
            py = (double *) ccdMall( "_DOUBLE", npoint[ i ], status );
            pi = (int *) ccdMall( "_INTEGER", npoint[ i ], status );
            ipxpos[ i ] = cnfFptr( px );
            ipypos[ i ] = cnfFptr( py );
            ipind[ i ] = cnfFptr( pi );
            for ( j = 0; j < npoint[ i ]; j++ ) {
               sprintf( buffer, fmt, i, j, 0 );
               ccdTclGetI( cinterp, buffer, &pi[ j ], status );
               sprintf( buffer, fmt, i, j, 1 );
               ccdTclGetD( cinterp, buffer, &px[ j ], status );
               sprintf( buffer, fmt, i, j, 2 );
               ccdTclGetD( cinterp, buffer, &py[ j ], status );
            }
         }
         ccdTclGetD( cinterp, "set ZOOM", zoom, status );
         ccdTclGetD( cinterp, "set PERCLO", percnt, status );
         ccdTclGetD( cinterp, "set PERCHI", percnt + 1, status );
         ccdTclGetI( cinterp, "set MAXCANV", maxcanv, status );
         ccdTclGetI( cinterp, "set WINX", windim, status );
         ccdTclGetI( cinterp, "set WINY", windim + 1, status );
         cnfExprt( ccdTclGetC( cinterp, "set MARKSTYLE", status ),
                   mstyle, mstyle_length );
         free( cmstyle );
      }

/* Delete the Tcl interpreter. */
      ccdTclStop( cinterp, status );
   }

/* $Id$ */
