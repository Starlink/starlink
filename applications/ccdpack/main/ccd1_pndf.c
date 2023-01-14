#include "f77.h"
#include "sae_par.h"
#include "tcl.h"
#include "mers.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tcltalk.h"
#include "ccdaux.h"
#include "star/grp.h"

F77_SUBROUTINE(grp_get)( INTEGER(IGRP), INTEGER(INDEX),
                         INTEGER(SIZE), CHARACTER(NAMES),
                         INTEGER(STATUS)
                         TRAIL(NAMES) );

   F77_SUBROUTINE(ccd1_pndf)( INTEGER(ndfgid), INTEGER(nset),
                              INTEGER_ARRAY(imem), INTEGER_ARRAY(imemof),
                              INTEGER(namgid), DOUBLE_ARRAY(percnt),
                              LOGICAL(skip2), DOUBLE(zoom), INTEGER(maxcnv),
                              INTEGER_ARRAY(windim), INTEGER_ARRAY(prvdim),
                              CHARACTER(mstya), CHARACTER(mstyb),
                              INTEGER(count),
                              INTEGER_ARRAY(nodes), INTEGER_ARRAY(nmat),
                              DOUBLE_ARRAY(xoff), DOUBLE_ARRAY(yoff),
                              POINTER_ARRAY(ipx1), POINTER_ARRAY(ipy1),
                              POINTER_ARRAY(ipx2), POINTER_ARRAY(ipy2),
                              INTEGER(status)
                              TRAIL(mstya) TRAIL(mstyb) ) {
/*
*+
*  Name:
*     CCD1_PNDF

*  Purpose:
*     Harness Tcl code to get offsets between pairs of NDFs.

*  Language:
*     ANSI C.

*  Invocation:
*     CALL CCD1_PNDF( NDFGID, NSET, IMEM, IMEMOF, NAMGID, PERCNT, SKIP2,
*                     ZOOM, MAXCNV, WINDIM, PRVDIM, MSTYA, MSTYB,
*                     COUNT, NODES, NMAT, XOFF, YOFF, IPX1, IPY1,
*                     IPX2, IPY2, STATUS )

*  Description:
*     This routine calls a Tcl script which presents the user with a
*     graphical method of specifying offsets between pairs of Sets
*     derived from a group of NDFs, and returns the offsets thus
*     obtained.

*  Arguments:
*     NDFGID = INTEGER (Given)
*        GRP identifier of the group of NDFs which is to be presented
*        to the user.
*     NSET = INTEGER (Given)
*        The number of Sets referred to by IMEM.
*     IMEM( NSET ) = INTEGER (Given)
*        The group indices of the NDFs in Set order.
*     IMEMOF( NSET + 1 ) = INTEGER (Given)
*        Pointers into the IMEM array indicating where each Set's NDFs
*        begin.  The final entry is a sentinel.
*     NAMGID = INTEGER (Given)
*        The GRP identifier for a group holding the Set names
*        corresponding to the NSET Sets.  It should have NSET members.
*        Members with a value of ' ', meaning no alignment Set
*        information is available, are permitted.
*     PERCNT( 2 ) = DOUBLE PRECISION (Given)
*        Lower and higher percentiles to use in displaying the images.
*        They should satisfy 0 <= PERCNT( 0 ) <= PERCNT( 1 ) <= 100.
*     SKIP2 = LOGICAL (Given)
*        If true, then don't bother to present the user with a Chooser
*        widget in the case where it's not required because there are
*        only two NDFs.
*     ZOOM = DOUBLE PRECISION (Given and Returned)
*        The zoom factor for the initial display.  May be limited by MAXCNV.
*     MAXCNV = INTEGER (Given and Returned)
*        The maximum X or Y dimension of the intial NDF display.
*     WINDIM( 2 ) = INTEGER (Given and Returned)
*        Dimensions of the window used for display.
*     PRVDIM( 2 ) = INTEGER (Given and Returned)
*        Dimensions of the preview window for each Set used in the
*        chooser widget.
*     MSTYA = CHARACTER * ( * ) (Given and Returned)
*        A string indicating how markers are to be plotted on the first
*        image.
*     MSTYB = CHARACTER * ( * ) (Given and Returned)
*        A string indicating how markers are to be plotted on the second
*        image.
*     COUNT = INTEGER (Returned)
*        Number of pairings made by the user.
*     NODES( 2, * ) = INTEGER (Returned)
*        For each pairing made, this array contains the indices of the
*        Sets which form the pair.  The first COUNT pairs
*        of elements in this array are filled on return.
*     NMAT( * ) = INTEGER (Returned)
*        For each pairing made, the number of objects which were matched
*        up together by the centroiding routine in order to find an
*        accurate offset.  This can be used as a weighting indicating
*        the accuracy of the returned offsets.  The first COUNT elements
*        of this array are filled on return.
*     XOFF( * ) = DOUBLE PRECISION (Returned)
*        For each pairing I made this array contains the accurate X
*        offset between the Sets with indices NODES( 1, I ) and
*        NODES( 2, I ).  The first COUNT elements in this array are
*        filled on return.
*     YOFF( * ) = DOUBLE PRECISION (Returned)
*        For each pairing I made this array contains the accurate Y
*        offset between the Sets with indices NODES( 1, I ) and
*        NODES( 2, I ).  The first COUNT elements in this array are
*        filled on return.
*     IPX1( * ) = INTEGER (Returned)
*        For each pairing this gives a pointer to the centroided X
*        coordinates of the points in the first of the pair which
*        were successfully matched up.  The array pointed to contains
*        NMAT( * ) elements.  These pointers are allocated within this
*        routine.
*     IPY1( * ) = INTEGER (Returned)
*        For each pairing this gives a pointer to the centroided Y
*        coordinates of the points in the first of the pair which
*        were successfully matched up.  The array pointed to contains
*        NMAT( * ) elements.  These pointers are allocated within this
*        routine.
*     IPX2( * ) = INTEGER (Returned)
*        For each pairing this gives a pointer to the centroided X
*        coordinates of the points in the second of the pair which
*        were successfully matched up.  The array pointed to contains
*        NMAT( * ) elements.  These pointers are allocated within this
*        routine.
*     IPY2( * ) = INTEGER (Returned)
*        For each pairing this gives a pointer to the centroided Y
*        coordinates of the points in the second of the pair which
*        were successfully matched up.  The array pointed to contains
*        NMAT( * ) elements.  These pointers are allocated within this
*        routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     The pointers in the arrays IPX1, IPY1, IPX2, IPY2 are allocated
*     within this array using CCD1_MALL, and it is the responsibility
*     of the calling routine to free them.

*  Copyright:
*     Copyright (C) 2000 Central Laboratory of the Research Councils

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
*     6-SEP-2000 (MBT):
*        Original version.
*     7-MAR-2001 (MBT):
*        Upgraded for use with Sets.
*     11-APR-2005 (TIMJ):
*        Fix compiler warning by including string.h
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments Given. */
      GENPTR_INTEGER(ndfgid)
      GENPTR_INTEGER(nset)
      GENPTR_INTEGER_ARRAY(imem)
      GENPTR_INTEGER_ARRAY(imemof)
      GENPTR_INTEGER(namgid)
      GENPTR_DOUBLE_ARRAY(percnt)
      GENPTR_LOGICAL(skip2)

/* Arguments Given and Returned. */
      GENPTR_DOUBLE(zoom)
      GENPTR_INTEGER(maxcnv)
      GENPTR_INTEGER_ARRAY(windim)
      GENPTR_INTEGER_ARRAY(prvdim)
      GENPTR_CHARACTER(mstya)
      GENPTR_CHARACTER(mstyb)

/* Arguments Returned. */
      GENPTR_INTEGER(count)
      GENPTR_INTEGER_ARRAY(nodes)
      GENPTR_INTEGER_ARRAY(nmat)
      GENPTR_DOUBLE_ARRAY(xoff)
      GENPTR_DOUBLE_ARRAY(yoff)
      GENPTR_POINTER_ARRAY(ipx1)
      GENPTR_POINTER_ARRAY(ipy1)
      GENPTR_POINTER_ARRAY(ipx2)
      GENPTR_POINTER_ARRAY(ipy2)

/* Global Status. */
      GENPTR_INTEGER(status)

/* Local variables. */
      int i;
      int j;
      const int one = 1;
      ccdTcl_Interp *cinterp;
      DECLARE_CHARACTER( fndfname, GRP__SZNAM );
      DECLARE_CHARACTER( fsetname, GRP__SZNAM );
      char buffer[ 1024 ];
      char ndfname[ GRP__SZNAM + 1 ];
      char setname[ GRP__SZNAM + 1 ];
      char *cmstya;
      char *cmstyb;

/* Test the global status. */
      if ( *status != SAI__OK ) return;

/* Initialise the Tcl interpreter. */
      cinterp = ccdTclStart( status );

/* Construct a list of NDF names available to the Tcl interpreter as
   the value of the NDFSETS variable. */
      for ( i = 1; i <= *nset; i++ ) {
         setname[0] = '\0';
         F77_CALL(grp_get)( INTEGER_ARG(namgid), INTEGER_ARG(&i),
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
            F77_CALL(grp_get)( INTEGER_ARG(ndfgid), INTEGER_ARG(&imem[ j - 1 ]),
                               INTEGER_ARG(&one), CHARACTER_ARG(fndfname),
                               INTEGER_ARG(status)
                               TRAIL_ARG(fndfname) );
            cnfImprt( fndfname, GRP__SZNAM, ndfname );
            ccdTclAppC( cinterp, "tmplist", ndfname, status );
         }
         ccdTclDo( cinterp, "lappend NDFSETS $tmplist", status );
      }

/* Set the value of other Tcl variables to be passed into the script. */
      if ( ( cmstya = malloc( mstya_length + 1 ) ) == NULL ||
           ( cmstyb = malloc( mstyb_length + 1 ) ) == NULL ) {
         *status = SAI__ERROR;
         errRep( " ", "Memory allocation failed", status );
         return;
      }
      cnfImprt( mstya, mstya_length, cmstya );
      cnfImprt( mstyb, mstyb_length, cmstyb );
      ccdTclSetI( cinterp, "SKIP2", F77_ISTRUE(*skip2), status );
      ccdTclSetD( cinterp, "PERCLO", percnt[ 0 ], status );
      ccdTclSetD( cinterp, "PERCHI", percnt[ 1 ], status );
      ccdTclSetI( cinterp, "MAXPOS", 0, status );
      ccdTclSetD( cinterp, "ZOOM", *zoom, status );
      ccdTclSetI( cinterp, "MAXCANV", *maxcnv, status );
      ccdTclSetI( cinterp, "WINX", windim[ 0 ], status );
      ccdTclSetI( cinterp, "WINY", windim[ 1 ], status );
      ccdTclSetI( cinterp, "PREVX", prvdim[ 0 ], status );
      ccdTclSetI( cinterp, "PREVY", prvdim[ 1 ], status );
      ccdTclSetC( cinterp, "MARKSTYLEA", cmstya, status );
      ccdTclSetC( cinterp, "MARKSTYLEB", cmstyb, status );

/* Execute the Tcl script. */
      ccdTclRun( cinterp, "pairndf.tcl", status );

/* Retrieve the values generated by the script. */
      ccdTclGetI( cinterp, "llength $PAIRS", count, status );
      if ( *status == SAI__OK ) {
         const char *fmt1 = "lindex [ lindex $PAIRS %d ] %d";
         const char *fmt2 = "lindex [ lindex [ lindex $MATPTS %d ] %d ] %d";
         double *px1;
         double *py1;
         double *px2;
         double *py2;

         for ( i = 0; i < *count; i++ ) {
            sprintf( buffer, fmt1, i, 0 );
            ccdTclGetI( cinterp, buffer, nodes + 2 * i + 0, status );
            sprintf( buffer, fmt1, i, 1 );
            ccdTclGetI( cinterp, buffer, nodes + 2 * i + 1, status );
            sprintf( buffer, fmt1, i, 2 );
            ccdTclGetI( cinterp, buffer, nmat + i, status );
            sprintf( buffer, fmt1, i, 3 );
            ccdTclGetD( cinterp, buffer, xoff + i, status );
            sprintf( buffer, fmt1, i, 4 );
            ccdTclGetD( cinterp, buffer, yoff + i, status );

            px1 = (double *) ccdMall( "_DOUBLE", nmat[ i ], status );
            py1 = (double *) ccdMall( "_DOUBLE", nmat[ i ], status );
            px2 = (double *) ccdMall( "_DOUBLE", nmat[ i ], status );
            py2 = (double *) ccdMall( "_DOUBLE", nmat[ i ], status );
            ipx1[ i ] = cnfFptr( px1 );
            ipy1[ i ] = cnfFptr( py1 );
            ipx2[ i ] = cnfFptr( px2 );
            ipy2[ i ] = cnfFptr( py2 );
            for ( j = 0; j < nmat[ i ]; j++ ) {
               sprintf( buffer, fmt2, i, j, 0 );
               ccdTclGetD( cinterp, buffer, px1 + j, status );
               sprintf( buffer, fmt2, i, j, 1 );
               ccdTclGetD( cinterp, buffer, py1 + j, status );
               sprintf( buffer, fmt2, i, j, 2 );
               ccdTclGetD( cinterp, buffer, px2 + j, status );
               sprintf( buffer, fmt2, i, j, 3 );
               ccdTclGetD( cinterp, buffer, py2 + j, status );
            }
         }
         ccdTclGetD( cinterp, "set ZOOM", zoom, status );
         ccdTclGetI( cinterp, "set MAXCANV", maxcnv, status );
         ccdTclGetI( cinterp, "set WINX", windim + 0, status );
         ccdTclGetI( cinterp, "set WINY", windim + 1, status );
         ccdTclGetI( cinterp, "set PREVX", prvdim + 0, status );
         ccdTclGetI( cinterp, "set PREVY", prvdim + 1, status );
         cnfExprt( ccdTclGetC( cinterp, "set MARKSTYLEA", status ),
                   mstya, mstya_length );
         cnfExprt( ccdTclGetC( cinterp, "set MARKSTYLEB", status ),
                   mstyb, mstyb_length );
      }
      free( cmstya );
      free( cmstyb );

/* Delete the Tcl interpreter. */
      ccdTclStop( cinterp, status );
   }

/* $Id$ */
