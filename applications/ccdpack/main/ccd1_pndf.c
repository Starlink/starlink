#include "f77.h"
#include "sae_par.h"
#include "tcl.h"
#include <stdio.h>
#include "ccdtcl.h"
#include "grp_par.h"

   void *ccdMall( char *type, int size, int *status );

   F77_SUBROUTINE(ccd1_pndf)( INTEGER(ndfgid), DOUBLE_ARRAY(percnt), 
                              INTEGER(count), DOUBLE(zoom), 
                              INTEGER(maxcanv), INTEGER_ARRAY(windim),
                              INTEGER_ARRAY(nodes), INTEGER_ARRAY(nmat),
                              DOUBLE_ARRAY(xoff), DOUBLE_ARRAY(yoff),
                              POINTER_ARRAY(ipx1), POINTER_ARRAY(ipy1),
                              POINTER_ARRAY(ipx2), POINTER_ARRAY(ipy2),
                              INTEGER(status) ) {
/*
*+
*  Name:
*     CCD1_PNDF

*  Purpose:
*     Harness Tcl code to get offsets between pairs of NDFs.

*  Language:
*     ANSI C.

*  Invocation:
*     CALL CCD1_PNDF( NDFGID, PERCNT, COUNT, NODES, NMAT, XOFF, YOFF, 
*                     STATUS )

*  Description:
*     This routine calls a Tcl script which presents the user with a
*     graphical method of specifying offsets between pairs of members
*     of a group of NDFs, and returns the offsets thus obtained.

*  Arguments:
*     NDFGID = INTEGER (Given)
*        GRP identifier of the group of NDFs which is to be presented 
*        to the user.
*     PERCNT( 2 ) = DOUBLE PRECISION (Given)
*        Lower and higher percentiles to use in displaying the images.
*        They should satisfy 0 <= PERCNT( 0 ) <= PERCNT( 1 ) <= 100.
*     ZOOM = DOUBLE PRECISION (Given and Returned)
*        The zoom factor for the initial display.  May be limited by MAXCANV.
*     MAXCANV = INTEGER (Given and Returned)
*        The maximum X or Y dimension of the intial NDF display.
*     WINDIM( 2 ) = INTEGER (Given and Returned)
*        Dimensions of the window used for display.
*     COUNT = INTEGER (Returned)
*        The number of pairings which have been made by the user.
*     NODES( 2, * ) = INTEGER (Returned)
*        For each pairing made, this array contains the indices of the 
*        NDFs which form the pair.  The index of the first NDF in the
*        group pointed to by NDFGID is 1, etc.  The first COUNT pairs
*        of elements in this array are filled on return.
*     NMAT( * ) = INTEGER (Returned)
*        For each pairing made, the number of objects which were matched
*        up together by the centroiding routine in order to find an
*        accurate offset.  This can be used as a weighting indicating
*        the accuracy of the returned offsets.  The first COUNT elements
*        of this array are filled on return.
*     XOFF( * ) = DOUBLE PRECISION (Returned)
*        For each pairing I made this array contains the accurate X
*        offset between the NDFs with indices NODES( 1, I ) and 
*        NODES( 2, I ).  The first COUNT elements in this array are
*        filled on return.
*     YOFF( * ) = DOUBLE PRECISION (Returned)
*        For each pairing I made this array contains the accurate Y
*        offset between the NDFs with indices NODES( 1, I ) and 
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

*  Authors:
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-2000 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Arguments. */
      GENPTR_INTEGER(ndfgid)
      GENPTR_INTEGER(count)
      GENPTR_INTEGER_ARRAY(nodes)
      GENPTR_INTEGER_ARRAY(nmat)
      GENPTR_DOUBLE_ARRAY(percnt)
      GENPTR_DOUBLE(zoom)
      GENPTR_INTEGER(maxcanv)
      GENPTR_INTEGER_ARRAY(windim)
      GENPTR_DOUBLE_ARRAY(xoff)
      GENPTR_DOUBLE_ARRAY(yoff)
      GENPTR_POINTER_ARRAY(ipx1)
      GENPTR_POINTER_ARRAY(ipy1)
      GENPTR_POINTER_ARRAY(ipx2)
      GENPTR_POINTER_ARRAY(ipy2)
      GENPTR_INTEGER(status)

/* Local variables. */
      int nndf;
      int i;
      int j;
      const int one = 1;
      ccdTcl_Interp *cinterp;
      DECLARE_CHARACTER( fndfname, GRP__SZNAM );
      char ndfname[ GRP__SZNAM + 1 ];
      char buffer[ 1024 ];

/* Test the global status. */
      if ( *status != SAI__OK ) return;

/* Initialise the Tcl interpreter. */
      cinterp = ccdTclStart( status );

/* Get the number of NDFs in the group. */
      F77_CALL(grp_grpsz)( INTEGER_ARG(ndfgid), INTEGER_ARG(&nndf),
                           INTEGER_ARG(status) );

/* Construct a list of NDF names available to the Tcl interpreter as 
   the value of the NDFNAMES variable. */
      for ( i = 1; i <= nndf; i++ ) {
         F77_CALL(grp_get)( INTEGER_ARG(ndfgid), INTEGER_ARG(&i), 
                            INTEGER_ARG(&one), CHARACTER_ARG(fndfname),
                            INTEGER_ARG(status)
                            TRAIL_ARG(fndfname) );
         cnfImprt( fndfname, GRP__SZNAM, ndfname );
         ccdTclAppC( cinterp, "NDFNAMES", ndfname, status );
      }

/* Set the value of other Tcl variables to be passed into the script. */
      ccdTclSetD( cinterp, "PERCLO", percnt[ 0 ], status );
      ccdTclSetD( cinterp, "PERCHI", percnt[ 1 ], status );
      ccdTclSetI( cinterp, "MAXPOS", 0, status );
      ccdTclSetD( cinterp, "ZOOM", *zoom, status );
      ccdTclSetI( cinterp, "MAXCANV", *maxcanv, status );
      ccdTclSetI( cinterp, "WINX", windim[ 0 ], status );
      ccdTclSetI( cinterp, "WINY", windim[ 1 ], status );

/* Execute the Tcl script. */
      ccdTclRun( cinterp, "pairndf.tcl", status );

/* Retrieve the values generated by the script. */
      ccdTclGetI( cinterp, "llength $PAIRS", count, status );
      if ( *status == SAI__OK ) {
         char *fmt1 = "lindex [ lindex $PAIRS %d ] %d";
         char *fmt2 = "lindex [ lindex [ lindex $MATPTS %d ] %d ] %d";
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
      }
      ccdTclGetD( cinterp, "set ZOOM", zoom, status );
      ccdTclGetI( cinterp, "set MAXCANV", maxcanv, status );
      ccdTclGetI( cinterp, "set WINX", windim + 0, status );
      ccdTclGetI( cinterp, "set WINY", windim + 1, status );

/* Delete the Tcl interpreter. */
      ccdTclStop( cinterp, status );
   }



#include "dat_par.h"

   void *ccdMall( char *type, int size, int *status ) {
/*
*+
*  Name:
*     ccdMall
*
*  Purpose:
*     C wrapper for fortran CCD1_MALL routine.
*
*  Arguments:
*     type = char *
*        HDS type of memory to allocate, as a null-terminated string.
*     size = int
*        Number of elements of type type to allocate.
*     status = int
*        The global status.
*
*  Return Value:
*     A pointer to a block of memory which will hold size elements of
*     type type.  This pointer has been registered with the CCDPACK
*     memory allocation machinery (and a fortiori the CNF memory 
*     allocation machinery) and so must be deallocated using CCD1_FREE.
*     The pointer returned is a C pointer, and thus suitable for direct
*     use by C code.  If it is to be used by Fortran code it must
*     be processed with the function cnfFptr.
*-
*/
      DECLARE_CHARACTER( ftype, DAT__SZTYP );
      F77_POINTER_TYPE ptr;

      if ( *status != SAI__OK ) return (void *) NULL;

      cnfExprt( type, ftype, DAT__SZTYP );
      F77_CALL(ccd1_mall)( INTEGER_ARG(&size), CHARACTER_ARG(ftype),
                           POINTER_ARG(&ptr), INTEGER_ARG(status)
                           TRAIL_ARG(ftype) );
      return cnfCptr( ptr );
   }
                           

/* $Id$ */
