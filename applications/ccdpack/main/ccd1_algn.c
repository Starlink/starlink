#include "f77.h"
#include "sae_par.h"
#include "mers.h"
#include "tcl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ccdtcl.h"

#define BUFLENG 200

   F77_SUBROUTINE(ccd1_algn)( CHARACTER_ARRAY(ndfnms), INTEGER(nndf),
                              INTEGER(refpos), INTEGER(maxpos), 
                              DOUBLE_ARRAY(percnt), DOUBLE(zoom), 
                              INTEGER(maxcanv), INTEGER_ARRAY(windim), 
                              CHARACTER(mstyle),
                              INTEGER(npoint), DOUBLE(xpos), DOUBLE(ypos), 
                              INTEGER(index), INTEGER(status)
                              TRAIL(ndfnms) TRAIL(mstyle) ) {
   
/*
*+
*  Name:
*     CCD1_ALGN
*
*  Purpose:
*     Get position lists by user interaction for a set of NDFs.
*
*  Language:
*     ANSI C.
*
*  Invocation:
*     CALL CCD1_ALGN( NDFNMS, NNDF, REFPOS, MAXPOS, PERCNT, ZOOM, 
*                     MAXCNV, WINDIM, MSTYLE, NPOINT, XPOS, YPOS, 
*                     INDEX, STATUS )
*
*  Description:
*     This routine calls a Tcl script which displays a number of NDFs
*     and allows the user to select a matching set of positions on
*     each member of the set.  It returns the lists of points to 
*     the calling routine.
*     
*     If the user attempts to select more positions than MAXPOS, the
*     GUI part of the application will not allow it, and will force
*     an early exit.
*
*  Arguments:
*     NDFNMS = CHARACTER( NNDF ) * ( * ) (Given)
*        Names of the NDFs which are to be matched.
*     NNDF = INTEGER (Given)
*        The number of NDFs to be matched.
*     REFPOS = INTEGER (Given)
*        The index of the NDF in NDFNMS which is to be the reference image.
*        The first NDF in NDFNMS has the index 1.
*     MAXPOS = INTEGER (Given)
*        The maximum number of points to be marked on any of the NDFs.
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
*     NPOINT = INTEGER( NNDF ) (Returned)
*        The number of points contained in each of the output position
*        lists in the array POINTS.
*     XPOS = DOUBLE PRECISION( MAXPOS, NNDF ) (Returned)
*        The X coordinates of the marked positions for each NDF.
*     YPOS = DOUBLE PRECISION( MAXPOS, NNDF ) (Returned)
*        The Y coordinates of the marked positions for each NDF.
*     INDEX = INTEGER( MAXPOS, NNDF ) (Returned)
*        The index label of the marked positions for each NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*
*  Authors:
*     MBT: Mark Taylor (STARLINK)
*
*  History:
*     21-JUL-2000 (MBT):
*        Original version.
*-
*/

/* Arguments. */
      GENPTR_CHARACTER_ARRAY(ndfnms)
      GENPTR_INTEGER(nndf)
      GENPTR_INTEGER(refpos)
      GENPTR_INTEGER(maxpos)
      GENPTR_DOUBLE_ARRAY(percnt)
      GENPTR_DOUBLE(zoom)
      GENPTR_INTEGER(maxcanv)
      GENPTR_INTEGER_ARRAY(windim)
      GENPTR_CHARACTER(mstyle)
      GENPTR_INTEGER_ARRAY(npoint)
      GENPTR_DOUBLE_ARRAY(xpos)
      GENPTR_DOUBLE_ARRAY(ypos)
      GENPTR_INTEGER_ARRAY(index)
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

/* Check the inherited status. */
      if ( *status != SAI__OK ) return;

/* Initialise the Tcl interpreter. */
      cinterp = ccdTclStart( status );

/* Set the value of Tcl variables to be passed into the script. */
      if ( ( cmstyle = malloc( mstyle_length + 1 ) ) == NULL ) {
         *status = SAI__ERROR;
         errRep( " ", "Memory allocation failed", status );
         return;
      }
      for ( i = 0; i < *nndf; i++ ) {
         cnfImpn( ndfnms + i * ndfnms_length, ndfnms_length, BUFLENG - 1, 
                  buffer );
         ccdTclAppC( cinterp, "NDFNAMES", buffer, status );
      }
      cnfImprt( mstyle, mstyle_length, cmstyle );
      ccdTclSetI( cinterp, "REFPOS", *refpos - 1, status );
      ccdTclSetI( cinterp, "MAXPOS", *maxpos, status );
      ccdTclSetD( cinterp, "PERCLO", percnt[ 0 ], status );
      ccdTclSetD( cinterp, "PERCHI", percnt[ 1 ], status );
      ccdTclSetD( cinterp, "ZOOM", *zoom, status );
      ccdTclSetI( cinterp, "MAXCANV", *maxcanv, status );
      ccdTclSetI( cinterp, "WINX", windim[ 0 ], status );
      ccdTclSetI( cinterp, "WINY", windim[ 1 ], status );
      ccdTclSetC( cinterp, "MARKSTYLE", cmstyle, status );

/* Execute the Tcl script. */
      ccdTclRun( cinterp, "ccdalign.tcl", status );

/* Retrieve the variables set by the script. */
      if ( *status == SAI__OK ) {
         fmt = "lindex [ lindex [ lindex $POINTS %d ] %d ] %d";
         for ( i = 0; i < *nndf; i++ ) {
            sprintf( buffer, "lindex $NPOINT %d", i );
            ccdTclGetI( cinterp, buffer, npoint + i, status );
            for ( j = 0; j < npoint[ i ] && j < *maxpos; j++ ) {
               ix = i * *maxpos + j;
               sprintf( buffer, fmt, i, j, 0 );
               ccdTclGetI( cinterp, buffer, index + ix, status );
               sprintf( buffer, fmt, i, j, 1 );
               ccdTclGetD( cinterp, buffer, xpos + ix, status );
               sprintf( buffer, fmt, i, j, 2 );
               ccdTclGetD( cinterp, buffer, ypos + ix, status );
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
