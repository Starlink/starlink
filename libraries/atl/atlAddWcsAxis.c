#include "atl.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

void atlAddWcsAxis( AstFrameSet *wcs, AstMapping *map, AstFrame *frm, 
                    int *status ){
/*
*+
*  Name:
*     atlAddWcsAxis

*  Purpose:
*     Add one or more axes to an NDFs WCS FrameSet.

*  Language:
*     C.

*  Invocation:
*     void atlAddWcsAxis(  AstFrameSet *wcs, AstMapping *map, AstFrame *frm, 
*                          int *status )

*  Description:
*     This function adds one or more new axes to all the Frames in an NDF 
*     WCS FrameSet. The base (i.e. GRID) Frame is expanded to include a number 
*     of extra axes equal to the Nin attribute of the supplied Mapping. All 
*     other Frames in the FrameSet are replaced by CmpFrames holding the 
*     original Frame and the supplied Frame. These new axes are connected to 
*     the new GRID axes using the supplied Mapping.

*  Arguments:
*     wcs
*        A pointer to a FrameSet that is to be used as the WCS FrameSet in
*        an NDF. This imposes the restriction that the base Frame must
*        have Domain GRID.
*     map
*        A pointer to a Mapping. The forward transformation should transform
*        the new GRID axes into the new WCS axes.
*     frm
*        A pointer to a Frame defining the new WCS axes.
*     status
*        Pointer to the global status variable.

*  Notes:
*     - The new GRID and WCS axes are appended to the end of the existing
*     axes, so the axis indices associated with the new axes will extend 
*     from "nold+1" to "nold+nnew", where "nold" is the number of axes in 
*     the original Frame, and "nnew" is the number of new axes.
*     - An error will be reported if the Nout attribute of "map" is
*     different to the Naxes attribute of "frm".

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry  (JAC, HAwaii)
*     {enter_new_authors_here}

*  History:
*     29-OCT-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*+
*/

/* Local Variables: */
   AstCmpMap *gmap;         /* Mapping from new GRID Frame to new base Frame */
   AstFrame *grid_frm;      /* Pointer to original base (GRID) Frame */
   AstFrame *new_grid_frm;  /* Pointer to new GRID Frame */
   const char *dom;         /* Pointer to Domain attribute value */
   int axes[ ATL__MXDIM ];  /* Indicies of grid axes to pick */
   int i;                   /* Axis index */
   int ibase;               /* Index of original base Frame */
   int icurr;               /* Index of original current Frame */
   int ngrid_add;           /* Number of additional grid axes */
   int ngrid_new;           /* New number of grid axes */
   int ngrid_old;           /* Original number of grid axes */
   int nwcs_add;            /* Number of WCS axes to add */
   int *old_status;         /* Pointer to status value used by AST on entry */

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Ensure AST uses the supplied status pointer. */
   old_status = astWatch( status );

/* Begin an AST Object context. This means we do not need to annul any
   AST pointers since they will be annulled automatically when the AST
   Object context ends. */
   astBegin;

/* Find the number of new WCS axes to add, and report an error if the
   Mapping has a different number of outputs. */
   nwcs_add = astGetI( frm, "Naxes" );
   if( astGetI( map, "Nout" ) != nwcs_add && *status == SAI__OK ) {
      msgSeti( "NAXES", nwcs_add );
      msgSeti( "NOUT", astGetI( map, "Nout" ) );
      *status = SAI__ERROR;
      errRep( "", "atlAddWcsAxis: The supplied Mapping has ^NOUT outputs "
              "but the supplied Frame has ^NAXES axes (programming error).", 
              status );
   }

/* Get a pointer to the original base Frame in the supplied WCS FrameSet. */
   grid_frm = astGetFrame( wcs, AST__BASE );

/* Report an error if the Domain is not GRID. */
   dom = astGetC( grid_frm, "Domain" );
   if( dom && strcmp( dom, "GRID" ) && *status == SAI__OK ) {
      msgSetc( "DOM", dom );
      *status = SAI__ERROR;
      errRep( "", "atlAddWcsAxis: The base Frame in the supplied FrameSet has "
              "Domain '^DOM'. It should be 'GRID' (programming error).", status );
   }

/* Find the original number of GRID axes, and the number of new GRID axes 
   to add. */
   ngrid_old = astGetI( grid_frm, "Naxes" );
   ngrid_add = astGetI( map, "Nin" );

/* Use astPickAxes to create a GRID Frame with the required additional number 
   of axes. */
   ngrid_new = ngrid_old + ngrid_add;
   for( i = 0; i < ngrid_old; i++ ) axes[ i ] = i + 1;
   for( ; i < ngrid_new; i++ ) axes[ i ] = 0;
   new_grid_frm = astPickAxes( grid_frm, ngrid_new, axes, NULL );

/* Set up the attributes for the new axes. These values are copied from
   ndf1_inifr.f  */
   for( i = ngrid_old + 1; i <= ngrid_new; i++ ) {
      astSet( new_grid_frm, "Format(%d)=%%3.1f", i );
      astSet( new_grid_frm, "Label(%d)=Data grid index %d", i, i );
      astSet( new_grid_frm, "Symbol(%d)=g%d", i, i );
      astSet( new_grid_frm, "Unit(%d)=pixel", i );
   }

/* Append the supplied Frame to every Frame in the FrameSet, including the base 
   (GRID) Frame. These new axes in each Frame are inter-connected using UnitMaps. */
   astAddFrame( wcs, AST__ALLFRAMES, NULL, frm );

/* Now construct a Mapping that transforms the base Frame in the FrameSet (modified 
   by the above call) into the expanded GRID Frame created above. This is a parallel 
   CmpMap that uses a UnitMap to transform the original grid axes, and the inverse of 
   the supplied Mapping to transform the newly added grid axes. */
   gmap = astCmpMap( astUnitMap( ngrid_old, " " ), map, 0, " " );
   astInvert( gmap );

/* Add the new GRID Frame into the FrameSet. It becomes the current Frame so note the 
   index of the original current Frame first. */
   icurr = astGetI( wcs, "Current" );
   astAddFrame( wcs, AST__BASE, gmap, new_grid_frm );

/* Note the original base Frame index, and make the new GRID Frame (just added) the 
   base Frame. */
   ibase = astGetI( wcs, "Base" );
   astSetI( wcs, "Base", astGetI( wcs, "Current" ) );

/* Re-instate the original current Frame (unless the original base and current Frames 
   were the same, in which case the original current Frame will be removed when the 
   original base Frame is removed below). */
   if( ibase != icurr ) astSetI( wcs, "Current", icurr );
 
/* Remove the original base Frame. */
   astRemoveFrame( wcs, ibase );

/* End the AST Object context. This annulls all AST objects created since
   the matching call to astBegin. */
   astEnd;

/* Revert to using the old AST status pointer. */
   astWatch( old_status );
}

