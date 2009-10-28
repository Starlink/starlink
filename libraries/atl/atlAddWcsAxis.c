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
*     This function adds one or more new axes to both the base (i.e. GRID)
*     Frame and the current (i.e. WCS) Frame in the supplied WCS FrameSet.
*     If the FrameSet contains any other Frames, their dimensionality is
*     left unchanged.

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
   AstCmpMap *new_map;      /* Mapping from new GRID Frame to new WCS Frame */
   AstFrame *grid_frm;      /* Pointer to original base (GRID) Frame */
   AstFrame *new_grid_frm;  /* Pointer to new GRID Frame */
   AstCmpFrame *new_wcs_frm;/* Pointer to new WCS Frame */
   AstMapping *pmap;        /* Mapping from original to new GRID Frame */
   const char *dom;         /* Pointer to Domain attribute value */
   int axes[ ATL__MXDIM ];  /* Indicies of grid axes to pick */
   int i;                   /* Axis index */
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
   of axes. This also returns a pointer to a new PermMap that maps the old
   GRID Frame to the new one. */
   ngrid_new = ngrid_old + ngrid_add;
   for( i = 0; i < ngrid_old; i++ ) axes[ i ] = i + 1;
   for( ; i < ngrid_new; i++ ) axes[ i ] = 0;
   new_grid_frm = astPickAxes( grid_frm, ngrid_new, axes, &pmap );

/* Set up the attributes for the new axes. These values are copied from
   ndf1_inifr.f  */
   for( i = ngrid_old + 1; i <= ngrid_new; i++ ) {
      astSet( new_grid_frm, "Format(%d)=%%3.1f", i );
      astSet( new_grid_frm, "Label(%d)=Data grid index %d", i, i );
      astSet( new_grid_frm, "Symbol(%d)=g%d", i, i );
      astSet( new_grid_frm, "Unit(%d)=pixel", i );
   }

/* Create a CmpFrame containing the original current Frame, and the
   supplied Frame holding the additional WCS axes. */
   new_wcs_frm = astCmpFrame( astGetFrame( wcs, AST__CURRENT ),
                              frm, " " );

/* Create a CmpMap describing the transformation from the new extended
   GRID Frame to the new extended. This is made up of the Mapping from
   the original GRID Frame to the original WCS Frame, in parallel with
   the supplied Mapping. */
   new_map = astCmpMap( astGetMapping( wcs, AST__BASE, AST__CURRENT ),
                        map, 0, " " );

/* Remove the original current Frame from the FrameSet. */
   astRemoveFrame( wcs, AST__CURRENT );

/* Add the new GRID Frame into the WCS FrameSet, using the PermMap
   returned by astPickAxes to connect it to the original GRID Frame. The
   new Frame is amde the current Frame. */
   astAddFrame( wcs, AST__BASE, pmap, new_grid_frm );

/* Remove the original base Frame and make the new GRID Frame (currently
   the current Frame) the new base Frame */
   astRemoveFrame( wcs, AST__BASE );
   astSetI( wcs, "Base", astGetI( wcs, "Current" ) );

/* Add the new WCS Frame into the WCS FrameSet, using the CmpMap found
   above to connect it to the new GRID Frame. The new Frame is made the 
   current Frame. */
   astAddFrame( wcs, AST__BASE, new_map, new_wcs_frm );

/* End the AST Object context. This annulls all AST objects created since
   the matching call to astBegin. */
   astEnd;

/* Revert to using the old AST status pointer. */
   astWatch( old_status );
}

