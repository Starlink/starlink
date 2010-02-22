#include "sae_par.h"
#include "ndf.h"
#include "ast.h"

void kpg1Asndf( int indf, int ndim, int *lbnd, int *ubnd, AstFrameSet **iwcs,
                int *status ){
/*
*  Name:
*     kpg1Asndf

*  Purpose:
*     Create a FrameSet containing NDF-special Frames with given bounds.

*  Language:
*     C.

*  Invocation:
*     void kpg1Asndf( int indf, int ndim, int *lbnd, int *ubnd, 
*                     AstFrameSet **iwcs, int *status )

*  Description:
*     This function creates a FrameSet containing the NDF-special Frames,
*     GRID, PIXEL, FRACTION and AXIS, appropriate to an NDF with the
*     supplied dimensionality and pixel index bounds. Optionally, AXIS
*     information can be propagated from a supplied NDF.

*  Arguments:
*     indf
*        An NDF from which to propagate AXIS information. May be NDF__NOID, 
*        in which case the AXIS Frame in the returned FrameSet will describe 
*        the default AXIS coordinate system (i.e. pixel coords).
*     ndim
*        The number of pixel axes in the modified FrameSet.
*     lbnd 
*        The lower pixel index bounds in the modified FrameSet.
*     ubnd
*        The upper pixel index bounds in the modified FrameSet.
*     iwcs
*        Address at which to return a pointer to a new FrameSet holding 
*        GRID, FRACTION, PIXEL and AXIS Frames describing the supplied
*        NDF bounds, plus AXIS information from the supplied NDF.
*     status 
*        The inherited status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-FEB-2010 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*/
      
/* Local Variables: */
   int place;         /* Place holder for temporary NDF */
   int indf2;         /* Identifier for temporary NDF */

/* Initialise */
   *iwcs = NULL;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Create a place-holder for a temporary NDF. */
   ndfTemp( &place, status ); 

/* If an input NDF was supplied, create a copy of it, propagating just
   the AXIS component, and then set the bounds of the copy to the supplied
   bounds. */
   if( indf != NDF__NOID ) {
      ndfScopy( indf, "AXIS,NOHISTORY,NOLABEL,NOTITLE,NOEXT(*)", &place, 
                &indf2, status ); 
      ndfSbnd( ndim, lbnd, ubnd, indf2, status ); 

/* If no input NDF was supplied, create a new NDF with the required
   bounds. */
   } else {
      ndfNew( "_REAL", ndim, lbnd, ubnd, &place, &indf2, status ); 
   }

/* Get the WCS FrameSet. */
   ndfGtwcs( indf2, iwcs, status );

/* Annul the temporary NDF. */
   ndfAnnul( &indf2, status );

}
