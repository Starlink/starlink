/*
*+
*  Name:
*     smf_tswcsOrder

*  Purpose:
*     Re-order the axes in a time series WCS FrameSet

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_tswcsOrder( AstFrameSet **tswcs, int isTordered, int *status )

*  Arguments:
*     tswcs = AstKeyMap* (Given and Returned)
*        Address of a pointer to the FrameSet to be modified. The pointer
*        may be modified on exit to refer to a new modified FrameSet.
*     isTordered = int (Given)
*        Should the returned FrameSet be time-ordered?
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     If "isTordered" is non-zero, then this function checks to see if
*     the third axis in the current Frame of the supplied FrameSet is a
*     time axis. If it is, it returns without action. If it is not, it
*     reorders the axes to ensure that the third WCS axis is time axis.
*
*     If "isTordered" is zero, then this function checks to see if
*     the first axis in the current Frame of the supplied FrameSet is a
*     time axis. If it is, it returns without action. If it is not, it
*     reorders the axes to ensure that the first WCS axis is time axis.

*  Authors:
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-JAN-2014 (DSB):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008-2014 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include <stdio.h>

/* Starlink includes*/
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes*/
#include "libsmf/smf.h"

void smf_tswcsOrder( AstFrameSet **tswcs, int isTordered, int *status ){

/* Local Variables*/
   AstFrameSet *newfs;
   const char *domain;
   int perm[ 3 ];
   int swap;

/* Check inherited status */
   if (*status != SAI__OK) return;

/* Check if the axes in the FrameSet are already in the right order. */
   domain = astGetC( *tswcs, "Domain(3)" );
   if( *status == SAI__OK ) {
      if( !strcmp( domain, "TIME" ) ) {
         swap = ( isTordered == 0 );
      } else {
         swap = ( isTordered != 0 );
      }

/* Swap axes if required. */
      if( swap ) {

/* Get the a list of the old WCS axis indices in their new order. */
         if( isTordered ) {
            perm[ 0 ] = 2;
            perm[ 1 ] = 3;
            perm[ 2 ] = 1;
         } else {
            perm[ 0 ] = 3;
            perm[ 1 ] = 1;
            perm[ 2 ] = 2;
         }

/* Permute the axes in the current Frame of the WCS FrmeSet. This also
   adjusts the mappings that connects the current Frame to the other
   Frames. */
         astPermAxes( *tswcs, perm );

/* We also need to permute the Mapping that connects the base Frame (i.e.
   grid coords) to the other Frames in the same way. Since astPermAxes
   operates on the current Frame of a FrameSet, we need to invert the
   FrameSet temporarily. */
         astInvert( *tswcs );
         astPermAxes( *tswcs, perm );
         astInvert( *tswcs );

/* Simplify the modified FrameSet. */
         newfs = astSimplify( *tswcs );
         (void) astAnnul( *tswcs );
         *tswcs = newfs;
      }
   }
}








