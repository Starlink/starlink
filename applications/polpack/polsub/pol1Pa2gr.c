#include "sae_par.h"
#include "prm_par.h"
#include "ast.h"
#include "mers.h"
#include "polsub.h"
#include "math.h"

void pol1Pa2gr( AstFrameSet *iwcs, int axis, int npos, const double *gx0,
                const double *gy0, double *angle, int *status ){
/*
*+
*  Name:
*     pol1Pa2gr

*  Purpose:
*     Find the orientation of a WCS axis within GRID coords.

*  Language:
*     Starlink Fortran 77

*  Synopsis:
*     void pol1Pa2gr( AstFrameSet *iwcs, int axis, int npos, const double *gx0,
*                     const double *gy0, double *angle, int *status )

*  Description:
*     The routine finds the orientation of a WCS axis within GRID coords,
*     at a given set of GRID positions.

*  Arguments:
*     iwcs
*        An AST identifier for a WCS FrameSet. Base Frame should be GRID.
*     axis
*        The index of the axis (0 or 1) defining the position angle to be
*        converted.
*     npos
*        The number of supplied positions at which to do the calculation.
*     gx0
*        An array with "npos" elements, holding the GRID X position at
*        which the axis angle is required.
*     gy0
*        An array with "npos" elements, holding the GRID Y position at
*        which the axis angle is required.
*     angle
*        An array with "npos" elements which is returned holding the
*        angle in radians from the GRID X axis to the axis of the current
*        Frame specified by "axis", measured positive in the same sense as
*        rotation from GRID X to GRID Y.
*     status (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-JUN-2015 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   const double *pgx0;
   const double *pgy0;
   double *pgy;
   double *gy;
   double *pa;
   double *gx;
   double *sx;
   double *ps;
   double *sy;
   double *pgx;
   double delta;
   double p2[ 2 ];
   double p1[ 2 ];
   int ipos;
   int nel;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Allocate work space. Ensure we have at least two elements in each
   array (as needed for calculating delta). */
   nel = ( npos > 1 ) ? npos : 2;
   gx = astMalloc( nel*sizeof( *gx ) );
   gy = astMalloc( nel*sizeof( *gy ) );
   sx = astMalloc( nel*sizeof( *sx ) );
   sy = astMalloc( nel*sizeof( *sy ) );

/* Find a suitable size increment that we can use to define the required
   direction. */
   delta = AST__BAD;
   ipos = 0;
   while( delta == AST__BAD && ipos < npos ) {

/* Transform the ipos'th supplied GRID position into SKY, together with a
   point half a pixel away along the diagonal. */
      gx[ 0 ] = gx0[ ipos ];
      gy[ 0 ] = gy0[ ipos ];
      gx[ 1 ] = gx[ 0 ] + 0.3536;
      gy[ 1 ] = gy[ 0 ] + 0.3536;
      astTran2( iwcs, 2, gx, gy, 1, sx, sy );

/* Find the distance between the two points, within the current Frame. */
      p1[ 0 ] = sx[ 0 ];
      p1[ 1 ] = sy[ 0 ];
      p2[ 0 ] = sx[ 1 ];
      p2[ 1 ] = sy[ 1 ];
      delta = astDistance( iwcs, p1, p2 );

      ipos++;
   }

/* Abort if no delta value can be found. */
   if( delta == AST__BAD ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep(" ", "The WCS transformations are too complex", status );
      }

/* Otherwise, find the required angles using the above delta value. */
   } else if( *status == SAI__OK ) {

/* Transform the supplied GRID positions into SKY. */
      astTran2( iwcs, npos, gx0, gy0, 1, sx, sy );

/* Increment each sky position by delta along the required axis. */
      if( axis == 0 ) {
         ps = sx;
      } else {
         ps = sy;
      }

      for( ipos = 0; ipos < npos; ipos++ ) {
         *(ps++) += delta;
      }

/* Transform the incremented sky positions back to GRID. */
      astTran2( iwcs, npos, sx, sy, 0, gx, gy );

/* For each position, get the angle from the GRID X axis to the required
   axis. */
      pgx0 = gx0;
      pgy0 = gy0;
      pgx = gx;
      pgy = gy;
      pa = angle;
      for( ipos = 0; ipos < npos; ipos++,pgx++,pgy++,pgx0++,pgy0++ ) {
         if( *pgx != AST__BAD && *pgy != AST__BAD ) {
            *(pa++) = atan2( *pgy - *pgy0, *pgx - *pgx0 );
         } else {
            *(pa++) = AST__BAD;
         }
      }
   }

/* Free work space. */
   gx = astFree( gx );
   gy = astFree( gy );
   sx = astFree( sx );
   sy = astFree( sy );

}
