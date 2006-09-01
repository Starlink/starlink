/*
*+
*  Name:
*     sc2sim_getpongends.c

*  Purpose:
*     Get coordinates of PONG vertices

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getpongends ( int gridcount, double spacing, double grid[][2],
*                          int *status )

*  Arguments:
*     gridcount = int (Given)
*        Number of grid lines (odd)
*     spacing = double (Given)
*        Grid spacing in arcsec
*     grid = double[][2] (Returned)
*        Coordinates of pong vertices
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     The grid count is an odd number. eg 7 implies the generating grid is
*     7x7. The total number of vertices is 2*gridcount-1. The pattern
*     generated is as follows (starting middle right, ending bottom centre):
*
*                +--+--0->0--+--+--+
*                |  |  |  |  |  |  |
*                +--0->+--+--0--+--+
*                |  |  |  |  |  |  |
*                0->+--+--+--+--0--+
*                |  |  |  |  |  |  |
*                0--+--+--+--+--+<-0
*                |  |  |  |  |  |  |
*                +--0--+--+--+<-0--+
*                |  |  |  |  |  |  |
*                +--+--0--+<-0--+--+
*                |  |  |  |  |  |  |
*                +--+--+--0--+--+--+
*
*    The grid coordinates generated have (0,0) at the pattern centre.

*  Authors:
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History :
*     2005-07-05 (BDK):
*        Original
*     2005-07-07 (BDK):
*        Change name to getpongends
*     2006-07-20 (JB):
*        Split from dsim.c

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

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
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* SC2SIM includes */
#include "sc2sim.h"

void sc2sim_getpongends
( 
int gridcount,           /* number of grid lines (odd) (given) */
double spacing,          /* grid spacing in arcsec (given) */
double grid[][2],        /* coordinates of pong vertices (returned) */
int *status              /* global status (given and returned) */
)

{
   /* Local variables */
   int g;               /* integer grid distance */
   double gridpos;      /* scaled grid position */
   int j;               /* loop counter */
   int k;               /* alternating sign */ 

   /* Check status */
   if ( !StatusOkP(status) ) return;

   k = -1;

   for ( j=0; j<gridcount-1; j++ ) {
      g = k * ( ( j + 1 ) / 2 );
      gridpos = spacing * (double)g ;
      grid[2*j][1] =  gridpos;
      grid[2*j+1][1] = gridpos;
      grid[(gridcount-1)*2-(2*j+1)][0] = -gridpos;
      grid[(gridcount-1)*2-2*j][0] = -gridpos;
      k *= -1;
   }

   grid[gridcount*2-2][1] = -grid[gridcount*2-3][1];
   grid[0][0] = -grid[1][0];

}
