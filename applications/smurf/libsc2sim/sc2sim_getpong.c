/*
*+
*  Name:
*     sc2sim_getpong

*  Purpose:
*     Get coordinates of full PONG pattern

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getpong ( double angle, int gridcount, double spacing, 
*                      double accel[2], double vmax[2], double samptime,
*                      double grid[][2], int *pongcount, double **posptr,
*                      int *status )

*  Arguments:
*     angle = double (Given)
*        Angle of pattern relative to the telescope axes in radians
*        anticlockwise
*     gridcount = int (Given)
*        Number of gridlines (odd)
*     spacing = double (Given)
*        Grid spacing in arcsec
*     accel = double[2] (Given)
*        Telescope accelerations (arcsec)
*     vmax = double[2] (Given)
*        Telescope maximum velocities (arcsec)
*     samptime = double (Given)
*        Sample interval in sec
*     grid = double[][2] (Returned)
*        Pong grid coordinates
*     pongcount = int* (Returned)
*        Number of positions in pattern
*     posptr = double** (Returned)
*        list of positions
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
*     The grid coordinates generated have (0,0) at the pattern centre.
*
*     The pattern is rotated through the given angle to get coordinates
*     related to the axes of telescope motion. The telescope motion along
*     the pattern is then computed and the positions at samptime intervals
*     recorded. 
*
*     It is assumed that a sample instant coincides with the start of each
*     straight line section, which is a reasonable approximation as the
*     telescope has to be "stationary" there.
*
*     Effects of "jerk" - ie time for change in acceleration, or other
*     telescope inertia effects are ignored.

*  Authors:
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History :
*     2005-07-07 (BDK):
*        Original
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

/* Standard includes */
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "libsmf/smf.h" 

void sc2sim_getpong
(
double angle,        /* angle of pattern relative to telescope
                        axes in radians anticlockwise (given) */
int gridcount,       /* number of grid lines (odd) (given) */
double spacing,      /* grid spacing in arcsec (given) */
double accel[2],     /* telescope accelerations (arcsec) (given) */
double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
double samptime,     /* sample interval in sec (given) */
double grid[][2],    /* pong grid coordinates (returned) */
int *pongcount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
)

{
   /* Local variables */
   double cend[2];          /* ending coordinates in arcsec */
   double cstart[2];        /* starting coordinates in arcsec */
   int curroff;             /* index of next free slot in position list */
   int j;                   /* loop counter */
   double tx;               /* temporary X coordinate */
   double ty;               /* temporary Y coordinate */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   sc2sim_getpongends ( gridcount, spacing, grid, status );

   /* Rotate the grid coordinates */
   for ( j=0; j<gridcount*2-1; j++ ) {
      tx = grid[j][0] * cos(angle) - grid[j][1] * sin(angle);
      ty = grid[j][0] * sin(angle) + grid[j][1] * cos(angle);
      grid[j][0] = tx;
      grid[j][1] = ty;
   }

   (*pongcount) = 0;

   for ( j=0; j<gridcount*2-2; j++ ) {
      cstart[0] = grid[j][0];
      cstart[1] = grid[j][1];
      cend[0] = grid[j+1][0];
      cend[1] = grid[j+1][1];
      sc2sim_getscansegsize ( samptime, cstart, cend, accel, vmax, &curroff,
                              status );
      (*pongcount) += curroff;
   }

   *posptr = smf_malloc ( (*pongcount)*2, sizeof(**posptr), 1, status );

   curroff = 0;

   for ( j=0; j<gridcount*2-2; j++ ) {
      cstart[0] = grid[j][0];
      cstart[1] = grid[j][1];
      cend[0] = grid[j+1][0];
      cend[1] = grid[j+1][1];
      sc2sim_getscanseg ( samptime, cstart, cend, accel, vmax, *pongcount,
                          &curroff, *posptr, status );
   }

}
