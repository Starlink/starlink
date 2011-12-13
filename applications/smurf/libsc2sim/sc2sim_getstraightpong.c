/*
 *+
 *  Name:
 *     sc2sim_getstraightpong

 *  Purpose:
 *     Get coordinates of full straight PONG pattern

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getstraightpong ( double angle, double width,
 *                              double height, double spacing,
 *                              double accel[2], double vmax[2],
 *                              double samptime, int nmaps,
 *                              int *pongcount, double **posptr,
 *                              int *status )

 *  Arguments:
 *     angle = double (Given)
 *        Angle of pattern relative to the telescope axes in radians
 *        anticlockwise
 *     width = double (Given)
 *        Minimum width of PONG pattern (arcsec)
 *     height = double (Given)
 *        Minimum height of PONG pattern (arcsec)
 *     spacing = double (Given)
 *        Grid spacing in arcsec
 *     accel = double[2] (Given)
 *        Telescope accelerations (arcsec/sec)
 *     vmax = double[2] (Given)
 *        Telescope maximum velocities (arcsec)
 *     samptime = double (Given)
 *        Sample interval in sec
 *     nmaps = int (Given)
 *        Number of cycles of the Pong pattern
 *     pongcount = int* (Returned)
 *        Number of positions in pattern
 *     posptr = double** (Returned)
 *        list of positions
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     The PONG pattern creates a grid of at least the required size
 *     (as specified by the width and height).  The vertices are determined
 *     from the supplied spacing.  The PONG pattern is a rectangular
 *     shape in which the number of vertices along each side share no
 *     common factors.
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
 *     J.Balfour (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2005-07-07 (BDK):
 *        Original
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2006-10-03 (JB):
 *        Allow non-square PONG patterns, change to user-friendly
 *        parameters.
 *     2006-11-22 (JB):
 *        Added multiple map cycle capabilities.
 *     2006-12-08 (JB):
 *        smf_free mapptr.
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour

 *  Copyright:
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
 *     Council. University of British Columbia. All Rights Reserved.

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
 *     MA 02110-1301, USA

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

void sc2sim_getstraightpong
(
 double angle,        /* angle of pattern relative to telescope
                         axes in radians anticlockwise (given) */
 double width,        /* minimum width of PONG pattern in arcsec (given) */
 double height,       /* minimum height of PONG pattern in arcsec (given) */
 double spacing,      /* grid spacing in arcsec (given) */
 double accel[2],     /* telescope accelerations (arcsec) (given) */
 double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
 double samptime,     /* sample interval in sec (given) */
 int nmaps,           /* number of cycles of the pattern */
 int *pongcount,      /* number of positions in pattern (returned) */
 double **posptr,     /* list of positions (returned) */
 int *status          /* global status (given and returned) */
 )

{
  /* Local variables */
  double cend[2];          /* ending coordinates in arcsec */
  double cstart[2];        /* starting coordinates in arcsec */
  int curroff;             /* index of next free slot in position list */
  double grid[1024][2];    /* array of vertex coordinates */
  int j;                   /* loop counter */
  double *mapptr;          /* list of positions for one map */
  int mcount;              /* number of positions in one map */
  int numvertices;         /* number of vertices (including start & end,
                              which are the same) */
  double tx;               /* temporary X coordinate */
  double ty;               /* temporary Y coordinate */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  sc2sim_getpongends ( width, height, spacing, grid, &numvertices, status );

  /* Rotate the grid coordinates */
  for ( j=0; j< numvertices; j++ ) {
    tx = grid[j][0] * cos(angle) - grid[j][1] * sin(angle);
    ty = grid[j][0] * sin(angle) + grid[j][1] * cos(angle);
    grid[j][0] = tx;
    grid[j][1] = ty;
  }


  /* Get the size of the each scan segment */
  mcount = 0;

  for ( j=0; j<numvertices - 1; j++ ) {
    cstart[0] = grid[j][0];
    cstart[1] = grid[j][1];
    cend[0] = grid[j+1][0];
    cend[1] = grid[j+1][1];
    sc2sim_getscansegsize ( samptime, cstart, cend, accel, vmax, &curroff,
                            status );
    mcount += curroff;
  }

  /* Allocate memory for a single cycle of the pattern */
  mapptr = astCalloc( mcount*2, sizeof(*mapptr) );

  /* Get the positions for each scan segment */
  curroff = 0;

  for ( j=0; j<numvertices - 1; j++ ) {
    cstart[0] = grid[j][0];
    cstart[1] = grid[j][1];
    cend[0] = grid[j+1][0];
    cend[1] = grid[j+1][1];
    sc2sim_getscanseg ( samptime, cstart, cend, accel, vmax, mcount,
                        &curroff, mapptr, status );
  }

  /* Allocate memory for all n cycles of the pattern */
  *pongcount = mcount * nmaps;
  *posptr = astCalloc( (*pongcount) * 2, sizeof(**posptr) );

  /* Copy the required number of cycles into the
     list of positions */
  for ( j = 0; j < (*pongcount) * 2; j++ ) {
    (*posptr)[j] = mapptr[j % (mcount * 2)];
  }

  mapptr = astFree( mapptr );

}
