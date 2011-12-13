/*
 *+
 *  Name:
 *     sc2sim_getpongends.c

 *  Purpose:
 *     Get coordinates of straight PONG vertices

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getpongends ( double width, double height, double spacing,
 *                          double grid[][2], int *numvertices,
 *                          int *status )

 *  Arguments:
 *     width = double (Given)
 *        Minimum width of PONG pattern (arcsec)
 *     height = double (Given)
 *        Minimum height of PONG pattern (arcsec)
 *     spacing = double (Given)
 *        Spacing of grid pattern (arcsec)
 *     grid = double[][2] (Returned)
 *        Array of vertex coordinates
 *     numvertices = int* (Returned)
 *        Total number of vertices
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine determines the vertices and their order for
 *     a straight PONG pattern.  The vertices along the x- and
 *     y- directions can share no common factors for the pattern
 *     to be complete and close.

 *  Authors:
 *     J.Balfour (UBC)
 *     E.Chapin (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-09-29 (JB):
 *        Original
 *     2006-10-18 (EC):
 *        Re-written to use more concise algorithm
 *     2006-10-19 (JB):
 *        Add status check after getpongvert
 *     2006-10-20 (AGG):
 *        Include smf.h
 *     2006-10-23 (EC):
 *        Fixed off-by-one memory allocation error
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"

/* SMURF includes */
#include "smurf_par.h"
#include "libsmf/smf.h"

/* SC2SIM includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_getpongends"

void sc2sim_getpongends
(
 double width,          /* minimum width of scan (arcsec) */
 double height,         /* minimum height of scan (arcsec) */
 double spacing,        /* spacing of grid pattern (arcsec) */
 double grid[][2],      /* array of vertex coordinates */
 int *numvertices,      /* total number of vertices */
 int *status            /* pointer to global status */
 )

{
  /* Local variables */

  int i;                   /* loop counter */
  double grid_space;       /* Spacing of grid points in tanplane coords. */
  int n_seg;               /* Number of straight linesegs in pattern */
  int steps;               /* total number of grid steps taken in pattern */
  double vert_spacing;     /* spacing along the vertices (arcsec) */
  double *xgrid=NULL;      /* LUT for x-tanplane coord. of grid points */
  int x_init;              /* Starting x-grid coordinate in pattern */
  int x_ngridseg;          /* Len. of box in x-dirn. measured in grid segs */
  int x_numvert;           /* number of vertices along x axis */
  int x_off;               /* current x-grid offset */
  int x_refl;              /* number of reflections in x-direction */
  double *ygrid=NULL;      /* LUT for y-tanplane coord. of grid points */
  int y_init;              /* Starting y-grid coordinate in pattern */
  int y_ngridseg;          /* Len. of box in y-dirn. measured in grid segs */
  int y_numvert;           /* number of vertices along y axis */
  int y_off;               /* current y-grid offset */
  int y_refl;              /* number of reflections in x-direction */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Calculate how many vertices (reflection points) there must be in each
     direction, and how far apart they are */

  sc2sim_getpongvert ( width, height, spacing, &vert_spacing,
                       &x_numvert, &y_numvert, status );

  if( *status == SAI__OK ) {

    /* The entire pattern is defined on a grid with points spaced half the
       distance between adjacent vertices as calculated above along the same
       side. Calculate spacing between these grid points and the length along
       each side in units of grid_space sized segments. */

    grid_space = vert_spacing / 2.;
    x_ngridseg = x_numvert*2;
    y_ngridseg = y_numvert*2;

    /* The total number of straight line segments in the pattern */

    n_seg = x_ngridseg + y_ngridseg;

    /* Calculate the total number of vertices (count both the start
       and end points, which should be the same) */

    *numvertices = n_seg+1;

    /* Create arrays of x & y coords. of grid points along each dimension
       centered over (0,0) - note that this is 1 greater than the number
       of grid segments along each side. */

    xgrid = astMalloc( (x_ngridseg+1)*sizeof(*xgrid) );
    ygrid = astMalloc( (y_ngridseg+1)*sizeof(*ygrid) );

  }

  if( *status == SAI__OK ) {

    for( i=0; i<x_ngridseg+1; i++ )
      xgrid[i] = ((double) i - x_ngridseg/2.)*grid_space;

    for( i=0; i<y_ngridseg+1; i++ )
      ygrid[i] = ((double) i - y_ngridseg/2.)*grid_space;

    /* Initialization */

    x_init = 0;     /* starting grid coordinates for the pattern */
    y_init = 1;

    x_off = x_init; /* current grid offsets */
    y_off = y_init;

    grid[0][0] = xgrid[x_init]; /* starting tplane offsets for the pattern */
    grid[0][1] = ygrid[y_init];

    steps = 0;   /* number of steps along the grid covered at start */

    /* Loop over line segments in pattern and calculate list of endpoints
       for each reflection in order */

    for( i=1; i<=n_seg; i++ ) {

      /* increment steps to the next boundary reflection (whichever comes
         first - along the sides or top/bottom of the rectangle) */

      if( (x_ngridseg - x_off) <= (y_ngridseg - y_off) ) {
        steps += x_ngridseg - x_off;   /* side reflection next */
      } else {
        steps += y_ngridseg - y_off;   /* top/bottom reflection next */
      }

      /* Number of steps divided by number of grid segments along each side
         tells us how many reflections we've been through. */

      x_refl = (steps+x_init) / x_ngridseg;
      y_refl = (steps+y_init) / y_ngridseg;

      /* The remainder is the grid offset */

      x_off = (steps+x_init) % x_ngridseg;
      y_off = (steps+y_init) % y_ngridseg;

      /* If the number of reflections is even, the offset is in the positive
         direction from the start of the lookup table. If it is odd, the
         offset is in the negative direction from the end of the lookup
         table. */

      if( !(x_refl % 2) ) {
        grid[i][0] = xgrid[x_off];             /* even reflections */
      } else {
        grid[i][0] = xgrid[x_ngridseg-x_off];  /* odd reflections */
      }

      if( !(y_refl % 2) ) {
        grid[i][1] = ygrid[y_off];             /* even reflections */
      } else {
        grid[i][1] = ygrid[y_ngridseg-y_off];  /* odd reflections */
      }

    }


    /* Clean up */

    xgrid = astFree( xgrid );
    ygrid = astFree( ygrid );

  }
}
