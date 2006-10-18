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
*     {enter_new_authors_here}

*  History :
*     2006-09-29 (JB):
*        Original

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

/* Starlink includes */
#include "sae_par.h"

/* SMURF includes */
#include "smurf_par.h"

/* SC2SIM includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_getpongends"

/* Enumerated type for border */
typedef enum {top, bot, left, right} bordertype;

void sc2sim_getpongends
( 
double width,          /* minimum width of scan (arcsec) */
double height,         /* minimum height of scan (arcsec) */
double spacing,        /* spacing of grid pattern (arcsec) */
double grid[][2],     /* array of vertex coordinates */
int *numvertices,      /* total number of vertices */
int *status            /* pointer to global status */
)

{
   /* Local variables */

   int i;                   /* loop counter */
   double vert_spacing;     /* spacing along the vertices (arcsec) */
   int x_count;             /* counter for x axis */
   int x_dir;               /* flag for x direction */
   int x_numvert;           /* number of vertices along x axis */
   int x_temp;              /* temporary x value for "extra" bounces */
   int y_count;             /* counter for y axis */
   int y_dir;               /* flag for y direction */
   int y_numvert;           /* number of vertices along y axis */
   int y_temp;              /* temporary y value for "extra" bounces */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Calculate how many vertices there must be in each direction,
      and how far apart they are */
   sc2sim_getpongvert ( width, height, spacing, &vert_spacing,
                        &x_numvert, &y_numvert, status );

   /* Calculate the total number of vertices (count both the start
      and end points, which should be the same) */
   *numvertices = ( 2 * x_numvert ) + ( 2 * y_numvert ) + 1;

   /* The algorithm described below is dependent on which of the 
      two dimensions is larger.  In the case of scan which is
      wider than it is tall, the pattern determines the vertices
      along the top and bottom of the "box" and adds extra 
      vertices as needed for bounces of the sides of the box.
      Similarly if the box is taller than it is wide, the 
      vertices along the sides are added with extra vertices
      added when the top or bottom is reached instead. */

   if ( y_numvert < x_numvert ) {

     /* Box is wider than it is tall.  Start at the lower left
         hand corner, on the bottom edge, moving up and to the 
         right */
      x_count = 1 - x_numvert;
      y_count = 0 - y_numvert;
      x_dir = 1;
      y_dir = 1;

      grid[0][0] = x_count * vert_spacing / 2;
      grid[0][1] = y_count * vert_spacing / 2; 

      for ( i = 1; i < *numvertices; i++ ) {
       
         /* Assume we hit the top/bottom of the "box" */
         x_count += x_dir * 2 * y_numvert;
         y_count = y_dir * y_numvert;
     
         /* Check to see if we hit a side of the "box" instead */
         if ( x_dir * x_count > x_numvert ) {

            /* We hit a side wall, so we need to calculate an 
               "extra" vertex for the bounce before finding
	       the next top/bottom vertex */
            x_temp = x_dir * x_numvert;
            y_temp = y_count - ( y_dir * x_dir * ( x_count - x_temp ) );

            /* Record these vertices */
            grid[i][0] = x_temp * vert_spacing / 2;
            grid[i][1] = y_temp * vert_spacing / 2;

            i++;

            /* Get the next "top" or "bottom" vertex and change
               x direction */
            x_count = 2 * x_temp - x_count;

            x_dir *= -1; 

         }

         /* Record these vertices */
         grid[i][0] = x_count * vert_spacing / 2;
         grid[i][1] = y_count * vert_spacing / 2;

         /* Change y direction */
         y_dir *= -1;
   
      }

   } else {

      /* Box is taller than it is wide.  Start at the lower left
         hand corner, on the left edge, moving up and to the 
         right */
      x_count = 0 - x_numvert;
      y_count = 1 - y_numvert;
      x_dir = 1;
      y_dir = 1;

      grid[0][0] = x_count * vert_spacing / 2;
      grid[0][1] = y_count * vert_spacing / 2; 

      for ( i = 1; i < *numvertices; i++ ) {

         /* Assume we hit the side of the "box" */
	 x_count = x_dir * x_numvert;
         y_count += y_dir * 2 * x_numvert;

         /* Check to see if we hit the top/bottom of the "box" instead */
         if ( y_dir * y_count > y_numvert ) {

            /* We hit the top/bottom, so we need to calculate an 
               "extra" vertex for the bounce before finding
	       the next side vertex */
	    y_temp = y_dir * y_numvert;
	    x_temp = x_count - ( x_dir * y_dir * ( y_count - y_temp ) );

            /* Record these vertices */
            grid[i][0] = x_temp * vert_spacing / 2;
            grid[i][1] = y_temp * vert_spacing / 2;

            i++;

            /* Get the next "side" vertex and change
               y direction */
            y_count = 2 * y_temp - y_count;

            y_dir *= -1; 

         }

         /* Record these vertices */
         grid[i][0] = x_count * vert_spacing / 2;
         grid[i][1] = y_count * vert_spacing / 2;

         /* Change x direction */
         x_dir *= -1;
   
      }

   }

}
