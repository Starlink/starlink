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
*     sc2sim_getpong ( double angle, double height, double width, 
*                      double spacing, double vmax[2], 
*                      double samptime, int *pongcount, 
*                      double **posptr, int *status )

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
*        Telescope accelerations (arcsec)
*     vmax = double[2] (Given)
*        Telescope maximum velocities (arcsec)
*     samptime = double (Given)
*        Sample interval in sec
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
*     The velocity of the scan is assumed to be constant and is the 
*     average of the specified x and y velocities.
*
*     The functions x(t) and y(t) are triangle functions with five terms
*     each.  This creates a scan with reasonable 'straight' sweeps across
*     the central region of the box, while allowing the corners to 
*     be "rounded" to avoid rapid changes in acceleration.
*     

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
*        Use triangle wave functions to create box scan of required
*        height, width, and angle.

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
double width,        /* minimum width of PONG pattern in arcsec (given) */
double height,       /* minimum height of PONG pattern in arcsec (given) */
double spacing,      /* grid spacing in arcsec (given) */
double vmax[2],      /* telescope maximum velocities (arcsec/sec) (given) */
double samptime,     /* sample interval in sec (given) */
int *pongcount,      /* number of positions in pattern (returned) */
double **posptr,     /* list of positions (returned) */
int *status          /* global status (given and returned) */
)

{
   /* Local variables */
   double amp_x;            /* amplitude of x(t) (arcsec) */
   double amp_y;            /* amplitude of y(t) (arcsec) */
   int i;                   /* loop counter */
   int *least;              /* the lower # of vertices */
   int *most;               /* the higher # of vertices */
   double period;           /* total time of scan (seconds) */
   double peri_x;           /* period of x(t) (seconds) */
   double peri_y;           /* period of y(t) (seconds) */
   double t_count;          /* time counter (seconds) */
   double tx;               /* temporary x value (arcsec) */
   double ty;               /* temporary y value (arcsec) */
   double vert_spacing;     /* spacing along the vertices (arcsec) */
   double vavg;             /* lower of max velocities (arcsec/sec) */
   int x_numvert;           /* number of vertices along x axis */
   int y_numvert;           /* number of vertices along y axis */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   /* Find out how far apart the vertices are along the axes */
   vert_spacing = spacing / sqrt ( 2.0 );

   /* Determine how many vertices (minimum) there must be along
      each axis to cover the required area */
   x_numvert = ceil ( width / vert_spacing );
   y_numvert = ceil ( height / vert_spacing );

   /* Determine which is lower and check to make sure that one is
      even while the other is odd */
   if ( x_numvert >= y_numvert ) {
      most = &x_numvert;
      least = &y_numvert;
   } else {
      most = &y_numvert;
      least = &x_numvert;
   }

   /* If both are odd or both are even, increment the lesser of
      the two, and update which is least */
   if ( ( x_numvert % 2 ) == (y_numvert % 2 ) ) {
      *least += 1;
      if ( x_numvert >= y_numvert ) {
         most = &x_numvert;
         least = &y_numvert;
      } else {
         most = &y_numvert;
         least = &x_numvert;
      }
   }

   /* Check for common factors between the two, and adjust as 
      necessary until x_numvert and y_numvert do not share any
      common factors */
   for ( i = 3; i <= *least; i++ ) {

      if ( ( ( (*least) % i ) == 0 ) && ( ( (*most) % i ) == 0 ) ) {
         /* Found a common factor, increment most and start over */
	 (*most)++;
         i = 3;
      }
      
      /* Increment i again to skip even numbers */
      i++;

   }  

   /* KLUDGE : Calculate the approximate periods (assuming a PONG scan with
      no "rounding" at the corners.  To do this, average the velocities
      in order to determine the period in each direction, and the 
      total time required for the scan. */
   vavg = ( vmax[0] + vmax[1] ) / 2.0;
   
   peri_x = x_numvert * vert_spacing * 2.0 / vavg;
   peri_y = y_numvert * vert_spacing * 2.0 / vavg;
   period = x_numvert * y_numvert * vert_spacing * 2.0 / vavg;

   /* Determine the number of positions required for the pattern
      and allocate memory */
   (*pongcount) = period / samptime;
   *posptr = smf_malloc ( (*pongcount)*2, sizeof(**posptr), 1, status );

   /* Calculate the amplitudes of x(t) and y(t) */
   amp_x = x_numvert * vert_spacing / 2.0;
   amp_y = y_numvert * vert_spacing / 2.0;

   /* Get the positions using a triangle wave approximation for both
      x(t) and y(t), applying a Fourier expansion with the first three
      terms (1, 3, 5) so as to "round off" the corners */
   t_count = 0;

   for ( i = 0; ( i < (*pongcount) * 2 ); i++ ) {
     
      tx = amp_x * ( sin ( 2.0 * M_PI * t_count / peri_x ) -
                   ( 1.0/9.0 * sin ( 6.0 * M_PI * t_count / peri_x ) ) + 
		   ( 1.0/25.0 * sin ( 10.0 * M_PI * t_count / peri_x ) ) -
		   ( 1.0/49.0 * sin ( 14.0 * M_PI * t_count / peri_x ) ) +
		   ( 1.0/81.0 * sin ( 18.0 * M_PI * t_count / peri_x ) ) );  

      ty = amp_y * ( sin ( 2.0 * M_PI * t_count / peri_y ) -
                   ( 1.0/9.0 * sin ( 6.0 * M_PI * t_count / peri_y ) ) + 
		   ( 1.0/25.0 * sin ( 10.0 * M_PI * t_count / peri_y ) ) -
		   ( 1.0/49.0 * sin ( 14.0 * M_PI * t_count / peri_y ) ) + 
		   ( 1.0/81.0 * sin ( 18.0 * M_PI * t_count / peri_y ) ) );  

      /* Apply the rotation angle and record the coordinates */
      (*posptr)[i] = tx * cos ( angle ) - ty * sin ( angle );
      i++;
      (*posptr)[i] = tx * sin ( angle ) + ty * cos ( angle );
      t_count += samptime;

   }

}
