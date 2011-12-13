/*
 *+
 *  Name:
 *     sc2sim_getpongvert.c

 *  Purpose:
 *     Get the number and spacing of the vertices of the
 *     PONG pattern

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getpongvert ( double width, double height, double spacing,
 *                          double *vert_spacing, int *x_numvert,
 *                          int *y_numvert, int *status )

 *  Arguments:
 *     width = double (Given)
 *        Minimum width of PONG pattern (arcsec)
 *     height = double (Given)
 *        Minimum height of PONG pattern (arcsec)
 *     spacing = double (Given)
 *        Spacing of grid pattern (arcsec)
 *     vert_spacing = double* (Returned)
 *        Spacing between vertices on axes (arcsec)
 *     x_numvert = int* (Returned)
 *        Number of vertices in x direction
 *     y_numvert = int* (Returned)
 *        Numver of vertices in y direction
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Given a user-specified minimum width and height in
 *     arcsec, as well as the desired spacing between the
 *     paths across the scan, this routine determines the
 *     number and spacing of vertices in the x- and
 *     y-directions.  In order to complete and close the
 *     PONG pattern, there must no common factors between
 *     the numbers of vertices along adjacent edges.
 *
 *     This routine will return parameters which guarantee
 *     at least a box as large as specified, but possibly
 *     larger if needed to satisfy the criteria above.

 *  Authors:
 *     J.Balfour (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-10-17 (JB):
 *        Original
 *     2006-10-19 (JB):
 *        Add error check, fix off-by-one error in checking
 *        common factors

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

/* SC2SIM includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_getpongvert"

/* Enumerated type for border */
typedef enum {top, bot, left, right} bordertype;

void sc2sim_getpongvert
(
 double width,          /* minimum width of scan (arcsec) */
 double height,         /* minimum height of scan (arcsec) */
 double spacing,        /* spacing of grid pattern (arcsec) */
 double *vert_spacing,  /* spacing of vertices along axes (arcsec) */
 int *x_numvert,        /* number of vertices in x direction */
 int *y_numvert,         /* number of vertices in y direction */
 int *status            /* pointer to global status */
 )

{
  /* Local variables */

  int i;                   /* loop counter */
  int *least;              /* the lower # of vertices */
  int *most;               /* the higher # of vertices */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  if ( width == 0.0 || height == 0.0 || spacing == 0.0 ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid PONG input parameters: must have non-zero width/height/spacing",
           status);
    return;
  }

  /* Find out how far apart the vertices are along the axes */
  *vert_spacing = ( 2.0 * spacing ) / sqrt ( 2.0 );

  /* Determine how many vertices (minimum) there must be along
     each axis to cover the required area */
  *x_numvert = ceil ( width / *vert_spacing );
  *y_numvert = ceil ( height / *vert_spacing );

  /* Determine which is lower and check to make sure that one is
     even while the other is odd */
  if ( *x_numvert >= *y_numvert ) {
    most = x_numvert;
    least = y_numvert;
  } else {
    most = y_numvert;
    least = x_numvert;
  }

  /* If both are odd or both are even, increment the lesser of
     the two, and update which is least */
  if ( ( *x_numvert % 2 ) == (*y_numvert % 2 ) ) {
    *least += 1;
    if ( *x_numvert >= *y_numvert ) {
      most = x_numvert;
      least = y_numvert;
    } else {
      most = y_numvert;
      least = x_numvert;
    }
  }

  /* Check for common factors between the two, and adjust as
     necessary until x_numvert and y_numvert do not share any
     common factors */
  for ( i = 3; i <= *least; i++ ) {

    if ( ( ( *least % i ) == 0 ) && ( ( *most % i ) == 0 ) ) {
      /* Found a common factor, increment most and start over */
      (*most) += 2;
      i = 3;
    }

  }

}
