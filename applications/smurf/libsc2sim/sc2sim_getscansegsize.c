/*
 *+
 *  Name:
 *     sc2sim_getscansegsize.c

 *  Purpose:
 *     Return size of scan segment

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getscansegsize ( double samptime, double cstart[2], double cend[2],
 *                             double accel[2], double vmax[2], int *size,
 int *status )

 *  Arguments:
 *     samptime = double (Given)
 *        Sample time in sec
 *     cstart = double[2] (Given)
 *        Starting coordinates in arcsec
 *     cend = double[2] (Given)
 *        Ending coordinates in arcsec
 *     accel = double[2] (Given)
 *        Telescope accelerations (arcsec)
 *     vmax = double[2] (Given)
 *        Telescope maximum velocities (arcsec)
 *     size = int* (Returned)
 *        Number of samples in pattern
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Calculate the number of positions belonging on a straight-line scan
 *     segment. The telescope is accelerated and decelerated at the ends of
 *     the segment, and travels at the maximum velocity (if reached) in the
 *     centre.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2005-07-07 (BDK):
 *        Original
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2006-10-12 (JB):
 *        Correct divide-by-zero error on zero accelerations
 *     2006-11-16 (JB):
 *        Round up size to correct for very small scans

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

void sc2sim_getscansegsize
(
 double samptime,     /* sample time in sec (given) */
 double cstart[2],    /* starting coordinates in arcsec (given) */
 double cend[2],      /* ending coordinates in arcsec (given) */
 double accel[2],     /* telescope accelerations (arcsec) (given) */
 double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
 int *size,           /* number of samples in pattern (returned) */
 int *status          /* global status (given and returned) */
 )

{
  /* Local variables */
  double c0;          /* distance in coordinate 0 */
  double c1;          /* distance in coordinate 1 */
  double dtotal;      /* total distance along path */
  double eps;         /* small number to trap angles */
  int jend;           /* count at end of scan */
  double midway;      /* midway position along scan */
  double theta;       /* angle of path relative to coordinate [0] */
  double raccel;      /* acceleration along path */
  double rmax;        /* distance along path when max velocity reached */
  double rmaxvel;     /* max velocity along path */
  double tmaxvel;     /* max velocity along path */
  double tmidway;     /* time at mid way */
  double ttotal;      /* total time along path */
  double vmidway;     /* velocity at mid way */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  eps = 0.001;

  /* Determine angle of motion */
  theta = atan2 ( ( cend[1] - cstart[1] ), ( cend[0] - cstart[0] ) );

  /* Check for motion parallel to one of the axes */
  if ( fabs ( theta ) < eps ) {
    raccel = accel[0];
    rmaxvel = vmax[0];
  } else if ( fabs ( fabs(theta)-AST__DPI ) < eps ) {
    raccel = accel[0];
    rmaxvel = vmax[0];
  } else if ( fabs ( theta - PIBY2 ) < eps ) {
    raccel = accel[1];
    rmaxvel = vmax[1];
  } else if ( fabs ( theta + PIBY2 ) < eps ) {
    raccel = accel[1];
    rmaxvel = vmax[1];
  } else {

    raccel = fabs ( accel[0] / cos(theta) );

    if ( raccel > fabs( accel[1] / sin(theta) ) )
      raccel = fabs ( accel[1] / sin(theta) );

    rmaxvel = fabs ( vmax[0] / cos(theta) );

    if ( rmaxvel > fabs ( vmax[1] / sin(theta) ) )
      rmaxvel = fabs ( vmax[1] / sin(theta) );

  }

  /* Determine whether the maximum velocity is reached before mid-way */
  c0 = cend[0] - cstart[0];
  c1 = cend[1] - cstart[1];

  dtotal = sqrt ( c0 * c0 + c1 * c1 );
  midway = 0.5 * dtotal;

  /* If acceleration is 0, simply calculate the size of the scan from
     the length of the path */

  if ( raccel == 0 ) {

    ttotal = dtotal / rmaxvel;

    if ( ttotal < samptime ) {
      *size = 1;
    } else {
      *size = ttotal / samptime;
    }

  } else {


    tmidway = sqrt ( fabs ( 2.0 * midway / raccel ) );
    vmidway = fabs(raccel) * tmidway;

    if ( vmidway > fabs(rmaxvel) ) {

      /* Need to accelerate, coast, then decelerate */
      tmaxvel = fabs ( rmaxvel / raccel );
      rmax  = 0.5 * raccel * tmaxvel * tmaxvel;
      tmidway = tmaxvel + fabs ( ( midway - rmax ) / rmaxvel );
      jend = (int) ( 2.0 * tmidway / samptime );

    } else {

      /* Accelerate all the way to the midway point
         then decelerate to the end */
      jend = (int) ( 2.0 * tmidway / samptime );

    }

    *size = jend + 1;

  }

}
