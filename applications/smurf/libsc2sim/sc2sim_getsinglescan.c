/*
 *+
 *  Name:
 *     sc2sim_getsinglescan

 *  Purpose:
 *     Get coordinates of straight scan path

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getsinglescan ( double angle, double pathlength, double accel[2],
 *                            double vmax[2], double samptime, int *scancount,
 *                            double **posptr, int *status )

 *  Arguments:
 *     angle = double (Given)
 *        Angle of pattern relative to the telescope axes in radians
 *        anticlockwise
 *     pathlength = double (Given)
 *        Length of scan path in arcsec
 *     accel = double[2] (Given)
 *        Telescope accelerations (arcsec)
 *     vmax = double[2] (Given)
 *        Telescope maximum velocities (arcsec)
 *     samptime = double (Given)
 *        Sample interval in sec
 *     scancount = int* (Returned)
 *        Number of positions in pattern
 *     posptr = double** (Returned)
 *        list of positions
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine determines the scanning positions for a scan in
 *     a straight line, given a scan length and angle from the
 *     telescope axes.
 *
 *     Effects of "jerk" - ie time for change in acceleration, or other
 *     telescope inertia effects are ignored.

 *  Authors:
 *     J.Balfour (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-07-25 (JB):
 *        Clone from sc2sim_getpong.c

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

void sc2sim_getsinglescan
(
 double angle,        /* angle of pattern relative to telescope
                         axes in radians anticlockwise (given) */
 double pathlength,   /* length of scanpath (arcsec) (given) */
 double accel[2],     /* telescope accelerations (arcsec) (given) */
 double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
 double samptime,     /* sample interval in sec (given) */
 int *scancount,      /* number of positions in pattern (returned) */
 double **posptr,     /* list of positions (returned) */
 int *status          /* global status (given and returned) */
 )

{
  /* Local variables */
  double cend[2];          /* ending coordinates in arcsec */
  double cstart[2];        /* starting coordinates in arcsec */

  int offset = 0;          /* Offset in pattern */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Get the ends of the scan path */
  cstart[0] = 0.0;
  cstart[1] = 0.0;
  cend[0] = pathlength * sin ( angle );
  cend[1] = pathlength * cos ( angle );

  /* Get the size of the scan */
  sc2sim_getscansegsize ( samptime, cstart, cend, accel, vmax, scancount,
                          status );

  /* Allocate memory for the list of positions */
  *posptr = astCalloc( (*scancount)*2, sizeof(**posptr) );
  /* Get the scan positions */

  sc2sim_getscanseg ( samptime, cstart, cend, accel, vmax, *scancount,
                      &offset, *posptr, status );

}
