/*
 *+
 *  Name:
 *     sc2sim_getbous

 *  Purpose:
 *     Get coordinates of boustrophedon scanning

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getbous ( double angle, double bouswidth, double bousheight,
 *                      double spacing, double accel[2], double vmax[2],
 *                      double samptime, int *bouscount, double **posptr,
 *                      int *status )

 *  Arguments:
 *     angle = double (Given)
 *        Angle of pattern relative to the telescope axes in radians
 *        anticlockwise
 *     bouswidth = double (Given)
 *        Width of boustrophedon pattern in arcsec
 *     bousheight = double (Given)
 *        Height of boustrophedon pattern in arcsec
 *     spacing = double (Given)
 *        Distance between scans across sky (arcsec)
 *     accel = double[2] (Given)
 *        Telescope accelerations (arcsec)
 *     vmax = double[2] (Given)
 *        Telescope maximum velocities (arcsec)
 *     samptime = double (Given)
 *        Sample interval in sec
 *     bouscount = int* (Returned)
 *        Number of positions in pattern
 *     posptr = double** (Returned)
 *        list of positions
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     This routine determines the scanning positions for a scan in
 *     a boustrophedon pattern, given an angle, width and height,
 *     and number of scans.
 *
 *     Effects of "jerk" - ie time for change in acceleration, or other
 *     telescope inertia effects are ignored.

 *  Authors:
 *     J.Balfour (UBC)
 *     {enter_new_authors_here}

 *  History :
 *     2006-07-26 (JB):
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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Standard includes */
#include <stdio.h>
#include <math.h>

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "libsmf/smf.h"

void sc2sim_getbous
(
 double angle,        /* angle of pattern relative to telescope
                         axes in radians anticlockwise (given) */
 double bouswidth,    /* width of bous pattern (arcsec) */
 double bousheight,   /* height of bous pattern (arcsec) */
 double spacing,      /* distance between scans (arcsec) (given) */
 double accel[2],     /* telescope accelerations (arcsec) (given) */
 double vmax[2],      /* telescope maximum velocities (arcsec) (given) */
 double samptime,     /* sample interval in sec (given) */
 int *bouscount,      /* number of positions in pattern (returned) */
 double **posptr,     /* list of positions (returned) */
 int *status          /* global status (given and returned) */
 )

{
  /* Local variables */
  int scancount;           /* number of scan paths across sky */
  double cend[2];          /* ending coordinates in arcsec */
  double cstart[2];        /* starting coordinates in arcsec */
  int curroff;             /* index of next free slot in position list */
  int j;                   /* loop counter */
  int direction = 1;       /* flag for direction of scan */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Determine the number of scan paths */
  scancount = bousheight / spacing;

  /* Set up the beginning of the scan */
  cend[0] = 0.0;
  cend[1] = 0.0;
  (*bouscount) = 0;

  /* Get the size of the scan */
  for ( j = 0; j < scancount; j++ ) {

    /* Do a scan across the sky */
    cstart[0] = cend[0];
    cstart[1] = cend[1];
    cend[0] = cend[0] + ( direction * ( bouswidth * cos ( angle ) ) );
    cend[1] = cend[1] + ( direction * ( bouswidth * sin ( angle ) ) );

    /* Increment the number of positions */
    sc2sim_getscansegsize ( samptime, cstart, cend, accel, vmax, &curroff,
                            status );

    (*bouscount) += curroff;

    /* Move up to the next scan path */
    cstart[0] = cend[0];
    cstart[1] = cend[1];
    cend[0] = cend[0] - ( spacing * sin ( angle ) );
    cend[1] = cend[1] + ( spacing * cos ( angle ) );

    /* Increment the number of positions */
    sc2sim_getscansegsize ( samptime, cstart, cend, accel, vmax, &curroff,
                            status );

    (*bouscount) += curroff;

    /* Change directions */
    if ( direction > 0 )
      direction = -1;
    else
      direction = 1;

  }

  /* Allocate memory for the list of positions */
  *posptr = astCalloc( (*bouscount)*2, sizeof(**posptr) );
  curroff = 0;
  cend[0] = 0.0;
  cend[1] = 0.0;
  direction = 1;

  /* Get the scan positions */
  for ( j = 0; j < scancount; j++ ) {

    /* Do a scan across the sky */
    cstart[0] = cend[0];
    cstart[1] = cend[1];
    cend[0] = cend[0] + ( direction * ( bouswidth * cos ( angle ) ) );
    cend[1] = cend[1] + ( direction * ( bouswidth * sin ( angle ) ) );

    /* Get positions for this scan */
    sc2sim_getscanseg ( samptime, cstart, cend, accel, vmax, *bouscount,
                        &curroff, *posptr, status );

    /* Move up to the next scan path */
    cstart[0] = cend[0];
    cstart[1] = cend[1];
    cend[0] = cend[0] - ( spacing * sin ( angle ) );
    cend[1] = cend[1] + ( spacing * cos ( angle ) );

    /* Get positions for moving up */
    /* Get positions for this scan */
    sc2sim_getscanseg ( samptime, cstart, cend, accel, vmax, *bouscount,
                        &curroff, *posptr, status );

    /* Change directions */
    if ( direction > 0 )
      direction = -1;
    else
      direction = 1;

  }

}
