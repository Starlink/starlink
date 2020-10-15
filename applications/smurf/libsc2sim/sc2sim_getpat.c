/*+
 *  Name:
 *     sc2sim_getpat

 *  Purpose:
 *     Return jiggle pattern

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_getpat ( int nvert, int smu_samples, double steptime,
 *                     double smu_offset, int conv-shape, double conv_sig,
 *                     int move_code, double jig_stepx, double jig_stepy,
 *                     int jig_vert[][2], dim_t *cycle_samples,
 *                     double pattern[][2], int *status )

 *  Arguments:
 *     nvert = int (Given)
 *        Number of vertices per pattern
 *     smu_samples = int (Given)
 *        Number of samples between vertices
 *     steptime = double (Given)
 *        Time between data samples in sec
 *     smu_offset = double (Given)
 *        SMU timing offset in msec
 *     conv_shape = int (Given)
 *        Choice of convolution function
 *     move_code = int (Given)
 *        SMU waveform choice
 *     jig_stepx = double (Given)
 *        X interval in arcsec
 *     jig_stepy = double (Given)
 *        Y interval in arcsec
 *     jig_vert = int[][2] (Given)
 *        Array with relative jiggle coordinates in units
 *        of jiggle steps in case jiggle positions are visited
 *     cycle_samples = dim_t* (Returned)
 *        The number of samples per cycle
 *     pattern = double[][2] (Returned)
 *        The array to hold the coordinates of the jiggle
 *        offsets in arcsec.  There are cycle_samples
 *        entries filled.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Fill the given arrays with lists of the X, Y offsets for a jiggle
 *     pattern.

 *  Authors:
 *     H.W. van Someren Greve (ASTRON)
 *     B.D.Kelly (ROE)
 *     Jennifer Balfour (UBC)
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History :
 *     2001-11-08 (Greve):
 *        Original
 *     2002-08-20 (BDK):
 *        C version
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2006-09-22 (JB):
 *        Removed DREAM specific code.
 *     2007-10-31 (TIMJ):
 *        Use dim_t

 *  Copyright:
 *     Copyright (C) 2007 Science and Technology Facilities Council.
 *     Copyright (C) 2002-2006 Particle Physics and Astronomy Research
 *     Council. Copyright (C) 2006 University of British Columbia.
 *     All Rights Reserved.

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

void sc2sim_getpat
(
 int nvert,            /* Number of vertices per pattern (given) */
 int smu_samples,      /* number of samples between vertices (given) */
 double steptime,      /* time between data samples in sec (given) */
 double smu_offset,    /* smu timing offset in msec (given) */
 int conv_shape __attribute__((unused)), /* choice of convolution function (given) */
 double conv_sig __attribute__((unused)), /* convolution parameter (given) */
 int move_code,        /* SMU waveform choice (given) */
 double jig_stepx,     /* X interval in arcsec (given) */
 double jig_stepy,     /* Y interval in arcsec (given) */
 int jig_vert[][2],    /* Array with relative jiggle coordinates in units of
                          jiggle steps in case jiggle positions are
                          visited (given) */
 dim_t *cycle_samples,   /* The number of samples per cycle (returned) */
 double pattern[][2],  /* The array to hold the coordinates of the jiggle
                          offsets in arcsec. There are cycle_samples entries
                          filled. (returned) */
 int *status           /* global status (given and returned) */
 )

{
  /* Local variables */
  dim_t j;
  double frac;
  double vertex_t;    /* time interval between jiggle vertices in msec */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /*  The implemented codes for the Jiggle Pattern are :
      nvert =1 : Circular pattern with 256 sampls.
      =8 : DREAM 8-pixel pattern. */

  if ( nvert == 0 ) {

    /* Generate a 256 point circular jiggle pattern, 2.5 pixels in
       diameter */
    frac = 2.0 * AST__DPI / 256.0;
    *cycle_samples = 256;

    if ( *cycle_samples > SC2SIM__MXSIM ) {
      *status = DITS__APP_ERROR;
      printf ( "GETPAT: cycle_samples too large, increase DREAM__MXSIM\n" );
      return;
    }

    for ( j=0; j<*cycle_samples; j++ ) {
      pattern[j][0] = COUNTTOSEC * 1.25 * cos ( frac * (double)(j-1) );
      pattern[j][1] = COUNTTOSEC * 1.25 * sin ( frac * (double)(j-1) );
    }

  } else {

    /* Calculate positions corresponding to data samples */
    *cycle_samples = nvert * smu_samples;

    if ( *cycle_samples > SC2SIM__MXSIM ) {
      *status = DITS__APP_ERROR;
      printf ( "GETPAT: cycle_samples too large, increase SC2SIM__MXSIM\n" );
      return;
    }

    /* Note: steptime is in SECONDS */
    vertex_t = steptime * smu_samples;
    sc2sim_smupath ( nvert, vertex_t, jig_vert, jig_stepx, jig_stepy,
                     move_code, smu_samples, steptime, smu_offset,
                     *cycle_samples, pattern, status );

  }

}



