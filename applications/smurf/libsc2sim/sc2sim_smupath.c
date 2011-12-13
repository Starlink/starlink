/*
 *+
 *  Name:
 *     sc2sim_smupath

 *  Purpose:
 *     Calculate the path positions of the SMU

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_smupath ( int nvert, double vertex_t, int jig_ver[][2],
 *                      double jig_stepx, double jig_stepy, int movecode,
 *                      int nppp, double steptime, double smu_offset,
 *                      int pathsz, double jigpath[][2], int *status )

 *  Arguments:
 *     nvert = int (Given)
 *        Number of vertices in the jiggle pattern,
 *        implemented are :
 *        =1 : No visit of points.
 *        At the moment a circle but that does not work !
 *        =4 : Visit 4 points on a square.
 *        =5 : Visit 5 points on a '+'
 *        =8 : Visit 8 points on a star. (This is the best)
 *     vertex_t = double (Given)
 *        Time for movement between vertices in sec
 *     jig_vert = int[][2] (Given)
 *        Table with relative vertex coordinates in time
 *     jig_stepx = double (Given)
 *        The step size in -X- direction between Jiggle
 *        positions in arcsec
 *     jig_stepy = double (Given)
 *        The step size in -Y- direction between Jiggle
 *        positions in arcsec
 *     movecode = int (Given)
 *        The code for the SMU waveform that determines the
 *        SMU motion
 *     nppp = int (Given)
 *        The number of calculated coordinates in the path
 *        between 2 successive vertices
 *     steptime = double (Given)
 *        Time between samples in sec
 *     smu_offset = double (Given)
 *        smu timing offset in msec
 *     pathsz = int (Given)
 *        maximum no of path points
 *     jigpath = double[][2] (Returned)
 *        Buffer containing the X and Y coordinates of each
 *        point in the path of the SMU during the Jiggle in
 *        units of arcsec
 *     status = int* (Given and Returned)
 *        global status


 *  Description:
 *     This routine calculates all the SMU positions during the cycle
 *     This depends on the Jiggle Pattern, the SMU waveform, and the
 *     overdimensioning factor.

 *  Authors:
 *     H.W. van Someren Greve (ASTRON)
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2001-07-09 (Greve):
 *        Original
 *     2002-10-31 (bdk):
 *        C translation
 *     2003-02-13 (bdk):
 *        change to use dream_smupos and to allow smu_offset
 *     2003-06-20 (bdk):
 *        pass-in pathsz, change constant name to DREAM__MXVERT
 *     2006-09-15 (JB):
 *        Convert to sc2sim_smupath from dream_smupath

 *  Copyright:
 *     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
 *     Council, University of British Columbia. All Rights Reserved.

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
#include "mers.h"

/* SC2SIM includes */
#include "smurf_par.h"
#include "sc2sim.h"
#include "sc2sim_par.h"

#define FUNC_NAME "sc2sim_smu_path"

void sc2sim_smupath ( int nvert, double vertex_t, int jig_vert[][2],
                      double jig_stepx, double jig_stepy, int movecode,
                      int nppp, double steptime, double smu_offset,
                      int pathsz, double jigpath[][2], int *status )

{

  /* Local variables */
  int np;                          /* nr of calculated points */
  int j;                           /* Loop variable */
  double anginc;                   /* Angle increment */
  double t;                        /* time at a point in the path */
  double vertxy[SC2SIM__MXVERT][2]; /* coordinates of jiggle vertices */

  if ( !StatusOkP(status) ) return;

  /* Make the number of positions in the Jiggle Path, and check the size */
  np = nppp * nvert;

  if ( np > pathsz )
    {
      *status = DITS__APP_ERROR;
      msgSeti ( "NP", np );
      msgSeti ( "PATHSZ", pathsz );
      msgOut(" ",
             "^NP points in the Jiggle path requested, but only ^PATHSZ points allowed",
             status );
      return;
    }

  if ( nvert > 1) {

    /* Calculate all the positions. */
    for ( j=0; j<nvert; j++ ) {
      vertxy[j][0] = jig_stepx * (double)jig_vert[j][0];
      vertxy[j][1] = jig_stepy * (double)jig_vert[j][1];
    }

    for ( j=0; j<np; j++ ) {
      t = (double)j * steptime + smu_offset;
      sc2sim_smupos ( t, vertex_t, movecode, nvert, vertxy,
                      &(jigpath[j][0]), &(jigpath[j][1]), status );
    }

  } else if ( nvert == 1) {

    anginc = 2.0 * M_PI / (double)np;

    for ( j=0; j<np; j++ ) {
      jigpath[j][0] = 1.2 * jig_stepx * cos ( (double)j * anginc );
      jigpath[j][1] = 0.9 * jig_stepy * sin ( (double)j * anginc );
    }
  }

}
