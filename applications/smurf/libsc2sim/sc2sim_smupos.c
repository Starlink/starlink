/*+
*  Name:
*     sc2sim_smupos

*  Purpose:
*     Calculate the SMU position at an instant

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_smupos ( double t, double vertex_t, int movecode,
*                     int nvert, double vertxy[][2], double *x,
*                     double *y, int *status )

*  Arguments:
*     t = double (Given)
*        Time from start of pattern in msec
*     vertex_t = double (Given)
*        Time for movement between vertices in msec
*     movecode = int (Given)
*        The code for the SMU waveform that determines the
*        SMU motion
*     nvert = int (Given)
*        number of vertices in the DREAM pattern
*     vertxy = double[][2] (Given)
*        Table of vertex offsets in arcsec
*     x = double* (Given)
*        Calculated X-positon
*     y = double* (Given)
*        Calculated Y-position
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine calculates the SMU waveform between 2 Jiggle positions.

*  Authors:
*     H.W. van Someren Greve (ASTRON)
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History :
*     2001-07-09 (Greve):
*        Original
*     2002-11-01 (BDK):
*        C version
*     2003-02-12 (bdk)
*        Adapted from original routine dsim_smuwave
*     2006-09-15 (JB):
*        Convert to sc2sim_smupos from dream_smupos

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

/* Sc2sim includes */
#include "smurf_par.h"
#include "sc2sim.h"


#define FUNC_NAME "sc2sim_smu_pos"

void sc2sim_smupos ( double t, double vertex_t, int movecode,
                     int nvert, double vertxy[][2], double *x,
                     double *y, int *status ) {

   /* Local variables */
   int jstart;                /* index of vertex before sample point */
   int jend;                  /* index of vertex after sample point */
   double mxv;                /* Maximum value */
   double theta;              /* Fractional time between vertices in radians */
   double frac;               /* fractional distance between vertices */
   double offset;             /* time after passing first vertex in msec */

   if ( !StatusOkP(status) ) return;

   /* Get vertex coordinates bracketing the measurement instant */
   if ( t < 0.0 ) {
      /* Add a cycle */
      jstart = ( (int)( (t + (double)nvert * vertex_t ) / vertex_t) )
               % nvert;
      offset = t + (double)nvert * vertex_t - (double)jstart * vertex_t;
   } else {
      jstart = ( (int)(t / vertex_t) ) % nvert;
      offset = t - (double)jstart * vertex_t;
   }

   jend = ( jstart + 1 ) % nvert;

   if ( movecode == 0 ) {

      /* Block wave. This is unrealistic, but is added for compatibility
         with old code. */
      *x = vertxy[jend][0];
      *y = vertxy[jend][1];

   } else if ( movecode == 1 ) {

      /* 2 term not damped. */
      mxv = sin ( M_PI / 4.0 ) + sin ( 3.0 * M_PI / 4.0 ) / 3.0;
      theta = ( -M_PI / 4.0 ) + offset * M_PI / vertex_t;
      frac = 1.104 * ( ( sin(theta) + sin ( 3.0 * theta ) / 3.0 ) /
             ( 2.0 * mxv ) + 0.5 );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 2 ) {

      /* 3 term not damped. */
      mxv = sin ( M_PI / 6.0 ) + sin ( 3.0 * M_PI / 6.0 ) / 3.0 +
            sin ( 5.0 *M_PI / 6.0 ) / 5.0;
      theta = ( -M_PI / 6.0 ) + offset * M_PI / vertex_t;
      frac = 1.104 * ( ( sin(theta) + sin ( 3.0 * theta ) / 3.0 +
             sin ( 5.0 * theta ) / 5.0 ) / ( 2.0 * mxv ) + 0.5 );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 3 ) {

      /* 4 term not damped. */
      mxv = sin ( M_PI / 8.0 ) + sin ( 3.0 * M_PI / 8.0 ) / 3.0 +
            sin ( 5.0 * M_PI / 8.0 ) / 5.0 + sin ( 7.0 * M_PI / 8.0 ) / 7.0;
      theta = (-M_PI/8.0) + offset * M_PI / vertex_t;
      frac = 1.104 * ( ( sin(theta) + sin ( 3.0 * theta ) / 3.0 +
             sin ( 5.0 * theta ) / 5.0 + sin ( 7.0 * theta ) / 7.0 )
             / ( 2.0 * mxv ) + 0.5 );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 4 ) {

      /* 2 term flat end. */
      mxv = sin(M_PI/4.0) + sin(3.0*M_PI/4.0) / 3.0;
      theta = (-M_PI/4.0) + offset * M_PI / vertex_t;

      if ( theta < M_PI/4.0 ) {
         frac =1.02 * ( ( sin(theta) + sin(3.0*theta) / 3.0 ) / (2.0*mxv) + 0.5 );
      } else {
         frac = 1.0;
      }

      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 5 ) {

      /* 3 term flat end. */
      mxv = sin(M_PI/6.0) + sin(3.0*M_PI/6.0) / 3.0 + sin(5.0*M_PI/6.0) / 5.0;
      theta = (-M_PI/6.0) + offset * M_PI / vertex_t;

      if ( theta < M_PI/6.0 ) {
         frac = ( sin(theta) + sin(3.0*theta) / 3.0 +  sin(5.0*theta) / 5.0 ) /
                (2.0*mxv) + 0.5;
      } else {
         frac = 1.0;
      }

      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 6 ) {

      /* 4 term flat end. */
      mxv = sin(M_PI/8.0) + sin(3.0*M_PI/8.0) / 3.0 +
            sin(5.0*M_PI/8.0) / 5.0 + sin(7.0*M_PI/8.0) / 7.0;
      theta = -M_PI/8.0 + offset * M_PI / vertex_t;

      if ( theta < M_PI/8.0 ) {
         frac = ( sin(theta) + sin(3.0*theta) / 3.0 + sin(5.0*theta) / 5.0 +
                sin(7.0*theta) / 7.0 ) / (2.0*mxv) + 0.5;
      } else {
         frac = 1.0;
      }

      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 7 ) {

      /* ScubaWave. After 1 Ms 0.098. After 8 Ms 0.913. After 9 Ms 1.000.
         popepi points is equivalent with 64 Ms. */
      if ( offset >= 9.0 ) {
         frac = 1.0;
      } else if ( offset >= 8.0 ) {
         frac = 0.913 + 0.087 * ( offset - 8.0 );
      } else if ( offset >= 1.0 ) {
         frac = 0.098 + 0.116428571 * ( offset - 1.0 );
      } else {
         frac = 0.098 * offset;
      }

      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   } else if ( movecode == 8 ) {

      /* Experimental.
         This is an experimental wave form, which may change often..
         Now it is a cosine waveform from 0 to 1 in the full time. */
      frac = 0.5 * ( 1.0 - cos ( M_PI * offset / vertex_t ) );
      *x = frac * ( vertxy[jend][0] - vertxy[jstart][0] ) + vertxy[jstart][0];
      *y = frac * ( vertxy[jend][1] - vertxy[jstart][1] ) + vertxy[jstart][1];

   }
}
