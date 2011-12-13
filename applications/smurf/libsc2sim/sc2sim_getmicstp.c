/*
 *+
 *  Name:
 *     sc2sim_getmicstp

 *  Purpose:
 *     add microstepping pattern to pointing solution

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SC2SIM subroutine

 *  Invocation:
 *     sc2sim_getmicstp ( struct sc2sim_obs_struct *inx,
 *                        int count,
 *                        double *posptr,
 *                        int *status );

 *  Arguements:
 *     inx = sc2sim_obs_struct* (Given)
 *        structure for values from XML
 *     count = int (Given)
 *        total number of samples in observation
 *     posptr = double* (Given and Returned)
 *        list of positions
 *     status = int* (Given and Returned)
 *        pointer to global status

 *  Description:
 *     This routine superimposes the user-specified microstepping
 *     pattern on to the pointing solution. It is for use in STARE and
 *     DREAM modes.

 *  Authors:
 *     Christa VanLaerhoven
 *     {enter_new_authors_here}

 *  History:
 *     2007-08-13 (CV):
 *        Initial version.

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
 *     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *     MA 02110-1301, USA

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* includes */
#include <math.h>
#include "sae_par.h"
#include "sc2sim.h"

void sc2sim_getmicstp ( struct sc2sim_obs_struct *inx,
                        int count, double *posptr, int *status )
{

  /* local variables */
  int curframe;    /* index for current frame */
  int frmsperstp;  /* number of frames per microstep */
  int i;           /* loop counter */
  int ms;          /* loop counter */
  int n_micstps;   /* number of microsteps */
  double xoff,yoff; /* offsets from microstepping in x/y (arcsec) */

  /* check status */
  if ( ! *status == SAI__OK ) { return; }

  n_micstps = inx->nmicstep;
  frmsperstp = count / n_micstps;

  /* superimpose microstep pattern on to posptr */

  /* loop over microsteps */
  for ( ms=0; ms < n_micstps; ms++ ) {

    xoff = inx->mspat_x[ms] * inx->bol_distx;
    yoff = inx->mspat_y[ms] * inx->bol_disty;

    /* loop over frames */
    for ( i=0; i < frmsperstp; i++ ) {

      curframe = frmsperstp*ms + i;
      posptr[curframe*2] += xoff;
      posptr[curframe*2+1] += yoff;

    } /* endfor each frame in a microstep */
  } /* endfor microsteps */



} /* end sc2sim_getms */

