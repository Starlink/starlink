/*
 *+
 *  Name:
 *     sc2sim_fitheat

 *  Purpose:
 *     Fit a cubic polynomial to each bolometer

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_fitheat ( int nboll, int nframes, double *heat, double *inptr,
 *                      double *coptr, int *status )

 *  Arguments:
 *     nboll = int (Given)
 *        Number of bolometers
 *     nframes = int (Given)
 *        Number of frames in scan
 *     heat = double* (Given)
 *        Heater values
 *     inptr = double* (Given)
 *        Measurement values
 *     coptr = double* (Returned)
 *        Coefficients of fit
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Fit a cubic to the data set for each bolometer.

 *  Authors:
 *     B.D.Kelly (bdk@roe.ac.uk)
 *     {enter_new_authors_here}

 *  History :
 *     2005-05-19 (BDK):
 *        Original
 *     2005-06-10 (BDK):
 *        Use middle of range as zero point
 *     2005-08-18 (BDK):
 *        Put into dsim library from map library
 *     2006-07-20 (JB):
 *        Split from dsim.c
 *     2007-12-18 (AGG):
 *        Update to use new smf_free behaviour

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
#include <stdio.h>

#include "sc2da/sc2math.h"

/* SC2SIM includes */
#include "sc2sim.h"

/* SMURF includes */
#include "libsmf/smf.h"

void sc2sim_fitheat
(
 int nboll,             /* number of bolometers (given) */
 int nframes,           /* number of frames in scan (given) */
 double *heat,          /* heater values (given) */
 double *inptr,         /* measurement values (given) */
 double *coptr,         /* coefficients of fit (returned) */
 int *status            /* global status (given and returned) */
 )

{
  /* Local variables */
  double coeff[4];    /* fit coefficients */
  int ncoeff;         /* number of fit coefficients */
  int i;              /* loop counter */
  int j;              /* loop counter */
  double *ht;         /* copy of heater settings */
  double *scan;       /* copy of scan for single bolometer */
  double var[4];      /* fit variances */
  FILE *fd;
  double t;

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* provide default values */
  ncoeff = 4;

  for ( j=0; j<nboll; j++ ) {
    for ( i=0; i<ncoeff; i++ ) {
      coptr[j+i*nboll] = 0.0;
    }
  }

  if ( nframes > 10 ) {
    scan = astCalloc( nframes, sizeof(*scan) );
    ht = astCalloc( nframes, sizeof(*ht) );

    for ( i=0; i<nframes; i++ ) {
      ht[i] = heat[i] - heat[nframes/2];
    }

    for ( j=0; j<nboll; j++ ) {

      /* extract the values for one bolometer */
      for ( i=0; i<nframes; i++ ) {
        scan[i] = inptr[nboll*i+j] - inptr[nboll*(nframes/2)+j];
      }

      /* fit a cubic */
      sc2math_cubfit ( nframes, scan, ht, coeff, var, status );

      for ( i=0; i<4; i++ ) {
        coptr[(i+2)*nboll+j] = coeff[i];
      }
      coptr[j] = heat[nframes/2];
      coptr[nboll+j] = inptr[nboll*(nframes/2)+j];

      if ( j == 740 ) {
        fd = fopen ( "fit.txt", "w" );

        for ( i=0; i<nframes; i++ ) {
          t = coptr[j]
            + coptr[2*nboll+j]
            + coptr[3*nboll+j] * scan[i]
            + coptr[4*nboll+j] * scan[i] * scan[i]
            + coptr[5*nboll+j] * scan[i] * scan[i] * scan[i];
          fprintf ( fd, "%e %e %e\n", inptr[nboll*i+j], heat[i], t );
        }
        fclose ( fd );
      }
    }

    scan = astFree( scan );

  }

}
