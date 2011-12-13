/*
 *+
 *  Name:
 *     sc2sim_invf

 *  Purpose:
 *     Generate a 1/f plus white noise sequence

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_invf ( double sigma, double corner, double samptime,
 *                   int nsamples, double *fnoise, int *status )

 *  Arguments:
 *     sigma = double (Given)
 *        White noise level
 *     corner = double (Given)
 *        Corner frequency
 *     samptime = double (Given)
 *        Time in sec between samples
 *     nsamples = int (Given)
 *        Number of positions in sequence
 *     fnoise = double* (Returned)
 *        Array to hold noise sequence
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Return a sequence of values with combined 1/f and white noise
 *     characteristics.
 *     Simulate a 1/f noise sequence by generating white noise sequence,
 *     Fourier transforming it, applying a 1/f law, then transforming back
 *     again. Generate and add a new white noise sequence.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     {enter_new_authors_here}

 *  History :
 *     2001-07-24 (BDK):
 *        Original
 *     2002-08-20 (BDK)
 *        C version
 *     2003-06-13 (BDK)
 *        Change argument name to nsamples
 *     2006-07-21 (JB):
 *        Split from dsim.c

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

void sc2sim_invf
(
 double sigma,     /* white noise level (given) */
 double corner,    /* corner frequency (given) */
 double samptime,  /* time in sec between samples (given) */
 int nsamples,     /* number of positions in sequence (given) */
 double *fnoise,   /* array to hold noise sequence (returned) */
 int *status       /* global status (given and returned) */
 )

{
  /* Local variables */
  int j;
  static double spectrum[4096];
  double counttonu;
  double fourfact;
  double fscale;
  double nu;
  double power;
  double y;

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Generate a set of random numbers with a bell-shaped distribution,
     dispersion 1.0 and zero mean */
  for ( j=0; j<2048; j++ ) {
    spectrum[2*j] = sc2sim_drand ( 1.0 );
    spectrum[1+2*j] = 0.0;
  }

  /* Fourier transform */
  sc2sim_four1 ( +1, 2048, spectrum );

  /* Modify to give 1/f characteristics */
  counttonu = 1.0 / ( samptime * 1024.0 );
  power = 1.0;
  fscale = sigma * pow ( corner, power );

  for ( j=0; j<2048; j++ ) {

    if ( j > 1024 ) {
      y = (double) ( 2048 - j );
    } else {
      y = (double) j;
    }

    nu = counttonu * y;

    if ( nu > 1.0e-10 ) {
      spectrum[j*2] = spectrum[j*2] * fscale / pow ( nu, power );
      spectrum[j*2+1] = spectrum[j*2+1] * fscale / pow ( nu, power );
    } else {
      spectrum[j*2] = spectrum[j*2] * fscale;
      spectrum[j*2+1] = spectrum[j*2+1] * fscale;
    }

  }

  /* Reverse Fourier transform */
  sc2sim_four1 ( -1, 2048, spectrum );

  /* Extract a noise sequence */
  fourfact = 1.0 / 2048.0;

  for ( j=0; j<nsamples; j++ ) {
    fnoise[j] = fourfact * spectrum[2*j];
  }

  /* Generate a set of random numbers with a bell-shaped distribution
     and add onto the 1/f noise */
  for ( j=0; j<nsamples; j++ ) {
    fnoise[j] = fnoise[j] + sc2sim_drand ( sigma );
  }


}
