/*
 *+
 *  Name:
 *     sc2sim_invf2d.c

 *  Purpose:
 *     Generate a 2-D image of 1/f noise

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_invf2d ( double corner, double p,
 *                     double pixsize, int size, double *fnoise,
 *                     double *spectrum, int *status )

 *  Arguments:
 *     corner = double (Given)
 *        Corner (1/f knee) frequency
 *     p = double (Given)
 *        Power law to be used (2 is the typical value)
 *     pixsize = double (Given)
 *        Pixsize is arcsec
 *     size = int* (Given)
 *        Size of square image array
 *     fnoise = double* (Returned)
 *        Array to hold noise image
 *     spectrum = double* (Returned)
 *        Array to hold complex 2-D spectrum
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Return a 2-D image containing spatial noise following a power law with
 *     exponent p which would gives power normalized to 1 pW at the corner
 *     (1/f knee) frequency at which point the noise will be dominated
 *     by photon noise rather than sky variations.
 *
 *     Simulate an f**(-p) noise field by generating a white noise image,
 *     Fourier transforming it, applying an f**(-p) law, then transforming back
 *     again.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     J.Balfour (UBC)
 *     Ed Chapin (UBC)

 *  History :
 *     2006-09-25 (JB):
 *        Split from dsim.c
 *     2007-06-27 (EC):
 *        Removed sigma argument so that the image is normalized to a power
 *        of 1 pW at the 1/f knee (or "corner") frequency. The map can then be
 *        easily scaled on-the-fly when we do scan simulations.

 *  Copyright:
 *     Copyright (C) 2006-2007 University of British Columbia. All
 *     Rights Reserved.

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

void sc2sim_invf2d
(
 double corner,    /* corner frequency in per arcsec (given) */
 double p,         /* power law to be used (given) */
 double pixsize,   /* pixel size in arcsec (given) */
 int size,         /* size of square image array (given) */
 double *fnoise,   /* array to hold noise image (returned) */
 double *spectrum, /* array to hold complex 2-D spectrum (returned) */
 int *status       /* global status (given and returned) */
 )

{

  /* Local variables */
  int i;
  int j;
  double counttonu;
  double fourfact;
  double fscale;
  double nu;
  double x;
  double y;

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* Generate a set of random numbers with a bell-shaped distribution,
     dispersion 1.0 and zero mean */

  for ( j=0; j<size*size; j++ ) {
    spectrum[2*j] = sc2sim_drand ( 1.0 );
    spectrum[1+2*j] = 0.0;
  }

  /* Fourier transform  */
  sc2sim_fft2d ( -1, size, spectrum, status );

  /* Modify to give 1/f characteristics */
  counttonu = 1.0 / ( pixsize * (double)size * 0.5 );

  /* fscale = sigma * pow ( corner, p ); */
  fscale = pow( corner, p );

  for ( j=0; j<size; j++ ) {

    if ( j > size/2 ) {
      y = (double) ( size - j );
    } else {
      y = (double) j;
    }

    for ( i=0; i<size; i++ ) {

      if ( i > size/2 ) {
        x = (double) ( size - i );
      } else {
        x = (double) i;
      }

      nu = counttonu * sqrt ( x*x + y*y );

      if ( nu > 1.0e-10 ) {
        spectrum[j*size*2+i*2] =
          spectrum[j*size*2+i*2] * fscale / pow ( nu, p );
        spectrum[j*size*2+i*2+1] =
          spectrum[j*size*2+i*2+1] * fscale / pow ( nu, p );
      } else {
        spectrum[j*size*2+i*2] =
          spectrum[j*size*2+i*2] * fscale;
        spectrum[j*size*2+i*2+1] =
          spectrum[j*size*2+i*2+1] * fscale;
      }
    }
  }

  /* Reverse Fourier transform */
  sc2sim_fft2d ( 1, size, spectrum, status );

  /* Extract a noise sequence */
  fourfact = 1.0 / (size*size);

  for ( j=0; j<size*size; j++ ) {
    fnoise[j] = fourfact * spectrum[2*j];
  }

}
