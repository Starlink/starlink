/*
 *+
 *  Name:
 *     sc2sim_four1

 *  Purpose:
 *     Cooley-Tukey fft by Brenner

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_four1 ( int isign, int nn, double data[] )

 *  Arguments:
 *     isign = int (Given)
 *        Direction of transform
 *     nn = int (Given)
 *        Number of complex values
 *     data = double[] (Given and Returned)
 *        Complex signal transformed in-place - even indices
 *        real values, odd imaginary

 *  Description:
 *     1-D Fourier transform originally published in FORTRAN by Brenner
 *     (see Mertz, Applied Optics vol 10 p386 1971).

 *  Authors:
 *     B.D.Kelly (bdk@roe.ac.uk)
 *     {enter_new_authors_here}

 *  History :
 *     2002-08-10 (BDK):
 *        Original
 *     2006-07-20 (JB):
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

void sc2sim_four1
(
 int isign,         /* direction of transform (given) */
 int nn,            /* number of complex values (given) */
 double data[]      /* complex signal transformed in-place - even indices
                       real values, odd imaginary (given and returned) */
 )

{
  /* Local variables */
  int m;
  int mmax;
  int n;
  int j;
  int i;
  int istep;
  double tempr;
  double tempi;
  double sinth;
  double wstpr;
  double wstpi;
  double wr;
  double wi;
  double theta;

  n = 2 * nn;
  j = 0;

  for ( i=0; i<n; i+=2 ) {
    if( i < j )  {
      tempr = data[j];
      tempi = data[j+1];
      data[j] = data[i];
      data[j+1] = data[i+1];
      data[i] = tempr;
      data[i+1] = tempi;
    }

    m = n / 2;

    while ( m >= 2) {
      if ( j < m ) break;
      j = j - m;
      m = m / 2;
    }

    j = j + m;

  }

  mmax = 2;

  while ( mmax < n ) {
    istep = 2 * mmax;
    theta = 6.28318530717959 / (double)( isign * mmax );
    sinth = sin ( theta / 2.0 );
    wstpr = -2.0 * sinth * sinth;
    wstpi = sin ( theta );
    wr = 1.0;
    wi = 0.0;
    for ( m=0; m<mmax; m+=2 ) {

      for ( i=m; i<n; i+=istep ) {
        j = i + mmax;
        tempr = wr * data[j] - wi * data[j+1];
        tempi = wr * data[j+1] + wi * data[j];
        data[j] = data[i] - tempr;
        data[j+1] = data[i+1] - tempi;
        data[i] = data[i] + tempr;
        data[i+1] = data[i+1] + tempi;
      }

      tempr = wr;
      wr = wr * wstpr - wi * wstpi + wr;
      wi = wi * wstpr + tempr * wstpi + wi;
    }

    mmax = istep;

  }

}
