/*
 *+
 *  Name:
 *     sc2sim_fft2d.c

 *  Purpose:
 *     2-D FFT for double, square array

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Subroutine

 *  Invocation:
 *     sc2sim_fft2d ( int direction, int size, double array, int *status )

 *  Arguments:
 *     direction = int (Given)
 *        Transform specification +1 or -1
 *     size = int (Given)
 *        Square dimension of complex image array
 *     array = double* (Returned)
 *        Image array size*2 by size
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     The given array represents a square matrix [size][size] of complex
 *     numbers arranged so that each real value is followed by its
 *     imaginary partner.
 *
 *     The algorithm is to transform each row, then rotate through 90
 *     degrees, then to transform each row.
 *
 *     When going from data to frequency, the result leaves low frequencies
 *     at the corners of the 2-D transform.

 *  Authors:
 *     B.D.Kelly (ROE)
 *     J.Balfour (UBC)

 *  History :
 *     2006-09-25 (JB):
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

void sc2sim_fft2d
(
 int direction,       /* transform specification +1 or -1 (given) */
 int size,            /* square dimension of complex image array (given) */
 double *array,       /* image array size*2 by size (given and returned) */
 int *status          /* global status (given and returned) */
 )

{

  /* Local variables */
  int i;
  int j;                 /* loop counter */
  double vali;           /* stored imaginary value */
  double valr;           /* stored real value */

  /* Check status */
  if ( !StatusOkP(status) ) return;

  /* first of all transform all the rows */
  for ( j=0; j<size; j++ ) {
    sc2sim_four1 ( direction, size, &(array[j*2*size]) );
  }

  /* Rotate through 90 degrees */
  if ( direction == 1 ) {

    /* clockwise */

    for ( i=0; i<size/2; i++ ) {

      for ( j=i; j<size-1-i; j++ ) {

        valr = array[2*(size-1-i) + 2*size * j];
        vali = array[2*(size-1-i)+1 + 2*size * j];

        array[2*(size-1-i) + 2*size * j] =
          array[2*(size-1-j) + 2*size * (size-1-i) ];
        array[2*(size-1-i)+1 + 2*size * j] =
          array[2*(size-1-j)+1 + 2*size * (size-1-i) ];

        array[2*(size-1-j) + 2*size * (size-1-i) ] =
          array[2*i + 2*size * (size-1-j) ];
        array[2*(size-1-j)+1 + 2*size * (size-1-i) ] =
          array[2*i+1 + 2*size * (size-1-j) ];

        array[2*i + 2*size * (size-1-j) ] =
          array[2*j + 2*size * i];
        array[2*i+1 + 2*size * (size-1-j) ] =
          array[2*j+1 + 2*size * i];

        array[2*j + 2*size * i] = valr;
        array[2*j+1 + 2*size * i] = vali;
      }
    }

  } else {

    /* anticlockwise */
    for ( i=0; i<size/2; i++ ) {

      for ( j=i; j<size-1-i; j++ ) {
        valr = array[2*j + 2*size * i];
        vali = array[2*j+1 + 2*size * i];

        array[2*j + 2*size * i] =
          array[2*i + 2*size * (size-1-j) ];
        array[2*j+1 + 2*size * i] =
          array[2*i+1 + 2*size * (size-1-j) ];

        array[2*i + 2*size * (size-1-j) ] =
          array[2*(size-1-j) + 2*size * (size-1-i) ];
        array[2*i+1 + 2*size * (size-1-j) ] =
          array[2*(size-1-j)+1 + 2*size * (size-1-i) ];

        array[2*(size-1-j) + 2*size * (size-1-i) ] =
          array[2*(size-1-i) + 2*size * j];
        array[2*(size-1-j)+1 + 2*size * (size-1-i) ] =
          array[2*(size-1-i)+1 + 2*size * j];

        array[2*(size-1-i) + 2*size * j] = valr;
        array[2*(size-1-i)+1 + 2*size * j] = vali;
      }
    }
  }


  /* Transform all the rows (formerly columns) */
  for ( j=0; j<size; j++ ) {
    sc2sim_four1 ( direction, size, &(array[j*2*size]) );
  }

}
