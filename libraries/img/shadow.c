#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"
#include "sae_par.h"
#include "ems.h"

/*+
 * Name:
 *    shadow

 *  Purpose:
 *    Produces a false shadowing effect in an image.

 *  Description:
 *     This routine is a demonstration module for IMG. It accesses an
 *     input image, an output image and a temporary image as
 *     workspace. The output image contains the difference between the
 *     input image and a slightly shifted version.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

 *  Authors:
 *     PWD: Peter Draper (Starlink)

 *  History:
 *     03-JUN-1998 (PWD):
 *         Original Version

 *  Notes:
 *     This can be done without the requirement for workspace (with a
 *     little extra effort), but then it wouldn't be an example of how
 *     to get some. Note this workspace is disk based so may be mapped
 *     rather than just read into memory. This technique can be very
 *     useful when dealing with very large images.

 *-
 */

/* Prototypes: */
void do_shift( float *arrin, int nx, int ny, int xshift, int yshift,
               float *arrout, int *istat );

/* Useful defines: */
#define MAX(x,y) ( (x) > (y) ) ? (x) : (y)
#define MIN(x,y) ( (x) < (y) ) ? (x) : (y)


F77_SUBROUTINE(shadow)(INTEGER(istat))
{
  /*  Local Variables: */
  int  xshift, yshift;
  float *ipin, *ipout, *iptemp;
  int nx, ny;
  int i;

  /*  Access the input image. */
  imgIn( "IN", &nx, &ny, &ipin, istat );

  /*  Copy this to an output image. */
  imgOut( "IN", "OUT", &ipout, istat );

  /*  Get a temporary image as workspace. */
  imgTmp( "temp", nx, ny, &iptemp, istat );

  /*  Find out how far to shift the image. */
  printf( "XSHIFT - Amount to shift image in X > " );
  scanf( "%d", &xshift );
  printf( "YSHIFT - Amount to shift image in Y > " );
  scanf( "%d", &yshift );

  /*  Shift the input data and place result in the temporary image. */
  do_shift( ipin, nx, ny, xshift, yshift, iptemp, istat );

  /*  Take the difference between the input data and the shifted data
   *  putting the result in the output image. */
  /*  Form the difference. */
  for( i=0; i<ny*nx; i++ ) {
    if ( iptemp[i] != 0.0f ) {
      ipout[i] = ipin[i] - iptemp[i];
    } else {
      ipout[i] = 0.0f;
    }
  }

  /*  Free all the images (this deletes the temporary image). */
  imgFree( "*", istat );

  /*  If an error occurred add the routine name. */
  if( *istat !=  SAI__OK ) {
    emsRep( "SHADOW_ERR", "SHADOW: failed to produce output image.",
               istat );
  }
}


/*+
 *  Name:
 *    do_shift

 *  Purpose:
 *     Shifts a data array writing result into another array.

 *  Authors:
 *     PWD: Peter Draper (Starlink)

 *  History:
 *     03-JUN-1998 (PWD):
 *         Original Version

 *-
 */
void do_shift( float *arrin, int nx, int ny, int xshift, int yshift,
               float *arrout, int *istat )
{

  /*  Local Variables: */
  int xhigh, xlow, yhigh, ylow;
  int i, j, ii, jj, istart;

  /*  Check the global status. */
  if ( *istat != SAI__OK ) return;

  /*  Determine the bounds of the overlap regions. */
  xlow = MAX( 0, xshift );
  ylow = MAX( 0, yshift );
  xhigh = MIN( nx, nx + xshift );
  yhigh = MIN( ny, ny + yshift );

  /*  Loop over all the output array initialising it to 0.0. */
  for( i=0; i<nx*ny; i++ ) arrout[i] = 0.0f;

  /*  Now copy the data into the overlap region. */
  jj = ylow - yshift;
  istart = xlow - xshift;
  for( j=ylow; j<yhigh; j++, jj++ ) {
    ii = istart;
    for( i=xlow; i<xhigh; i++, ii++ ) {
      arrout[jj*nx+ii] = arrin[j*nx+i];
    }
  }
}

/* $Id$ */
