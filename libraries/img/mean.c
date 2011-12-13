#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    mean

 *  Purpose:
 *     Calculates and reports the mean value of an image.

 *  Description:
 *     This is a demonstration routine for IMG. It accesses an existing
 *     image and calculates the mean value which it then writes to the
 *     terminal.

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

 *-
 */

F77_SUBROUTINE(mean)(INTEGER(istat))
{
  /* Local variables: */
  float *ip;
  int nx, ny;
  float sum;
  int i;

  /*  Access an input image. */
  imgIn( "in", &nx, &ny, &ip, istat );

  /*  Derive the mean and write it out. */
  sum = 0.0f;
  for( i=0; i < nx*ny; i++ ) sum += ip[i];
  printf ("Mean value = %f\n", sum/(nx*ny) );

  /*  Free the input image. */
  imgFree( "in", istat );
}

/* $Id$ */
