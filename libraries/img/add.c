#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    add

 *  Purpose:
 *     Adds a constant value to all the elements of an image.

 *  Description:
 *     This is a demonstration routine for IMG. It creates a copy of an
 *     existing image and then adds a specified constant to all the
 *     image elements.

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

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

 *  Authors:
 *     PWD: Peter Draper (Starlink)

 *  History:
 *     03-JUN-1998 (PWD):
 *         Original Version

 *  Notes:
 *     This routine could also be implemented to just modify the input
 *     image, rather than creating a new copy.
 *
 *     The PAR routines should be used to access the data value when
 *     they are available with a C interface.

 *-
 */

F77_SUBROUTINE(add)(INTEGER(status))
{
  int nx, ny;
  float *ptrIn, *ptrOut;
  float value;
  int i;

  /*  Access an existing image */
  imgIn( "IN", &nx, &ny, &ptrIn, status );

  /*  Copy this to an output image */
  imgOut( "IN", "OUT", &ptrOut, status );

  /*  Get the value to add. */
  /*  parGet0r( "CONSTANT", value, status ); */
  printf("CONSTANT - value to add to image> ");
  scanf( "%e", &value );

  /*  And do the work. */
  for( i=0; i <nx*ny; i++ ) {
    ptrOut[i] = value + ptrIn[i];
  }

  /*  Free the input and output images. */
  imgFree( "*", status );
}

/* $Id$ */
