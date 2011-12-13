#include <stdio.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    HDRWRITE

 *  Purpose:
 *     Writes a new header item.

 *  Description:
 *     This routine is part of the IMG example suite. It accesses an
 *     existing image and creates a copy as an output image. It then
 *     writes a new header item to the output image.

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
 *     This could be simplified by just modifying the input image rather
 *     than creating a copy. To do this just comment out the imgIn and
 *     imgOut lines. You might also want to change the hdrOut line to
 *     access the image via parameter "in".

 *-
 */

F77_SUBROUTINE(hdrwrite)(INTEGER(istat))
{

  /*  Local Variables: */
  char item[8];
  char comment[30], value[30];
  int nx, ny;
  float *ipin, *ipout;

  /*  Access the input image. */
  imgIn( "in", &nx, &ny, &ipin, istat );

  /*  Copy this to an output image. */
  imgOut( "in", "out", &ipout, istat );

  /*  Now get the name of the header item. */
  printf( "ITEM - Name of header item > " );
  scanf( "%8s", item );

  /*  And its value.*/
  printf( "VALUE - Header item value > " );
  scanf( "%30s", value );

  /*  And a comment for it. */
  printf( "COMMENT - Item description > " );
  scanf( "%30s", comment );

  /*  Now write it. */
  hdrOut( "out", " ", item, comment, value, 30, istat );

  /*  Free all the images. */
  imgFree( "*", istat );

}

/* $Id$ */
