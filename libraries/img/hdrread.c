#include <stdio.h>
#include <string.h>
#include "f77.h"
#include "img.h"


/*+
 * Name:
 *    hdrread

 *  Purpose:
 *     Reports the value of a header item.

 *  Description:
 *     This routine is part of the IMG example suite. It accesses an
 *     existing image and reads the value of a named item. The value is
 *     then reported.

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
 *     - The headers accessed by this routine are FITS items.
 *
 *     - The output is written using MSG routines rather than
 *       print statements. MSG is documented in SUN/104.

 *-
 */

F77_SUBROUTINE(hdrread)(INTEGER(istat))
{

  /*  Local Variables: */
  char item[8];
  char value[40];
  int n;

  /*  Get the name of the FITS item we are to read. */
  printf( "ITEM - Name of header item > " );
  scanf( "%8s", item );

  /*  See if it exists (this call also accesses the image). */
  hdrNumb( "IN", " ", item, &n, istat );
  if ( n > 0 ) {

    /*  Try to read the value. */
    hdrIn( "IN", " ", item, 1, value, 40, istat );

    /*  And write it out */
    printf( "The header item '%s' has a value of %s.\n", item, value);
  } else {

    /*  Item doesn't exist so make a report to this effect. */
    printf( "The header item '%s' doesn''t exist.\n", item );
  }

  /*  Free the input image. */
  imgFree( "IN", istat );
}

/* $Id$ */
