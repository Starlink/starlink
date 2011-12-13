#include <stdio.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    flat

 *  Purpose:
 *     Creates a false flatfield.

 *  Description:
 *     This is a demonstration routine for IMG. It creates a new image
 *     and fills it with ones.

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

 *-
 */

F77_SUBROUTINE(flat)(INTEGER(istat))
{

  /*  Local variables: */
  float *ip;
  int i;

  /*  Create a new image. */
  imgNew( "out", 416, 578, &ip, istat );

  /*  Set all its elements to the value 1.0. */
  for( i=0; i < 416*578; i++ ) {
    ip[i] = 1.0f;
  }

  /*  Free the new image. */
  imgFree( "out", istat );

}

/* $Id$ */
