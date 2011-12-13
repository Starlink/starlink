#include <stdio.h>
#include "f77.h"
#include "img.h"

/*+
 * Name:
 *    proc

 *  Purpose:
 *    Processes a CCD frame.

 *  Description:
 *     This is a demonstration routine for IMG. It accesses several
 *     images using one call.

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

F77_SUBROUTINE(proc)(INTEGER(istat))
{
   float *ip[3], *ipproc, *ipbias, *ipflat, *ipraw ;
   int nx, ny, i;

   /*  Access images. */
   imgIn( "bias,flat,raw", &nx, &ny, ip, istat );
   ipbias = ip[0];
   ipflat = ip[1];
   ipraw = ip[2];

   /*  Create a new output image by copying the RAW input image. */
   imgOut( "raw", "proc", &ipproc, istat );

   /*  Debias and flatfield data. */
   for( i=0; i <nx*ny; i++ ) {
      *ipproc++ = ( *ipraw++ - *ipbias++ ) / *ipflat++;
   }

   /*  Free all the images.*/
   imgFree( "*", istat );
}

/* $Id$ */
