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
