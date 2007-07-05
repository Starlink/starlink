/*
*+
*  Name:
*     sc2sim_getbilinear

*  Purpose:
*     Bilinear interpolation on an image

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     sc2sim_getbilinear ( double xpos, double ypos, double scale, int size,
*                          double *image, double *value, int *status )

*  Arguments:
*     xpos = double (Given)
*        X-coordinate of sample point in arcsec
*     ypos = double (Given)
*        Y-coordinate of sample point in arcsec
*     scale = double (Given)
*        Scale of image in arcsec per pixel
*     size = int (Given)
*        Size of image
*     image = double* (Given)
*        Astronomical image
*     value = double* (Returned)
*        Value sampled from image
*     status = int* (Given and Returned)
*        Pointer to global status.  

*  Description:
*     Interpolate a value at the given position in an image.
*     Use bilinear interpolation.

*  Authors:
*     B.D.Kelly (ROE)
*     {enter_new_authors_here}

*  History :
*     2001-07-19 (BDK):
*        Original
*     2002-08-10 (BDK)
*        C version
*     2005-07-11 (BDK):
*        Trap out-of-range indices
*     2006-07-20 (JB):
*        Split from dsim.c
*     2007-04-04 (AGG):
*        Wrap out-of-range indices
*     2007-07-05 (AGG):
*        Allow for multiple wraps
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2005-2007 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"

/* SC2SIM includes */
#include "sc2sim.h"

#define FUNC_NAME "sc2sim_getbilinear"

void sc2sim_getbilinear 
( 
double xpos,         /* X-coordinate of sample point in arcsec (given) */
double ypos,         /* Y-coordinate of sample point in arcsec (given) */
double scale,        /* scale of image in arcsec per pixel (given) */
int size,            /* size of image (given) */
double *image,       /* astronomical image (given) */
double *value,       /* value sampled from image (returned) */
int *status          /* global status (given and returned) */
)

{
   /* Local variables */
   double a;             /* image value near point */
   double b;             /* image value near point */
   double c;             /* image value near point */
   double d;             /* image value near point */
   double dx;            /* fractional pixel offset */
   double dy;            /* fractional pixel offset */
   int ixpix;            /* truncated pixel position */
   int iypix;            /* truncated pixel position */
   int nwraps;           /* Multiplier to indicate how many times the
			    atm image is wrapped */
   double xpix;          /* pixel position */
   double ypix;          /* pixel position */

   /* Check status */
   if ( !StatusOkP(status) ) return;

   xpix = xpos / scale;
   ypix = ypos / scale;
   ixpix = (int)xpix;
   iypix = (int)ypix;

   /* Since ATM image has periodic boundary conditions, we can just
      return to the lower edge of the image if we go outside */
   if ( ixpix > size ) {
     nwraps = ixpix / size;
     ixpix -= (nwraps*(size) - 1);
   }
   if ( iypix > size ) {
     nwraps = iypix / size;
     iypix -=  (nwraps*(size) - 1);
   }

   if ( ( ixpix > 0 ) && ( ixpix <= size ) &&
        ( iypix > 0 ) && ( iypix <= size ) ) {
      a = image [ ixpix + size*iypix ];
      b = image [ ixpix+1 + size*iypix ];
      c = image [ ixpix + size*(iypix+1) ];
      d = image [ ixpix+1 + size*(iypix+1) ];
      dx = xpix - (double)ixpix;
      dy = ypix - (double)iypix;

      *value = a * ( 1.0 - dy - dx + dy * dx )
             + b * ( dx - dy * dx )
             + c * ( dy - dy * dx )
             + d * dy * dx;
   } else {
      *status = SAI__ERROR;
      msgSeti("XPIX", ixpix);
      msgSeti("YPIX", iypix);
      errRep( FUNC_NAME, "Data point outside image ^XPIX ^YPIX", status);
   }

}
