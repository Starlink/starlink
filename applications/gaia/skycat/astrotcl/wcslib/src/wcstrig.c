/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995,1996 Mark Calabretta
*   Function names changed by Doug Mink, SAO, April 15, 1998
*   wcstrig.h include file added to wcslib.h by Doug Mink, SAO, September 28, 1998
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*=============================================================================
*
*   The functions defined herein are trigonometric or inverse trigonometric
*   functions which take or return angular arguments in decimal degrees.
*
*   $Id: wcstrig.c,v 1.6 1998/10/01 08:23:59 abrighto Exp $
*---------------------------------------------------------------------------*/

#include "wcslib.h"

#ifndef PI	/* often defined in math.h */
#define PI 3.141592653589793238462643
#endif

static const double d2r = PI / 180.0;
static const double r2d = 180.0 / PI;

double cosdeg (angle)

const double angle;

{
   double resid;

   resid = fabs(fmod(angle,360.0));
   if (resid == 0.0) {
      return 1.0;
   } else if (resid == 90.0) {
      return 0.0;
   } else if (resid == 180.0) {
      return -1.0;
   } else if (resid == 270.0) {
      return 0.0;
   }

   return cos(angle*d2r);
}

/*--------------------------------------------------------------------------*/

double sindeg (angle)

const double angle;

{
   double resid;

   resid = fmod(angle-90.0,360.0);
   if (resid == 0.0) {
      return 1.0;
   } else if (resid == 90.0) {
      return 0.0;
   } else if (resid == 180.0) {
      return -1.0;
   } else if (resid == 270.0) {
      return 0.0;
   }

   return sin(angle*d2r);
}

/*--------------------------------------------------------------------------*/

double tandeg (angle)

const double angle;

{
   double resid;

   resid = fmod(angle,360.0);
   if (resid == 0.0 || fabs(resid) == 180.0) {
      return 0.0;
   } else if (resid == 45.0 || resid == 225.0) {
      return 1.0;
   } else if (resid == -135.0 || resid == -315.0) {
      return -1.0;
   }

   return tan(angle*d2r);
}

/*--------------------------------------------------------------------------*/

double acosdeg (v)

const double v;

{
   if (v >= 1.0) {
      if (v-1.0 <  WCSTRIG_TOL) return 0.0;
   } else if (v == 0.0) {
      return 90.0;
   } else if (v <= -1.0) {
      if (v+1.0 > -WCSTRIG_TOL) return 180.0;
   }

   return acos(v)*r2d;
}

/*--------------------------------------------------------------------------*/

double asindeg (v)

const double v;

{
   if (v <= -1.0) {
      if (v+1.0 > -WCSTRIG_TOL) return -90.0;
   } else if (v == 0.0) {
      return 0.0;
   } else if (v >= 1.0) {
      if (v-1.0 <  WCSTRIG_TOL) return 90.0;
   }

   return asin(v)*r2d;
}

/*--------------------------------------------------------------------------*/

double atandeg (v)

const double v;

{
   if (v == -1.0) {
      return -45.0;
   } else if (v == 0.0) {
      return 0.0;
   } else if (v == 1.0) {
      return 45.0;
   }

   return atan(v)*r2d;
}

/*--------------------------------------------------------------------------*/

double atan2deg (y, x)

const double x, y;

{
   if (y == 0.0) {
      if (x >= 0.0) {
         return 0.0;
      } else if (x < 0.0) {
         return 180.0;
      }
   } else if (x == 0.0) {
      if (y > 0.0) {
         return 90.0;
      } else if (y < 0.0) {
         return -90.0;
      }
   }

   return atan2(y,x)*r2d;
}
/* Apr 30 1998	Define PI only if not defined already
 * Jul 30 1998	Make r2d and d2r static to avoid conflicts - Allan Brighton
 * Sep 28 1998	wcstrig.h now part of wcslib.h
 */
