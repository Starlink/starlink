/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995, Mark Calabretta
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
*  This version of wcstrig.c is based on the version in wcslib-2.2, but has
*  been modified in the following ways by the Starlink project (e-mail: 
*  ussc@star.rl.ac.uk):
*     -  Support for non-ANSI C "const" class removed
*        (D.S. Berry, 19th June 1996).
*     -  Undefine PI, if previously defined
*        (R.F. Warren-Smith, 15th November 1996).
*
*=============================================================================
*
*   The functions defined herein are trigonometric or inverse trigonometric
*   functions which take or return angular arguments in decimal degrees.
*---------------------------------------------------------------------------*/

#include "wcstrig.h"

#if defined(PI)
#undef PI
#endif
#define PI 3.141592653589793238462643
const double d2r = PI / 180.0;
const double r2d = 180.0 / PI;

double cosd(angle)

double angle;

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

double sind(angle)

double angle;

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

double tand(angle)

double angle;

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

double acosd(v)

double v;

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

double asind(v)

double v;

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

double atand(v)

double v;

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

double atan2d(y, x)

double x, y;

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
