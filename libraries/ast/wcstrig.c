/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995-2002, Mark Calabretta
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
*   Inc., 51 Franklin Street,Fifth Floor, Boston, MA 02110-1301, USA
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
*  This version of wcstrig.c is based on the version in wcslib-2.9, but has
*  been modified in the following ways by the Starlink project (e-mail:
*  ussc@star.rl.ac.uk):
*     -  Support for non-ANSI C "const" class removed
*     -  Changed names of projection functions and degrees trig functions
*        to avoid clashes with wcslib.
*=============================================================================
*
*   The functions defined herein are trigonometric or inverse trigonometric
*   functions which take or return angular arguments in decimal degrees.
*
*   $Id$
*---------------------------------------------------------------------------*/

#include <math.h>
#include "wcsmath.h"
#include "wcstrig.h"

double astCosd(angle)

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

   return cos(angle*D2R);
}

/*--------------------------------------------------------------------------*/

double astSind(angle)

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

   return sin(angle*D2R);
}

/*--------------------------------------------------------------------------*/

double astTand(angle)

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

   return tan(angle*D2R);
}

/*--------------------------------------------------------------------------*/

double astACosd(v)

const double v;

{
   if (v >= 1.0) {
      if (v-1.0 <  WCSTRIG_TOL) return 0.0;
   } else if (v == 0.0) {
      return 90.0;
   } else if (v <= -1.0) {
      if (v+1.0 > -WCSTRIG_TOL) return 180.0;
   }

   return acos(v)*R2D;
}

/*--------------------------------------------------------------------------*/

double astASind(v)

const double v;

{
   if (v <= -1.0) {
      if (v+1.0 > -WCSTRIG_TOL) return -90.0;
   } else if (v == 0.0) {
      return 0.0;
   } else if (v >= 1.0) {
      if (v-1.0 <  WCSTRIG_TOL) return 90.0;
   }

   return asin(v)*R2D;
}

/*--------------------------------------------------------------------------*/

double astATand(v)

const double v;

{
   if (v == -1.0) {
      return -45.0;
   } else if (v == 0.0) {
      return 0.0;
   } else if (v == 1.0) {
      return 45.0;
   }

   return atan(v)*R2D;
}

/*--------------------------------------------------------------------------*/

double astATan2d(y, x)

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

   return atan2(y,x)*R2D;
}
