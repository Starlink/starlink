#ifndef WCSTRIG_INCLUDED
#define WCSTRIG_INCLUDED
/*
*=============================================================================
*
*  This version of wcstrig.h is based on the version in wcslib-2.2, but has
*  been modified in the following ways by the Starlink project (e-mail: 
*  ussc@star.rl.ac.uk):
*     -  Support for non-ANSI C prototypes removed
*        (D.S. Berry (19th June 1996).
*     -  Changed the name of the WCSTRIG macro to WCSTRIG_INCLUDED
*        (R.F. Warren-Smith (13th November 1996).
*
*=============================================================================
*/

#include <math.h>

   double cosd(double);
   double sind(double);
   double tand(double);
   double acosd(double);
   double asind(double);
   double atand(double);
   double atan2d(double, double);

/* Domain tolerance for asin and acos functions. */
#define WCSTRIG_TOL 1e-10

#endif /* WCSTRIG_INCLUDED */
