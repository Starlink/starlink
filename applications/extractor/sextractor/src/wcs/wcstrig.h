#ifndef WCSTRIG
#define WCSTRIG

#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

#if !defined(__STDC__) && !defined(__cplusplus)
#ifndef const
#define const
#endif
#endif

#ifdef TRIGD
#include <TRIGD>
#else /* not TRIGD */
#if __STDC__ || defined(__cplusplus)
   double wcs_cosd(const double);
   double wcs_sind(const double);
   double wcs_tand(const double);
   double wcs_acosd(const double);
   double wcs_asind(const double);
   double wcs_atand(const double);
   double wcs_atan2d(const double, const double);
#else
   double wcs_cosd();
   double wcs_sind();
   double wcs_tand();
   double wcs_acosd();
   double wcs_asind();
   double wcs_atand();
   double wcs_atan2d();
#endif

/* Domain tolerance for asin and acos functions. */
#define WCSTRIG_TOL 1e-10
#endif /* TRIGD */

#ifdef __cplusplus
};
#endif

#endif /* WCSTRIG */
