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
   double cosd(const double);
   double sind(const double);
   double tand(const double);
   double acosd(const double);
   double asind(const double);
   double atand(const double);
   double atan2d(const double, const double);
#else
   double cosd();
   double sind();
   double tand();
   double acosd();
   double asind();
   double atand();
   double atan2d();
#endif

/* Domain tolerance for asin and acos functions. */
#define WCSTRIG_TOL 1e-10
#endif /* TRIGD */

#ifdef __cplusplus
};
#endif

#endif /* WCSTRIG */
