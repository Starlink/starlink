#ifndef wcstrig_h_
#define wcstrig_h_

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
   double cosdeg(const double);
   double sindeg(const double);
   double tandeg(const double);
   double acosdeg(const double);
   double asindeg(const double);
   double atandeg(const double);
   double atan2deg(const double, const double);
#else
   double cosdeg();
   double sindeg();
   double tandeg();
   double acosdeg();
   double asindeg();
   double atandeg();
   double atan2deg();
#endif

/* Domain tolerance for asin and acos functions. */
#define WCSTRIG_TOL 1e-10
#endif /* TRIGD */

#ifdef __cplusplus
};
#endif

#endif /* wcstrig_h_ */
/* Apr 15 1998	"deg" added to function names by Doug Mink, SAO
 * May 27 1998	ifndef WCSTRIG changed to ifndef wcstrig_h_
 */
