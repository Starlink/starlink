// -*-c++-*-
#ifndef _define_h_
#define _define_h_
/*
 *
 * E.S.O. - VLT project
 *
 * $Id: define.h,v 1.1 1997/11/28 01:38:53 abrighto Exp $
 *
 * define.h - common definitions
 * 
 * who             when      what
 * --------------  --------  ----------------------------------------
 * Allan Brighton  05/10/95  Created
 */


// min/max
inline int min(int x, int y) {return x<y ? x : y;}
inline int max(int x, int y) {return x>y ? x : y;}
inline double min(double x, double y) {return x<y ? x : y;}
inline double max(double x, double y) {return x>y ? x : y;}

// swap values
inline void swap(int& x, int& y) {int tmp = x; x = y; y = tmp;}
inline void swap(double& x, double& y) {double tmp = x; x = y; y = tmp;}

// inline int roundup(int nbytes, int pad) {return ((nbytes + (pad - 1)) / pad) * pad;}

/*
 * time a function call
 * usage: TIMECALL("name", function(args,...));
 */
#ifndef DEBUG
#define TIMECALL(name,func) func
#else
#include <stdio.h>
#include <sys/time.h>
#include <sys/timeb.h>
extern "C" int ftime(timeb *tp);
#define TIMECALL(name,func) {\
     timeb tp1,tp2; \
     ftime(&tp1); \
     func; \
     ftime(&tp2); \
     fprintf(stderr, "time %s: %d\n", name, (tp2.time-tp1.time)*1000+(tp2.millitm-tp1.millitm)); \
}
#endif

#endif /* _define_h_ */
