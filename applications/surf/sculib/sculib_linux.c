/*
*  Name:
*     Linux compatibility routine for SECNDS 
*/


/* Only run if using g77 version less than 0.5.20 */
#ifdef NO_SECNDS
 
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include <stdio.h>
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#include <sys/types.h>
#include "f2c.h"
 
/* This is a VMS intrinsic. */
 
doublereal secnds_(real *r)
{
    struct tm *lt;
    time_t clock;
    float f;
 
    clock = time (NULL);
    lt = localtime (&clock);
    f= (3600.0*((real)lt->tm_hour) + 60.0*((real)lt->tm_min) +
            (real)lt->tm_sec - *r);
    return f;
}
 
#endif

/* Define some dummy function here so that the compilers
   dont complain about having no code to compile!! */

void mysecnds () {


}
