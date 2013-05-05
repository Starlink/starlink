
/*
gcc -pedantic -Wall -std=c99 -O2 -o csofit2_test csofit2_test.c csofit2.c
*/

/* needed for -std=c99 */
#define _XOPEN_SOURCE

#include "csofit2.h"
#include <stdio.h>
#include <stdlib.h>

#include <time.h>

int main(int argc, char * argv[])
{
    const char * fname = "/jcmtdata/raw/wvm/csofit2.dat";
    if(argc > 1)
    {
        fname = argv[1];
    }
    csofit2_t * fits = csofit2_open(fname);
    fprintf(stderr, "%d fits in data file %s.\n", fits->npolys, fname);
    struct tm utc0, utc1;
    //strptime("2013-04-02T12:27:14", "%Y-%m-%dT%H:%M:%S", &utc0);
    //strptime("2013-04-03T06:56:55", "%Y-%m-%dT%H:%M:%S", &utc1);
    strptime("2013-03-29T12:27:14", "%Y-%m-%dT%H:%M:%S", &utc0);
    strptime("2013-03-31T12:56:55", "%Y-%m-%dT%H:%M:%S", &utc1);
    double t0 = timegm(&utc0);
    double t1 = timegm(&utc1);
    csofit2_t * subset = csofit2_subset(fits, t0, t1);
    free(fits);
    fprintf(stderr, "%d fits in subset.\n", subset->npolys);
    t0 -= 600.0;
    t1 += 600.0;
    double t;
    for(t = t0; t < t1; t += 60.0)
    {
        double tau = csofit2_calc(subset, t);
        printf("%g %g\n", t-t0, tau);
    }
    free(subset);
    fprintf(stderr, "done.\n");
    return 0;
}
