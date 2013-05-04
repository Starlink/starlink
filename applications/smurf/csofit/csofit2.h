/*
 * csofit2.h
 * Ryan Berthold, 2 May 2013
 *
 * Process csofit2 data file and calculate fit results.
 * Usage:
 *
 * #include <csofit2.h>
 *
 * csofit2_t * fits = csofit2_open("/jcmtdata/raw/wvm/csofit2.dat");
 * double t1 = time(0);
 * double t0 = t1 - 86400;
 * csofit2_t * subset = csofit2_subset(fits, t0, t1);
 * double tau = csofit2_calc(subset, t0 + 43200.555);
 * free(subset);
 * free(fits);
 *
 * NOTE: Times are Posix, seconds since 00 UTC 1 Jan 1970 (unix timestamps).
 *       Ignores leap seconds and makes other stupid assumptions.
 *       Should probably change to use TAI or similar.
 */

#ifndef CSOFIT2_H
#define CSOFIT2_H

typedef struct csofit2_poly_t {
    double start, end;
    int deg;
    double coefs[20];
} csofit2_poly_t;

typedef struct csofit2_t {
    int npolys;
    csofit2_poly_t polys[1];  /* variable length; [1] to avoid warnings. */
} csofit2_t;

csofit2_t * csofit2_open(const char * filename);
csofit2_t * csofit2_subset(csofit2_t * fits, double t0, double t1);
double csofit2_calc(csofit2_t * fits, double t);

#endif /* CSOFIT2_H */

