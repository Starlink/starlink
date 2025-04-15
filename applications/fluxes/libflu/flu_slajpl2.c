/*+
*     Approximate geocentric apparent RA,Dec of a planet.
*     Does not cater for the moon.

*  Given:
*     DATE        d       MJD of observation (JD - 2400000.5)
*     NP          i       planet: 1 = Mercury
*                                 2 = Venus
*                                 4 = Mars
*                                 5 = Jupiter
*                                 6 = Saturn
*                                 7 = Uranus
*                                 8 = Neptune
*                                 9 = Pluto
*                                11 = Sun
*     ELONG       d       Longtitude

*  Returned:
*     RA,DEC      d       RA, Dec (geocentric apparent, radians)
*     R           d       geocentric distance to the planet/moon/sun

*  Notes:

*     1  The date is in a dynamical timescale (TDB, formerly ET) and is
*     in the form of a Modified Julian Date (JD-2400000.5).  For all
*     practical purposes, TT can be used instead of TDB, and for many
*     applications UT will do (except for the Moon).

*     Called: sla_GMST, sla_DT, sla_EPJ, sla_PRENUT,
*     sla_DMXV, sla_DCC2S, sla_DRANRM, pleph

*     Based on sla_rdplan             P.T.Wallace  Starlink 30 November 1994
*     Modified to use JPL ephemeris   G.J.Privett  Starlink 02 September 1996

*     Copyright (C) 1995 Rutherford Appleton Laboratory
*-
*/

#include <math.h>

#include "star/pal.h"

#include "flu.h"

void flu_slajpl2(double date, int np, double elong,
        double* ra, double* dec, double* r) {
    /* Variables */
    int i, ok;
    double stl, dx, dy, dz, tl;
    double v[6];
    double vgm[6];
    double vse[6];
    double vsg[6];
    double vsp[6];
    double rmat[3][3];

    /* Approximate local ST */
    stl = flu_slalast(date, elong);

    /* Geocentric Moon position. */
    palDmoon(date, v);

    /* Create nutation matrix. */
    palNut(date, rmat);

    /* Multiply a 3D vector by a rotation matrix. */
    palDmxv(rmat, v, vgm);
    palDmxv(rmat, v + 3, vgm + 3);

    /* No: precession/nutation matrix, J2000 to date */
    palPrenut(2000.0, date, rmat);

    /* Sun to Earth-Moon Barycentre (J2000). Heliocentric position. */
    palPlanet(date, 3, v, &ok);

    /* Precession and nutation to date */
    palDmxv(rmat, v, vse);
    palDmxv(rmat, v + 3, vse + 3);

    /* Sun to geocentre */
    for (i = 0; i < 6; i ++) {
        vsg[i] = vse[i] - 0.012150581 * vgm[i];
    }

    /* Sun? */
    if (np == 11) {
        /* Yes: geocentre to Sun */
        for (i = 0; i < 6; i ++) {
            v[i] = - vsg[i];
        }
    }
    else {
        /* No: Sun to Planet. Heliocentric position. */
        palPlanet(date, np, v, &ok);

        /* Precession and nutation to date */
        palDmxv(rmat, v, vsp);
        palDmxv(rmat, v + 3, vsp + 3);

        /* Geocentre to planet */
        for (i = 0; i < 6; i ++) {
            v[i] = vsp[i] - vsg[i];
        }
    }

    /* Geometric distance (AU) */
    dx = v[0];
    dy = v[1];
    dz = v[2];
    *r = sqrt(dx * dx + dy * dy + dz * dz);

    /* Light time (sec) */
    tl = 499.004782 * *r;

    /* Correct position for planetary aberration */
    for (i = 0; i < 3; i ++) {
        v[i] = v[i] - tl * v[i + 3];
    }

    /* To RA,Dec */
    palDcc2s(v, ra, dec);
    *ra = palDranrm(*ra);
}
