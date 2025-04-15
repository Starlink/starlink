/*
*  Purpose:
*     Right Ascension and Declination of the Pole of the Planets
*     for Mean Equinox and Equator of date.

*     See: Report of the IAU working group on cartographic coordinates
*     and rotational elements of the planets and satellites (1982)

*     Variable Type  Function
*     IB       I    Planet code: Sun IB=0, Mercury IB=1, Venus   IB=2,
*     Earth IB=3, Mars    IB=4, Jupiter IB=5,
*     Saturn IB=6, Uranus  IB=7, Neptune IB=8,
*     Pluto IB=9
*     AJD      D    Julian date
*     RAP      D    Right ascension of the pole of the planet for date (degrees)
*     DECP     D    Declination of the pole of the planet for date (degrees)
*/

#include <math.h>

#include "erfam.h"

#include "flu.h"

/* Data */
double pc[6][3] = {
    {2306.2181,   2306.2181,   2004.3109},
    {   1.39656,     1.39656,    -0.85330},
    {  -0.000139,   -0.000139,   -0.000217},
    {   0.30188,     1.09468,    -0.42665},
    {  -0.000344,    0.000066,   -0.000217},
    {   0.017998,    0.018203,   -0.041833}};

/* RA0, DEC0 are the standard equatorial coordinates with equinox J2000,
 * together with their rate. See IAU report Table 3 */

double ra0[10][2] = {
        {285.96,   0.0},
        {281.02,  -0.033},
        { 272.78,   0.0},
        {   0.00,    -0.641},
        {317.681, -0.108},
        {268.05,  -0.009},
        { 40.66,   -0.036},
        {257.43,   0.0},
        {295.33,   0.0},
        {311.63,   0.0}};
double dec0[10][2] = {
        { 63.96,   0.0},
        { 61.45,  -0.005},
        { 67.21,   0.0},
        { 90.00,  -0.557},
        { 52.886, -0.061},
        { 64.49,   0.003},
        { 83.52,  -0.004},
        {-15.10,   0.0},
        { 40.65,   0.0},
        {  4.18,   0.0}};

void flu_poleplan(int ib, double ajd, double* rap, double* decp) {
    /* Variables */
    double az, c, ca, cd, ct, dec, ra,
            s, sa, sd, sd1, st, t, t1, th, xi, z;

    const double deg = 1.0 / 3600.0;

    t = (ajd - ERFA_DJ00) / ERFA_DJC;
    ra =   ra0[ib][0] + t * ra0[ib][1];
    dec = dec0[ib][0] + t * dec0[ib][1];

    /* Compute precession angles zeta, z and theta to precess
     * data from J2000.0 (2451545.0) to date (ajd).
     * XI, Z, TH are equatorial precession parameters (in degrees) using
     * constants defined by IAU (1978)
     * These equations are from Astron.Astrophys., 73, 282-284, (1979) by
     * J H Liesle. */

    // NOTE: this expresson is ERFA_DJ00 - ERFA_DJ00, i.e. always 0?
    t1 = (ERFA_DJ00 - 2451545.0) / ERFA_DJC;

    xi = (pc[0][0] + pc[1][0] * t1 + pc[2][0] * t1 * t1) * t
           + (pc[3][0] + pc[4][0] * t1) * t * t
           + pc[5][0] * t * t * t;
    xi *= deg;

    z = (pc[0][1] + pc[1][1] * t1 + pc[2][1] * t1 * t1) * t
           + (pc[3][1] + pc[4][1] * t1) * t * t
           + pc[5][1] * t * t * t;
    z *= deg;

    th = (pc[0][2] + pc[1][2] * t1 + pc[2][2] * t1 * t1) * t
           + (pc[3][2] + pc[4][2] * t1) * t * t
           + pc[5][2] * t * t * t;
    th *= deg;

    cd = cos(dec * ERFA_DD2R);
    sd = sin(dec * ERFA_DD2R);
    az = ra + xi;
    ca = cos(az * ERFA_DD2R);
    sa = sin(az * ERFA_DD2R);
    ct = cos(th * ERFA_DD2R);
    st = sin(th * ERFA_DD2R);

    c = -sd * st + cd * ct * ca;
    s =  cd * sa;

    /* Right Ascension and declination of the Pole */

    *rap = z + atan2(s, c) * ERFA_DR2D;
    if (*rap < 0.0) *rap += 360.0;
    if (ib == 3) *rap = 0.0;

    sd1 = sd*ct + cd*st*ca;
    if (fabs(sd1) <= 0.99) {
        *decp = asin(sd1) * ERFA_DR2D;
    }
    else {
        *decp = acos(sqrt(s * s + c * c)) * ERFA_DR2D;
        if (sd1 < 0.0) *decp = - *decp;
    }
}
