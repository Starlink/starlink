/*
*  Purpose:
*     Calculate solid angle subtended by planet at Earth.

*     Variable Type  Function
*     J        I    Planet code:  Mars J=4  Jupiter J=5  Saturn J=6
*     Uranus J=7  Neptune J=8
*     RJD      D    Julian date
*     RA       D    Apparent Geocentric Right Ascension of the planet in degrees
*     DEC      D    Apparent Geocentric Declination of the planet in degrees
*     GD       D    True Geocentric Distance in au
*
*   Returned Value:
*     OMEGA    D
*/

#include <math.h>
#include <stdio.h>

#include "erfam.h"
#include "merswrap.h"
#include "parwrap.h"
#include "sae_par.h"

#include "flu.h"

/* Data (Only array items for Mars thru Neptune) */
static const double req[] = {
        0.0,
        0.0,
        0.0,
        3397.0,
        71495.0,
        60233.0,
        25563.0,
        24760.0,
        0.0,
        0.0,
};

static const double epsln[] = {
        0.0,
        0.0,
        0.0,
        0.005,
        0.065,
        0.096,
        0.024,
        0.021,
        0.0,
        0.0,
};

double flu_solidangle(
        int j, double rjd, double ra, double dec, double gd,
        int screen, FILE* fiod,
        int* status) {
    /* Variables */
    double appdec, appra, bc, de, de1, de2, decnpj,
            decp, gm, omega, p, p1, ranpj, rap, sd, sdrad;
    int junk;
    char* string1;
    char string2[81];

    /* Check the inherited global status. */
    if (*status != SAI__OK) return 0.0;

    /* Only implemented at present for Mars through Neptune */
    if (j < 4 || j > 8) return 0.0;

    /* Set up arrays of equatorial radii and flattening
     * See: Astronomical Almanac (1985, E88) and Hildebrand's work
     * Convert apparent RA and DEC to radians */
    appra = ra * ERFA_DD2R;
    appdec = dec * ERFA_DD2R;

    /* Calculate the RA and Dec of the Pole of the planet and convert to radians */
    flu_poleplan(j, rjd + ERFA_DJM0, &rap, &decp);
    ranpj = rap * ERFA_DD2R;
    decnpj = decp * ERFA_DD2R;

    /* Calculate planetocentric declination of Earth */
    de1 = -sin(decnpj) * sin(appdec);
    de2 = -cos(decnpj) * cos(appdec) * cos(ranpj - appra);
    de = asin(de1 + de2);

    /* Calculate which pole is Earth-facing and polar inclination angle
     * of planet. Output Earth-facing pole and convert polar inclination
     * angle to degrees and output */
    if (de <= 0.0) {
        /* Calc inclination. */
         p = de + 1.5708;
         p1 = p * ERFA_DR2D;

        /* Create output string. */
        msgFmt("P1", "%6.2f", p1);
        string1 = "North pole is Earth-facing; Inclination Angle = ^P1 degrees";
        msgLoad("", string1, string2, sizeof(string2), &junk, status);

        /* Output to file and screen. */
        if (screen) msgOut("", string2, status);
        if (fiod) fprintf(fiod, "%s\n", string2);
    }
    else {
        /* Calc inclination. */
        p = 1.5708 - de;
        p1 = p * ERFA_DR2D;

        /* Create output string. */
        msgFmt("P1", "%6.2f", p1);
        string1 = "South pole is Earth-facing; Inclination Angle = ^P1 degrees";
        msgLoad("", string1, string2, sizeof(string2), &junk, status);

        /* Output to file and screen. */
        if (screen) msgOut("", string2, status);
        if (fiod) fprintf(fiod, "%s\n", string2);
    }

    /* Viewing-modifies semi-major axis of planet */
    bc = (1.0 - epsln[j - 1]) * req[j - 1] / (1.0 - epsln[j - 1] * cos(p));

    /* Geometrical mean radius of planet */
    gm = sqrt(req[j - 1] * bc);

    /* Semi-diameter of planet */
    sdrad = gm / (gd * 1.49598e8);
    sd = sdrad * 3600.0 * ERFA_DR2D;

    /* Solid Angle subtended at the planet by the Earth */
    omega = ERFA_DPI * pow(sdrad, 2);

    /* Create output string. */
    msgFmt("P1", "%5.2f", sd);
    msgFmt("P2", "%9.2E", omega);
    string1 = "Semi-diameter = ^P1 arcsecs    Solid angle = ^P2 sterads";
    msgLoad("", string1, string2, sizeof(string2), &junk, status);

    /* Store parameter values */
    parPut0d("SOLID_ANG", omega, status);
    parPut0d("SEMI_DIAM", sd, status);

    /* Output to file and screen. */
    if (screen) msgOut("", string2, status);
    if (fiod) fprintf(fiod, "%s\n", string2);

    return omega;
}
