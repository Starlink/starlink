/*
*  Purpose:
*     Uses PAL to create an approximate
*     topocentric ephemeris for the planets, moon and sun.

*     GJP Starlink 07 September 1996
*/

#include <string.h>
#include <stdio.h>

#include "erfam.h"
#include "merswrap.h"
#include "sae_par.h"
#include "star/one.h"
#include "star/pal.h"

#include "flu.h"

void flu_topeph(
        char* reqbody, double rjd, FluDateTime* time, FluTelescope* telescope,
        int pos, int screen, FILE* fiod,
        int* status) {
    /* Variables */
    int i, junk, np, tempi;
    size_t len;
    char string1[81];
    char string2[81];
    const char* head1;
    const char* head2;
    char sign;
    double tdb, tdb2, rav1, rav2, decv1, decv2, dist, dlen3, dlen4,
            frac1, frac2, stl, airm, epoch;
    int ivr1[4];
    int ivr2[4];

    int all = 0;
    int ir = 0;
    int iq = 0;

    /* Check the inherited global status. */
    if (*status != SAI__OK) return;

    /* Set up requested Planets */
    len = strlen(reqbody);
    if ((! strcmp(reqbody, "ALL"))
            || (! strcmp(reqbody, "all"))
            || (! strcmp(reqbody, ""))) {
        all = 1;
        iq = 1;
        ir = 10;
    }
    else {
        for (i = 0; i < FLU_NPLANET; i ++) {
            if (! strncmp(planet[i], reqbody, len)) {
                iq = i + 1;
                ir = i + 1;
            }
        }
    }

    /* Headings. */
    if (all || (iq >= 4 && iq <= 8)) {
        head1 = "Planetary Submillimetre Fluxes for the JCMT";
    }
    else if (pos) {
        head1 = "Positional data only for this Planet";
    }
    else {
        head1 = "Date information only for this Planet";
    }

    /* Create substrings. */
    tempi = time->ih - 10;
    if (tempi < 0) tempi += 24;
    msgFmt("P1", "%2.2d", time->id);
    msgFmt("P2", "%s", time->cmon);
    msgFmt("P3", "%4d", time->iy);
    msgFmt("P4", "%2.2d", time->ih);
    msgFmt("P5", "%2.2d", time->im);
    msgFmt("P6", "%2.2d", time->is);
    msgFmt("P7", "%2d", tempi);

    /* Create header string. */
    msgLoad("",
            "UT:  ^P4:^P5:^P6       UT Date:^P1-^P2-^P3   HST:^P7:^P5:^P6",
            string2, sizeof(string2), &junk, status);

    /* Display. */

    if (screen) {
        msgBlank(status);
        msgOut("", head1, status);
        msgBlank(status);
        msgOut("", string2, status);
        msgBlank(status);
    }

    /* File. */
    if (fiod) {
        fprintf(fiod, "\n");
        fprintf(fiod, "%s\n", head1);
        fprintf(fiod, "\n");
        fprintf(fiod, "%s\n", string2);
        fprintf(fiod, "\n");
    }

    /* Calculate the modified time.[RJD now is the MJD] */
    tdb = rjd;
    /* And a time 1 second into the future */
    tdb2 = tdb + 1.0 / 86400.0;

    /* Current epoch. */
    epoch = palEpj(tdb);

    /* Call the slalib jpl routines to get the local sid. time. */
    palDr2tf(4, flu_slalast(tdb, telescope->lon), &sign, ivr1);

    /* Create output strings. */
    msgFmt("P1", "%2.2d", ivr1[0]);
    msgFmt("P2", "%2.2d", ivr1[1]);
    frac1 = ivr1[2] + (ivr1[3] / 10000.0);
    msgFmt("P3", "%7.4f", frac1);
    msgFmt("P4", "%12.3f", rjd);
    msgFmt("P5", "%9.4f", epoch);
    msgLoad("",
        "LST: ^P1:^P2:^P3  MJD (TT): ^P4  Epoch: ^P5",
        string2, sizeof(string2), &junk, status);

    /* Create display / file headings. */
    if (screen) {
        msgOut("", string2, status);
        msgBlank(status);
    }

    if (fiod) {
        fprintf(fiod, "%s\n", string2);
        fprintf(fiod, "\n");
    }

    /* Return after date info if positional data not required. */
    if (! pos) return;

    /* Header strings. */
    head1 = "Body         RA        TRIMRA         Dec       TRIMDEC        GD     AMASS";
    head2 = "           (h m s)  (arcsec/sec)    (d m s)  (arcsec/sec)     (au)";

    /* Create display / file headings. */
    if (screen) {
        msgOut("", head1, status);
        msgOut("", head2, status);
    }

    if (fiod) {
        fprintf(fiod, "%s\n", head1);
        fprintf(fiod, "%s\n", head2);
    }

    /* Loop through all objects. */
    for (i = iq - 1; i < ir; i ++) {
        /* Ensure that values come out in expected order. */
        np = lup[i];

        if (np == 9) {
            /* Pluto not supported by PAL. */
            continue;
        }

        /* Call the slalib jpl routines. */
        flu_slajpl(tdb, np, telescope->lon, telescope->lat, telescope->height,
                &rav1, &decv1, &dist, &stl, &airm);

        /* Convert radians to hh mm ss and dd mm ss. */
        palDr2tf(4, rav1, &sign, ivr1);
        palDr2af(4, decv1, &sign, ivr2);

        /* Convert secs and fractional secs to a double precision for printing. */
        frac1 = ivr1[2] + (ivr1[3] / 10000.0);
        frac2 = ivr2[2] + (ivr2[3] / 10000.0);

        /* Set up parameter values to display. */
        msgFmt("P1", "%2d", ivr1[0]);
        msgFmt("P2", "%2d", ivr1[1]);
        msgFmt("P3", "%7.4f", frac1);
        msgFmt("P10", "%c", sign);
        msgFmt("P4", "%2d", ivr2[0]);
        msgFmt("P5", "%2d", ivr2[1]);
        msgFmt("P6", "%7.4f", frac2);
        msgFmt("P7", "%9.6f", dist);
        msgFmt("P11", "%6.3f", airm);

        /* Recalc positions 1 sec later and deduce movement rate. */
        flu_slajpl(tdb2, np, telescope->lon, telescope->lat, telescope->height,
                &rav2, &decv2, &dist, &stl, &airm);
        dlen3 = (rav2 - rav1) * ERFA_DR2AS;
        dlen4 = (decv2 - decv1) * ERFA_DR2AS;

        /* Set up parameter values to display. */
        msgFmt("P8", "%7.4f", dlen3);
        msgFmt("P9", "%7.4f", dlen4);

        /* Expand string. */
        one_snprintf(string1, sizeof(string1),
                "%-7s ^P1 ^P2 ^P3 ^P8   ^P10^P4 ^P5 ^P6  ^P9   ^P7  ^P11",
                status, planet[i]);
        msgLoad("", string1, string2, sizeof(string2), &junk, status);

        /* Place output on the screen or in the file. */
        if (pos) {
            if (screen) msgOut("", string2, status);
            if (fiod) fprintf(fiod, "%s\n", string2);
        }
    }
}
