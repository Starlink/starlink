/*
*  Purpose:
*     Calculate integrated flux densities of planet at earth
*     and beam-corrected flux densities of planet at earth
*     for NF frequencies.
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "erfam.h"
#include "merswrap.h"
#include "parwrap.h"
#include "sae_par.h"
#include "star/one.h"

#include "flu.h"

void flu_pbflux(double omega, double freq[][2], double tb[],
        double hpbw[], double hpbw2[], double amp1[], double amp2[],
        int nf, char fname[][FLU_FILTLEN], double er[], char* filter, char* body,
        int screen, FILE* fiod,
        int* status) {
    /* Variables */
    double denom, ratio;
    double flux[FLU_MAXFILT];
    double fluxbc[FLU_MAXFILT];
    double pdiam;             /* Planet diameter in arcsec */
    double ln2;               /* LN(2) */

    int i, k, np, junk;
    char* string1;
    char string2[81];

    /* Check the inherited global status. */
    if (*status != SAI__OK) return;

    np = nf;

    /* The multi-beam formula is written in terms of arcsec values
     * so convert omega back to a diameter in ARCSEC. */
    pdiam = 2.0 * ERFA_DR2AS * sqrt(omega / ERFA_DPI);
    ln2 = log(2.0);

    for (i = 0; i < np; i ++) {
        denom = exp(0.04799 * freq[i][0] / tb[i]) - 1.0;
        flux[i] = 1.475e3 * omega * pow(freq[i][0], 3) / denom;

        ratio = (1/ln2) * (
          amp1[i] * pow(hpbw[i] / pdiam, 2) * (1.0 - exp(- ln2 * pow(pdiam / hpbw[i], 2))) +
          amp2[i] * pow(hpbw2[i] / pdiam, 2) * (1.0 - exp(- ln2 * pow(pdiam / hpbw2[i], 2))));

        fluxbc[i] = ratio * flux[i];
    }

    /* Output integrated and beam-corrected flux densities for six
     * frequencies */

    char* head1 = "Filter    Centre   Filter   Total    Flux in    Brightness         HPBW";
    char* head2 = "Wavel.     Freq     Width    Flux     beam      Temperature       assumed";
    char* head3 = "micron     (GHz)    (GHz)    (Jy)      (Jy)         (K)          (arcsecs)";

    /* Screen. */
    if (screen) {
        msgOut("", head1, status);
        msgOut("", head2, status);
        msgOut("", head3, status);
    }

    /* File. */
    if (fiod) {
        fprintf(fiod, "%s\n", head1);
        fprintf(fiod, "%s\n", head2);
        fprintf(fiod, "%s\n", head3);
    }

    parPut0r("F_CENTRE", -1.0, status);
    parPut0r("F_WIDTH", -1.0, status);
    parPut0r("F_TOTAL", -1.0, status);
    parPut0r("F_BEAM",  -1.0, status);
    parPut0r("T_BRIGHT", -1.0, status);
    parPut0r("T_ERROR", -1.0, status);
    parPut0r("HPBW", -1.0, status);

    /* Suppress the 600 micron (np=6) output for Jupiter (j=5) and Saturn (j=6) */
    for (i = 0; i < np; i ++) {
        if ((! strcmp(filter, "ALL")) || (! strncasecmp(fname[i], filter, 4))) {
            if (((! strncmp(body, "JUP", 3)) || (! strncmp(body, "SAT", 3)))
                     && (! strncmp(fname[i], "600", 3))) {
                one_strlcpy(string2, "  600     no flux", sizeof(string2), status);
            }
            else {
                /* Create output string. */
                if (! strcmp(fname[i], "CUSTOM")) {
                    k = -1;
                }
                else {
                    k = atoi(fname[i]);
                }

                if (strcmp(filter, "ALL")) {
                    parPut0d("F_CENTRE", freq[i][0], status);
                    parPut0d("F_WIDTH", freq[i][1], status);
                    parPut0d("F_TOTAL", flux[i], status);
                    parPut0d("F_BEAM", fluxbc[i], status);
                    parPut0d("T_BRIGHT", tb[i], status);
                    parPut0d("T_ERROR", er[i], status);
                    parPut0d("HPBW", hpbw[i], status);
                }

                if (k == -1) {
                    msgSetc("P1", "custom");
                }
                else {
                    msgFmt("P1", "%4i", k);
                }
                msgFmt("P2", "%6.1f", freq[i][0]);
                msgFmt("P3", "%5.1f", freq[i][1]);
                msgFmt("P4", "%9.2f", flux[i]);
                msgFmt("P5", "%9.2f", fluxbc[i]);
                msgFmt("P6", "%5.1f", tb[i]);
                msgFmt("P7", "%5.1f", er[i]);
                msgFmt("P8", "%10.1f", hpbw[i]);
                string1 = " ^P1     ^P2   ^P3 ^P4 ^P5    ^P6 +-^P7 ^P8";
                msgLoad("", string1, string2, sizeof(string2), &junk, status);
            }

            /* Write the output. */
            if (screen) msgOut("", string2, status);
            if (fiod) fprintf(fiod, "%s\n", string2);
        }
    }
}
