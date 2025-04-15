/*
*+
*  Description:
*     Reads information from a fluxes data file.
*-
*/

#include <stdio.h>

#include "chr.h"
#include "merswrap.h"
#include "sae_par.h"

#include "flu.h"

void flu_read_data(
        char* pathname, char fname[][FLU_FILTLEN],
        int* nf, int* nb, double freq[][2],
        double hpbw[], double hpbw2[], double amp1[], double amp2[],
        double tbnorm[][FLU_MAXFILT], double error[][FLU_MAXFILT],
        size_t* inote, char note[][FLU_NOTELEN],
        int* status) {
    double frequency, width, bw, bw2, a1, a2;
    FILE* fiod2 = 0;
    char fplan[5][8];
    char filt[11];
    int i, j, k;

    if (*status != SAI__OK) return;

    *inote = 0;

    fiod2 = fopen(pathname, "r");
    if (! fiod2) {
        if (*status == SAI__OK) *status = SAI__ERROR;
        msgSetc("PATH", pathname);
        errRep("", "Error opening ^PATH", status);
        return;
    }

    fscanf(fiod2, " %*28c%d", nf);
    fscanf(fiod2, " %*34c%d", nb);

    if (*nb == 1) {
        fscanf(fiod2, "%*s %*s %*s %*s %*s");
    }
    else {
        fscanf(fiod2, "%*s %*s %*s %*s %*s %*s %*s %*s");
    }

    for (i = 0; i < FLU_MAXFILT; i ++) {
        if (*nb == 1) {
            fscanf(fiod2, "%d%*2[ ]%10c %lf %lf %lf",
                    &k, filt, &frequency, &width, &bw);
            bw2 = 0.0;
            a1 = 1.0;
            a2 = 0.0;
        }
        else {
            fscanf(fiod2, "%d%*2[ ]%10c %lf %lf %lf %lf %lf %lf",
                    &k, filt, &frequency, &width, &bw, &bw2, &a1, &a2);
        }
        filt[sizeof(filt) - 1] = '\0';
        chrRmblk(filt);
        strncpy(fname[i], filt, FLU_FILTLEN);
        freq[i][0] = frequency;
        freq[i][1] = width;
        hpbw[i] = bw;
        hpbw2[i] = bw2;
        amp1[i] = a1;
        amp2[i] = a2;
    }

    fscanf(fiod2, " %*34c");

    for (i = 0; i < 5; i ++) {
        fscanf(fiod2, "%7s", fplan[i]);
    }

    for (j = 0; j < FLU_MAXFILT; j ++) {
        fscanf(fiod2, "%*[\n]%10c", filt);
        filt[sizeof(filt) - 1] = '\0';
        for (i = 3; i <= 7; i ++) {
            fscanf(fiod2, "%lf %lf", &tbnorm[i][j], &error[i][j]);
        }
    }

    fscanf(fiod2, " ");

    for (i = 0; i < FLU_MAXNOTE; i ++) {
        if (fscanf(fiod2, "%80[^\n]", note[i]) == 1) {
            *inote = i + 1;
        }
        else {
            note[i][0] = '\0';
        }
        fscanf(fiod2, "%*1[\n]");
    }

    /* Close the file. */
    fclose(fiod2);

    /* Now make sure that the planets read from the file are indeed
     * Mars through Neptune. */
    for (i = lup[3]; i <= lup[7]; i ++) {
        if (strncasecmp(planet[i - 1], fplan[i - 4], 4)) {
            *status = SAI__ERROR;
            errRep("",
                  "FATAL: PLANET sequence not MARS-NEPTUNE in flux file.",
                  status);
        }
    }
}
