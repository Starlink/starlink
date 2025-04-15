/*
*+
*  Name:
*     CUSTOMFILT

*  Purpose:
*     Get the properties of a custom filter using environment parameters.

*  Invocation:
*     valid = flu_customfilt(reqbody, fname, nf, nb, freq,
*        hpbw1, hpbw2, amp1, amp2,
*        tbnorm, error, inote, note, status);

*  Description:
*     This routine gets the properties of a custom filter, using a set of
*     environment parameters, which correspond to the columns in the
*     scuba2.dat file. This is mainly intended for sitiuations where a
*     arbitrary centra frequency needs to be specified.

*  Arguments:
*     REQBODY = CHARACTER * ( * ) (Given)
*        Name of the planet for which fluxes are being calculated.
*     FNAME = CHARACTER * ( * ) (Returned)
*        The name of the custom filter. This will always be 'CUSTOM'.
*     NF = INTEGER (Returned)
*        The number of filtered defined by the returned arguments. This
*        will always be one.
*     NB = INTEGER (Returned)
*        The number of Gaussian components used to model the instrument beam.
*        Will be either 1 or 2.
*     FREQ = DOUBLE PRECISION (Returned)
*        FREQ be returned holding the central frequency of the
*        filter in GHz.
*     HPBW1 = DOUBLE PRECISION (Returned)
*        Half-power beam width of the first Gaussian component of the
*        beam, in arcsec.
*     HPBW2 = DOUBLE PRECISION (Returned)
*        Half-power beam width of the second Gaussian component of the
*        beam, in arcsec. Returned holding zero if NB is one.
*     AMP1 = DOUBLE PRECISION (Returned)
*        Amplitude of the first Gaussian component of the beam. Normalised
*        so that AMP1 plus AMP2 is one.
*     AMP2 = DOUBLE PRECISION (Returned)
*        Amplitude of the second Gaussian component of the beam. Normalised
*        so that AMP1 plus AMP2 is one. Returned equal to zero if NB is one.
*     TBNORM = DOUBLE PRECISION (Returned)
*        The numerical planetary brightness temperature at the required
*        frequency. This is obtained from environment parameter BTEMP,
*        which may give the numerical planetary brightness temperature
*        directly, or may be the name of a 1-dimensional NDF. If an NDF
*        name is given, it chould contain the required brightness temperature
*        as a function of frequency. The current WCS Frame should contain
*        a single spectral axis giving frerquency in units of GHz. The
*        NDF is used to look up the required temperature at the frequency
*        specified by parameter FREQ.
*     ERROR = DOUBLE PRECISION (Returned)
*        Error in TBNORM. Always returned equal to zero.
*     INOTE = INTEGER (Returned)
*        The number of elements returned in the NOTE array.
*     NOTE( 50 ) = CHARACTER*(*) (Returned)
*        The number of elements returned in the NOTE array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     Returned .FALSE. if any of the user-supplied values were
*     invalid, in which case all the environment parameters used by
*     this routine are cancelled.
*-
*/

#include <string.h>

#include "ast.h"
#include "merswrap.h"
#include "ndf.h"
#include "par_err.h"
#include "par_par.h"
#include "parwrap.h"
#include "sae_par.h"
#include "erfam.h"
#include "star/grp.h"
#include "star/kaplibs.h"

#include "flu.h"

void helper(const char* const param, const char* const reqbody, int* first, int* status);
double interp(int dim, double x, double array[], int* status);

int flu_customfilt(
        const char* const reqbody,
        char fname[][FLU_FILTLEN], int* nf, int* nb,
        double freq[][2], double* hpbw1, double* hpbw2,
        double* amp1, double* amp2, double* tbnorm, double* error,
        size_t* inote, char note[][FLU_NOTELEN],
        int* status) {
    char* note_ptr[FLU_MAXNOTE];
    char text[255];
    char* endptr;
    const char* unit;
    double x;
    double y;
    int dim;
    Grp* igrp = GRP__NOID;
    int indf;
    double* ipd;
    AstFrameSet* iwcs;
    int ndim;
    int nel;
    int first = 1;
    double tb;
    int i;

    /* Check the inherited global status. */
    if (*status != SAI__OK) return 0;

    if (! strcmp(reqbody, "ALL")) {
        *status = SAI__ERROR;
        errRep("",
                "CUSTOM filters can only be used with a single planet - not ALL.",
                status);
    }

    strncpy(fname[0], "CUSTOM", FLU_FILTLEN);
    *nf = 1;
    *error = 0.0;

    helper("NB", reqbody, &first, status);
    parGdr0i("NB", 1, 1, 2, 0, nb, status);

    helper("FREQ", reqbody, &first, status);
    parGdr0d("FREQ", 1.0, 1.0, 1.0e5, 0, &freq[0][0], status);

    helper("HPBW1", reqbody, &first, status);
    parGdr0d("HPBW1", 0.0, 0.0, 1.0e3, 0, hpbw1, status);

    helper("AMP1", reqbody, &first, status);
    parGdr0d("AMP1", -1.0e3, -1.0e3, 1.0e3, 0, amp1, status);

    if (*nb == 2) {
        helper("HPBW2", reqbody, &first, status);
        parGdr0d("HPBW2", 0.0, 0.0, 1.0e3, 0, hpbw2, status);

        helper("AMP2", reqbody, &first, status);
        parGdr0d("AMP2", -1.0e3, -1.0e3, 1.0e3, 0, amp2, status);
    }

    if (strcmp(reqbody, "MARS")) {
       helper("BTEMP", reqbody, &first, status);
       parGet0c("BTEMP", text, sizeof(text), status);
       if (*status == SAI__OK) {
          tbnorm[0] = strtod(text, &endptr);

          if (text == endptr) {
             ndfFind(0, text, &indf, status);
             ndfMap(indf, "DATA", "_DOUBLE", "READ", (void**) &ipd, &nel, status);
             ndfDim(indf, 1, &dim, &ndim, status);
             ndfGtwcs(indf, &iwcs, status);
             unit = astGetC(iwcs, "UNIT(1)");

             if (! strcmp(unit, "GHz")) {
                astTran1(iwcs, 1, &freq[0][0], 0, &x);
                tbnorm[0] = interp(dim, x - 1, ipd, status);
             }
             else if (*status == SAI__OK) {
                *status = SAI__ERROR;
                ndfMsg("N", indf);
                msgSetc("U", unit);
                errRep("",
                        "Supplied NDF '^N' has axis units '^U' - must be 'GHz'.",
                        status);
             }

             astAnnul(iwcs);
             ndfAnnul(&indf, status);
          }
       }
    }

    *inote = 0;
    if (*status == SAI__OK) {
        kpg1Gtgrp("NOTE", &igrp, inote, status);
        if (*status == PAR__NULL) {
            errAnnul(status);
        }
        else {
            /* Convert from char[][] to char** for grpGet by making pointers
               to the strings inside. */
            for (i = 0; i < FLU_MAXNOTE; i ++) {
                note_ptr[i] = note[i];
            }
            *inote = ERFA_GMIN(*inote, FLU_MAXNOTE);
            grpGet(igrp, 1, *inote, note_ptr, FLU_NOTELEN, status);
            grpDelet(&igrp, status);
        }
    }

    return (*status == SAI__OK) ? 1 : 0;
}


/* Issue a helpful message if required. Used when getting the details of
 * a custom filter.
 */
void helper(
        const char* const param, const char* const reqbody, int* first,
        int* status) {
    int state;

    parState(param, &state, status);

    if (state != PAR__ACTIVE && *first) {
        msgSetc("P", reqbody);
        msgOut("",
                "Please enter details of custom filter for ^P:",
                status);
        *first = 0;
    }
}

double interp(int dim, double x, double array[], int* status) {
    double a;
    int lo;

    if (*status != SAI__OK) return 0.0;

    if (x <= 0.0) {
        return array[0];
    }
    else if (x >= (dim - 1)) {
        return array[dim - 1];
    }

    lo = (int) x;
    a = x - lo;

    return (1.0 - a) * array[lo] + a * array[lo + 1];
}
