/*
*  Purpose:
*     To calculate real top Julian date from Gregorian date.
*     Returned as a modified Julian date.
*     Values are returned in TT rather than UT since this is the most common
*     usage in the FLUXES program.

*  Note:
*     Now uses palCaldj
*/

#include "star/pal.h"

#include "flu.h"

double flu_rjdate(FluDateTime* time) {
    /* Variables */
    double rjdate, fdutc;
    int j;

    palCaldj(time->iy, time->m, time->id, &rjdate, &j);
    palDtf2d(time->ih, time->im, time->s, &fdutc, &j);
    rjdate += fdutc;

    /* Correct to TT [older than 1960 we need to guess] */
    if (rjdate > 36934.0) {
        rjdate += palDtt(rjdate) / 86400.0;
    }
    else {
        rjdate += palDt(palEpj(rjdate)) / 86400.0;
    }

    return rjdate;
}
