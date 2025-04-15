/*
* Calculate Local Apparent Sidereal time given the DATE
* (MJD TT) and the longitude (radians, east +ve).
* Returns the LAST in radians.
*/

#include "star/pal.h"

#include "flu.h"

double flu_slalast(double date, double elong) {
    double utdate;

    /* Need UT for GMST calculation. Note the break at epoch 1960.0 */
    if (date > 36934.0) {
         utdate = date - (palDtt(date) / 86400.0);
    }
    else {
         utdate = date - (palDt(palEpj(date)) / 86400.0);
    }

    /* Note that DATE is UT despite what it says on the packet.
     * It is not in TT. */
    return palDranrm(palGmst(utdate) + elong + palEqeqx(date));
}
