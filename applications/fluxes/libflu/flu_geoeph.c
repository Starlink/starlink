/*
*  Purpose:
*     Uses PAL to create geocentric planet coordinates.

*  Modified: GJP Starlink 07 September 1996
*/

#include "sae_par.h"
#include "erfam.h"

#include "flu.h"

void flu_geoeph(int np, double* exra, double* exdec, double* dist,
        double rjd, FluTelescope* telescope, int* status) {
    double tdb, rav1, decv1;

    /* Check the inherited global status. */
    if (*status != SAI__OK) return;

    /* Calculate the modified time. */
    tdb = rjd;

    /* Call JPL/SLA_LIB locations. */
    flu_slajpl2(tdb, np, telescope->lon, &rav1, &decv1, dist);

    /* Convert radians to degrees. */
    *exra = rav1 * ERFA_DR2D;
    *exdec = decv1 * ERFA_DR2D;
}
