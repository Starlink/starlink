/*
*     14 Jul04        : TIMJ - RJD is MJD
*     26 Apr01        : TIMJ - Extend model for until 2011 using
*                              numbers posted on Wright's web page
*                              http://www.astro.ucla.edu/~wright/old_mars.txt
*     26 Oct95        :  HEM - add in simplified model to extend MJD range
*     Original version:  Unknown origin

*  Purpose:
*     Do linear interpolation of Martian 350-micron brightness temp-
*     eratures (M.350.TB.'s) produced by Wright's model to get value
*     for any Modified Julian Date (MJD=J.D.-2400000.5) between
*     42760 < MJD < 52000. Two versions are used: Wright's full model
*     (Ap.J. 210, 250; 1976) is used for 46040 < MJD < 50000, and
*     outside these dates, a simplified version (Wright, private
*     communication to H.E. Matthews; 1995) is used. The latter uses a
*     modification of a rotating cratered asteroid program. After approx.
*     correction for the cosmic background (-2.56 K) this model reproduces
*     the original Wright (1976) values with an rms error of 0.13 K.

*  Input:
*     RJD    - J.D. for which M.350.TB. is calculated by interpolation

*  Returned Value:
*     TBAR   - M.350.TB. calculated by interpolation for RJD
*
*     IERR   - Error condition (0 = OK; 1 = MJD out of valid range)
*     IERR is not returned. STARLINK status is returned instead but is
*     not actually set in this routine. Routine returns immediately if
*     STATUS is bad on entry.
*/

#include "erfam.h"
#include "merswrap.h"
#include "sae_par.h"

#include "flu.h"

/* Array of 100 TB'S for MJD's 46040 - 50000 in steps of 40 days */
static const double tb1ar[] = {
        213.07, 215.42, 215.25, 213.54, 211.09, 210.47,
        211.17, 211.63, 211.75, 211.63, 210.13, 208.91, 211.40,
        218.38, 226.78, 219.98, 207.64, 206.85, 207.87, 207.42,
        206.14, 204.41, 203.76, 205.17, 208.00, 210.09, 211.98,
        213.38, 212.96, 213.10, 215.70, 220.10, 224.58, 229.15,
        233.53, 227.64, 212.36, 203.60, 199.02, 196.68, 196.97,
        200.10, 204.55, 208.20, 211.76, 214.81, 216.40, 218.74,
        222.62, 226.30, 228.04, 227.08, 222.44, 220.21, 222.70,
        212.15, 197.96, 192.10, 192.14, 196.70, 201.25, 206.03,
        210.86, 215.56, 219.23, 223.74, 229.09, 232.12, 232.14,
        228.31, 220.36, 215.72, 214.28, 215.34, 208.91, 195.23,
        190.67, 193.85, 198.52, 203.65, 209.22, 214.85, 220.20,
        226.64, 232.62, 234.88, 233.72, 228.15, 221.11, 215.95,
        212.87, 211.65, 212.30, 208.76, 197.66, 194.8 , 196.96,
        201.34, 206.73, 212.44};

/* Array of 326 TB's from simplified Wright model; step 40 days */
static const int nwright = 1167;
static const double tbwright[] = {
        218.0, 203.0, 194.0, 193.0, 197.0, 202.0, 208.0, 213.0,
        218.0, 223.0, 228.0, 234.0, 236.0, 236.0, 231.0, 224.0,
        218.0, 216.0, 216.0, 214.0, 202.0, 194.0, 196.0, 200.0,
        205.0, 211.0, 217.0, 223.0, 230.0, 235.0, 238.0, 236.0,
        230.0, 223.0, 218.0, 215.0, 213.0, 213.0, 214.0, 205.0,
        199.0, 199.0, 203.0, 208.0, 214.0, 220.0, 227.0, 233.0,
        234.0, 232.0, 227.0, 221.0, 218.0, 215.0, 213.0, 212.0,
        213.0, 215.0, 210.0, 203.0, 202.0, 205.0, 209.0, 215.0,
        221.0, 226.0, 227.0, 225.0, 221.0, 218.0, 216.0, 215.0,
        214.0, 213.0, 212.0, 212.0, 217.0, 219.0, 209.0, 205.0,
        205.0, 208.0, 215.0, 218.0, 218.0, 216.0, 214.0, 213.0,
        213.0, 214.0, 214.0, 214.0, 213.0, 212.0, 214.0, 221.0,
        229.0, 224.0, 211.0, 209.0, 210.0, 210.0, 209.0, 207.0,
        206.0, 207.0, 210.0, 212.0, 214.0, 216.0, 216.0, 216.0,
        218.0, 223.0, 227.0, 232.0, 236.0, 231.0, 216.0, 206.0,
        202.0, 199.0, 199.0, 202.0, 206.0, 210.0, 214.0, 218.0,
        219.0, 221.0, 225.0, 229.0, 231.0, 230.0, 226.0, 223.0,
        225.0, 216.0, 201.0, 195.0, 194.0, 197.0, 203.0, 208.0,
        213.0, 218.0, 222.0, 226.0, 232.0, 235.0, 235.0, 232.0,
        224.0, 219.0, 217.0, 218.0, 212.0, 198.0, 193.0, 195.0,
        201.0, 206.0, 211.0, 218.0, 223.0, 229.0, 235.0, 238.0,
        237.0, 232.0, 224.0, 219.0, 215.0, 214.0, 214.0, 212.0,
        200.0, 197.0, 199.0, 203.0, 209.0, 215.0, 221.0, 228.0,
        234.0, 236.0, 234.0, 229.0, 223.0, 218.0, 215.0, 213.0,
        212.0, 213.0, 213.0, 205.0, 201.0, 202.0, 206.0, 211.0,
        216.0, 223.0, 229.0, 230.0, 228.0, 224.0, 220.0, 217.0,
        215.0, 213.0, 212.0, 212.0, 213.0, 217.0, 212.0, 205.0,
        204.0, 206.0, 210.0, 217.0, 221.0, 221.0, 220.0, 217.0,
        215.0, 214.0, 214.0, 214.0, 214.0, 213.0, 211.0, 214.0,
        224.0, 212.0, 206.0, 206.0, 211.0, 212.0, 212.0, 211.0,
        209.0, 209.0, 211.0, 213.0, 214.0, 215.0, 215.0, 214.0,
        216.0, 220.0, 227.0, 234.0, 235.0, 220.0, 211.0, 207.0,
        204.0, 202.0, 202.0, 204.0, 207.0, 211.0, 214.0, 217.0,
        218.0, 219.0, 222.0, 226.0, 229.0, 230.0, 229.0, 229.0,
        228.0, 212.0, 201.0, 196.0, 196.0, 199.0, 204.0, 209.0,
        213.0, 218.0, 221.0, 224.0, 229.0, 233.0, 234.0, 232.0,
        225.0, 219.0, 219.0, 220.0, 208.0, 196.0, 193.0, 195.0,
        201.0, 206.0, 212.0, 217.0, 222.0, 228.0, 234.0, 237.0,
        237.0, 233.0, 225.0, 219.0, 216.0, 215.0, 215.0, 207.0,
        196.0, 195.0, 199.0, 204.0, 210.0, 216.0, 222.0, 228.0,
        235.0, 237.0, 236.0, 231.0, 224.0, 219.0,
        /* These are the new numbers from astro-ph/0703640 */
        216.0, 213.0, 213.0, 214.0, 210.0, 201.0, 199.0, 202.0,
        207.0, 212.0, 218.0, 225.0, 231.0, 233.0, 231.0, 227.0,
        221.0, 218.0, 216.0, 214.0, 212.0, 212.0, 214.0, 215.0,
        206.0, 203.0, 204.0, 207.0, 212.0, 219.0, 224.0, 225.0,
        224.0, 220.0, 217.0, 216.0, 215.0, 214.0, 213.0, 212.0,
        211.0, 215.0, 221.0, 215.0, 206.0, 204.0, 206.0, 213.0,
        216.0, 216.0, 215.0, 212.0, 211.0, 212.0, 214.0, 214.0,
        215.0, 214.0, 212.0, 214.0, 219.0, 228.0, 233.0, 220.0,
        211.0, 210.0, 209.0, 207.0, 205.0, 204.0, 206.0, 209.0,
        212.0, 214.0, 216.0, 217.0, 217.0, 219.0, 223.0, 227.0,
        230.0, 233.0, 235.0, 222.0, 209.0, 202.0, 198.0, 198.0,
        200.0, 205.0, 209.0, 213.0, 217.0, 220.0, 222.0, 226.0,
        230.0, 232.0, 231.0, 226.0, 221.0, 222.0, 220.0, 205.0,
        195.0, 193.0, 196.0, 201.0, 207.0, 212.0, 217.0, 222.0,
        226.0, 232.0, 236.0, 236.0, 233.0, 225.0, 219.0, 216.0,
        216.0, 215.0, 202.0, 194.0, 195.0, 199.0, 204.0, 210.0,
        216.0, 222.0, 228.0, 234.0, 238.0, 237.0, 232.0, 225.0,
        219.0, 216.0, 214.0, 214.0, 214.0, 205.0, 198.0, 199.0,
        202.0, 208.0, 213.0, 219.0, 226.0, 233.0, 235.0, 234.0,
        229.0, 223.0, 219.0, 216.0, 214.0, 212.0, 213.0, 215.0,
        209.0, 202.0, 202.0, 205.0, 209.0, 214.0, 221.0, 227.0,
        228.0, 227.0, 223.0, 219.0, 217.0, 215.0, 214.0, 213.0,
        212.0, 212.0, 216.0, 217.0, 208.0, 204.0, 205.0, 208.0,
        214.0, 219.0, 219.0, 218.0, 216.0, 214.0, 214.0, 214.0,
        214.0, 214.0, 213.0, 211.0, 213.0, 219.0, 227.0, 221.0,
        209.0, 207.0, 210.0, 211.0, 210.0, 209.0, 208.0, 208.0,
        210.0, 212.0, 214.0, 215.0, 216.0, 215.0, 217.0, 221.0,
        226.0, 232.0, 237.0, 230.0, 215.0, 208.0, 203.0, 201.0,
        200.0, 202.0, 206.0, 210.0, 214.0, 217.0, 218.0, 220.0,
        223.0, 227.0, 230.0, 230.0, 228.0, 225.0, 227.0, 217.0,
        203.0, 196.0, 194.0, 197.0, 202.0, 207.0, 212.0, 217.0,
        221.0, 224.0, 230.0, 234.0, 235.0, 233.0, 226.0, 220.0,
        217.0, 218.0, 213.0, 199.0, 193.0, 194.0, 200.0, 205.0,
        210.0, 217.0, 222.0, 227.0, 234.0, 237.0, 237.0, 233.0,
        226.0, 220.0, 216.0, 214.0, 215.0, 212.0, 200.0, 196.0,
        198.0, 203.0, 208.0, 214.0, 220.0, 227.0, 233.0, 236.0,
        236.0, 231.0, 225.0, 219.0, 216.0, 214.0, 212.0, 213.0,
        213.0, 204.0, 200.0, 201.0, 205.0, 211.0, 216.0, 223.0,
        229.0, 231.0, 230.0, 226.0, 221.0, 218.0, 216.0, 214.0,
        212.0, 212.0, 213.0, 217.0, 211.0, 204.0, 204.0, 206.0,
        210.0, 216.0, 222.0, 223.0, 222.0, 219.0, 216.0, 215.0,
        215.0, 214.0, 213.0, 213.0, 211.0, 213.0, 220.0, 222.0,
        211.0, 205.0, 205.0, 211.0, 214.0, 214.0, 213.0, 211.0,
        210.0, 211.0, 213.0, 214.0, 215.0, 215.0, 213.0, 214.0,
        219.0, 226.0, 234.0, 231.0, 218.0, 211.0, 208.0, 206.0,
        204.0, 203.0, 204.0, 207.0, 211.0, 214.0, 216.0, 217.0,
        218.0, 220.0, 224.0, 228.0, 230.0, 230.0, 231.0, 229.0,
        213.0, 202.0, 197.0, 196.0, 199.0, 203.0, 208.0, 212.0,
        217.0, 220.0, 223.0, 227.0, 231.0, 233.0, 232.0, 227.0,
        221.0, 220.0, 221.0, 209.0, 197.0, 193.0, 195.0, 200.0,
        205.0, 211.0, 216.0, 221.0, 226.0, 232.0, 236.0, 237.0,
        234.0, 226.0, 220.0, 216.0, 215.0, 216.0, 208.0, 196.0,
        195.0, 198.0, 203.0, 209.0, 215.0, 221.0, 227.0, 234.0,
        237.0, 237.0, 233.0, 226.0, 220.0, 216.0, 214.0, 213.0,
        214.0, 210.0, 200.0, 199.0, 201.0, 206.0, 212.0, 217.0,
        224.0, 231.0, 234.0, 233.0, 229.0, 223.0, 219.0, 216.0,
        214.0, 212.0, 212.0, 214.0, 214.0, 205.0, 202.0, 204.0,
        207.0, 212.0, 218.0, 225.0, 226.0, 226.0, 222.0, 218.0,
        216.0, 215.0, 214.0, 213.0, 212.0, 212.0, 214.0, 220.0,
        214.0, 206.0, 204.0, 206.0, 212.0, 217.0, 217.0, 217.0,
        214.0, 213.0, 213.0, 214.0, 214.0, 214.0, 214.0, 212.0,
        213.0, 218.0, 226.0, 230.0, 216.0, 209.0, 210.0, 210.0,
        209.0, 207.0, 206.0, 206.0, 209.0, 211.0, 214.0, 216.0,
        216.0, 216.0, 217.0, 221.0, 226.0, 230.0, 234.0, 236.0,
        222.0, 209.0, 203.0, 200.0, 199.0, 200.0, 204.0, 209.0,
        213.0, 216.0, 219.0, 221.0, 224.0, 228.0, 231.0, 231.0,
        228.0, 223.0, 224.0, 222.0, 206.0, 196.0, 194.0, 196.0,
        201.0, 206.0, 211.0, 216.0, 221.0, 225.0, 230.0, 234.0,
        236.0, 234.0, 227.0, 220.0, 217.0, 217.0, 216.0, 203.0,
        194.0, 194.0, 198.0, 203.0, 209.0, 215.0, 221.0, 227.0,
        233.0, 237.0, 237.0, 234.0, 227.0, 221.0, 216.0, 214.0,
        214.0, 214.0, 204.0, 197.0, 198.0, 201.0, 207.0, 213.0,
        219.0, 225.0, 232.0, 236.0, 235.0, 231.0, 225.0, 220.0,
        216.0, 214.0, 212.0, 212.0, 214.0, 209.0, 202.0, 201.0,
        204.0, 209.0, 214.0, 220.0, 227.0, 230.0, 229.0, 225.0,
        221.0, 218.0, 216.0, 214.0, 213.0, 212.0, 212.0, 216.0,
        216.0, 207.0, 204.0, 205.0, 208.0, 214.0, 220.0, 221.0,
        220.0, 218.0, 215.0, 214.0, 215.0, 214.0, 214.0, 213.0,
        212.0, 212.0, 218.0, 226.0, 218.0, 208.0, 205.0, 210.0,
        212.0, 212.0, 211.0, 209.0, 209.0, 210.0, 212.0, 214.0,
        215.0, 215.0, 214.0, 215.0, 219.0, 224.0, 231.0, 237.0,
        227.0, 215.0, 208.0, 205.0, 203.0, 201.0, 202.0, 206.0,
        210.0, 213.0, 216.0, 218.0, 219.0, 221.0, 225.0, 228.0,
        230.0, 229.0, 228.0, 230.0, 219.0, 204.0, 197.0, 195.0,
        197.0, 202.0, 207.0, 211.0, 216.0, 220.0, 223.0, 227.0,
        232.0, 234.0, 233.0, 228.0, 221.0, 218.0, 219.0, 214.0,
        200.0, 193.0, 194.0, 199.0, 204.0, 210.0, 216.0, 221.0,
        226.0, 232.0, 236.0, 237.0, 235.0, 227.0, 221.0, 217.0,
        215.0, 215.0, 212.0, 200.0, 195.0, 197.0, 202.0, 207.0,
        214.0, 219.0, 226.0, 233.0, 237.0, 237.0, 233.0, 226.0,
        221.0, 217.0, 214.0, 213.0, 213.0, 213.0, 204.0, 199.0,
        201.0, 205.0, 210.0, 215.0, 222.0, 229.0, 232.0, 232.0,
        228.0, 223.0, 219.0, 216.0, 214.0, 213.0, 212.0, 213.0,
        216.0, 210.0, 204.0, 203.0, 206.0, 210.0, 216.0, 222.0,
        225.0, 224.0, 221.0, 218.0, 216.0, 215.0, 214.0, 213.0,
        213.0, 211.0, 212.0, 219.0, 220.0, 210.0, 205.0, 205.0,
        210.0, 215.0, 215.0, 215.0, 213.0, 211.0, 212.0, 213.0,
        214.0, 215.0, 214.0, 213.0, 213.0, 217.0, 224.0, 233.0,
        228.0, 214.0, 211.0, 209.0, 207.0, 206.0, 204.0, 205.0,
        207.0, 210.0, 213.0, 215.0, 217.0, 217.0, 218.0, 222.0,
        226.0, 229.0, 231.0, 234.0, 230.0, 213.0, 204.0, 199.0,
        197.0, 199.0, 203.0, 208.0, 212.0, 216.0, 219.0, 221.0,
        225.0, 229.0, 232.0, 232.0, 228.0, 222.0, 221.0, 223.0,
        211.0, 198.0, 193.0, 194.0, 199.0, 205.0, 210.0, 215.0,
        220.0, 225.0, 230.0, 235.0, 236.0, 235.0, 228.0, 221.0,
        217.0};

double flu_tb350(double rjd, int* status) {
    /* Variables */
    double dat1i, dat1i1, tb1i, tb1i1, jd;
    int i;
    int ierr = 0;

    /* Check the inherited global status. */
    if (*status != SAI__OK) return 0.0;

    /* Convert to JD */
    jd = rjd + ERFA_DJM0;

    /* Interpolate to get M.350.TB for JD */

    tb1i = 0.0;
    i = ((int) ((jd - 2442760.5) / 40.0)) + 1;
    dat1i = 2442760.5 + ((i - 1) * 40.0);
    dat1i1 = dat1i + 40.0;
    if ((i <= 0) || (i >= nwright)) {
        ierr = 1;
    }
    else if ((i <= 82) || (i >= 183)) {
        tb1i = tbwright[i - 1] - 2.56;
    }
    else {
        tb1i = tb1ar[i - 83];
    }

    i++;
    if ((i <= 0) || (i > nwright)) {
        ierr = 1;
        msgOut("",
                "Interpolation problem in subroutine TB350!",
                status);
        msgOut("",
                "Mars fluxes will be incorrect!",
                status);
    }
    else if ((i <= 82) || (i >= 183)) {
        tb1i1 = tbwright[i - 1] - 2.56;
    }
    else {
        tb1i1 = tb1ar[i - 83];
    }

    /* Do linear interpolation between two points. */

    if (ierr == 0) {
        return tb1i + (tb1i1 - tb1i) * (jd - dat1i) / (dat1i1 - dat1i);
    }

    return 0.0;
}
