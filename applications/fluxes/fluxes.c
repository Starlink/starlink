/*
*     Modifications:

*     26 Jan 2021 : DSB- Add custom filter support
*     22 Jun 12 : TIMJ- Add two component gaussian beam support
*     30 Aug 11 : TIMJ- Add simple SCUBA-2
*     31 Mar 05 : TIMJ- Fix infinite loop when time invalid
*     18 Mar 05 : TIMJ- A THUMPER tweak to the output formatting
*     2  Feb 04 : TIMJ- Fix TT vs UT confusion. RJDATE now returns TT MJD
*                       since that is the most common variant. LST calculation
*                       now (correctly) uses UT but all other SLA routines
*                       use TT. LST calculation factored out into separate
*                       function.
*                     - Use TELESCOPE common block for telescope parameters
*                       rather than duplicating the information. Now use
*                       SLA_OBS to obtain coordinates.
*     4  Dec 98 : TIMJ- Fix problems with linux port: Have to use
*                       BLOCK DATA to initialise COMMON. Fix MSG_FMTD
*                       (concerning SIGN variable) since SIGN is
*                       a CHAR*
*     29 Sep 98 : TIMJ- Allow Filter 1350 to be the same as 1300
*                       This is a kluge for SCUBA since we changed
*                       the name of the filter after commissioning!!
*     17 Sep 98 : TIMJ- Remove variables that are never used
*                       Use Message filtering (MSG_FILTER) and convert
*                       MSG_OUT to MSG_OUTIF
*                       Store semi-diameter as a parameter (SEMI_DIAM)
*                       and solid angle (SOLID_ANG)
*     04 Sep 98 : RPT - Add FILTER parameter and store results in
*                       as ADAM parameters.
*     03 Sep 98 : RPT - If a particular planet is specified just print
*                       fluxes for that planet rather than all.
*                       Use same index for a planet throughout.
*     06 Oct 97 : TIMJ- Fixed bug in calculation of LST. Sometimes came
*                       out negative (but sign was ignored). Now checks
*                       for this and adds 2 PI.
*     11 Jul 97 : TIMJ- Found bug when reading time from parameter TIME
*                       if less than 3 entries specified (ie HH MM does
*                       not work!) Change it such that HH MM assumes SS=0
*                       and HH assumes MM and SS = 0
*                       Change name of STOP variable to ASTOP (reserved
*                       keyword).
*                       Initialise LSTAT to good status.
*        May 97 : TIMJ- Revert back to GMTIME() since there is no
*                       PSX equivalent (PSX_TIME was incorrect)
*     06 Feb 97 : GJP - Linux port.
*                       Needed TIME()/GMTIME() replaced with
*                       PSX equivalents. Also, slight mods to
*                       a FORMAT statment -  "X" must be "1X"
*     10 Oct 96 : GJP - Modified output headings.
*     16 Sep 96 : GJP - Modified file reading code to ensure
*                       that an unused unit is always used.
*                       Also added the FLUDAT/JPL env variables
*                       to the shell script
*     12 Sep 96 : GJP - Added FIO usage to file writing sections.
*     11 Sep 96 : GJP - Modified UNIT=10 and CLOSE(10) statements
*                       to use the variable LUNO. Removed SPLIT2
*                       subroutine.
*     10 Sep 96 : GJP - Removed CHEBYBLK common block, CHEBY
*                       and READCHEB subroutines.
*     09 Sep 96 : GJP - Modified to use JPL and SLALIB routines for
*                       generating ephemeris and planet distances.
*     08 Jul 96 : GJP - Modified SCREEN output to use MSG system.
*     03 Jul 96 : GJP - Restructured some of the input routines to use the
*                       Starlink parameter system.
*     05 Jun 96 : GJP - Modified the TIMEBLK common block to avoid boundary.
*                       Modified the OUTPUT common block to avoid boundary.
*                       Introduced FLUXES.IFL and SAE_PAR.
*     05 Jun 96 : GJP - Modify the TIMEBLK common block to avoid boundary.
*                       First Starlink Version.
*     19 Mar 96 : TIMJ - Ported to unix (see notes after this list)
*     26 Oct 95 : HEM - modified subroutine TB350 to extend range of useful MJD's
*     26 Aug 95 : HEM - minor change to planet body question
*     30 Apr 93 : HEM - change to deal properly with month increment
*     12 Sep 92 : Use different values if date > 6 Aug 1992
*     14 Nov 90 : Reorganize program i/o logic
*     12 Nov 90 : Reorganize external file, plus small program mods.
*     10 Nov 90 : Changed to read external file containing data
*     25 Oct 90 : Moved program (plus ephemeris files) to [HEM.PROGS(.EPH)]
*     06 Jun 90 : FLUXNOW version, taking "now" as a default
*     20 Mar 89 : Modified by GDW
*     09 Mar 89 : Restructure of entire file structure and output format - GDW
*     06 Mar 89 : Adjustment of beam-sizes to those from Sandell's calibration
*                      note, and program now lists out which beam-sizes have been
*     16 Jun 88 : New table of brightness temperatures and errors.
*     03 Jun 88 : Remove wrong calibration of fluxes when planet is specified
*                      instead of ALL. assumed for "flux in DL beam". - Jim Emerson.
*     April 1985: Original - Catherine Hohenkerk - HM Nautical Almanac Office

*     Purpose:
*     This program calculates the positions of the planets and/or
*     the fluxes of five planetery calibrators for the effective
*     frequencies and beam-sizes of various receivers on the JCMT.

*     Arrays of effective frequencies and half-power beam-widths
*     for filters and brightness temperatures for planets (except Mars) are
*     read from a file (e.g. UKT14.DAT) . Brightness temperatures and errors
*     given by Matt Griffin. HPBW's due to G. Sandell.

*     Alternatively, if "FILTER=CUSTOM" is included on the command line,
*     the parameters of a single filter to use are obtained from the user
*     using a set of environment parameters, rather than being read from
*     a file. The planetary brightness temperature can either be supplied
*     explicitly or can be looked up from a specified NDF using the supplied
*     frequency as the index (the FLUXES package includes files esa2_uranus.sdf
*     and esa4_uranus.sdf that can be used for this purpose). See parameter
*     BTEMP.


*     Description of (non-ADAM) Parameters:

*     NF       - Number of filters at which fluxes are to be calculated.
*     TBNORM   - Array of brightness temperatures at nf
*                frequencies for five planets.
*     FREQ     - Array of effective frequencies of nf
*                filters for observing a planet.
*     TB       - Array of brightness temperatures at nf
*                frequencies for one planet.
*     FLUX     - Array of integrated flux densities at
*                nf frequencies for one planet.
*     FLUXBC   - Array of beam-corrected flux densities
*                at nf frequencies for one planet.
*     HPBW     - Array of half-power beam-widths at nf
*                frequencies.
*     GHZ857   - Frequency at which Wright's model gives
*                martian brightness temperature.
*     GHZ90    - Frequency at which Ulich's work gives
*                martian brightness temperature.
*     TB857    - Martian brightness temperature from
*                Wright's model.
*     TB90     - Martian brightness temperature from
*                Ulich's work.
*     RJD      - Julian date for which fluxes of planetary
*                calibrators are required.
*     RA       - Apparent geocentric right ascension of planet
*                for date, in degrees
*     DEC      - Apparent geocentric declination of planet
*                for date, in degrees
*     GD       - True geocentric distance of planet for date, in au
*     SRA      - Apparent ra of sun for date, ( used for mars helio dist.)
*     SDEC     - Apparent dec of sun for date (           ..             )
*     SGD      - True geocnetric distance earth-sun (     ..             )
*     HD       - Martian heliocentric distance for julian date
*     I        - Index which determines filter under consideration
*     J        - Index which determines planet under consideration
*                (4=mars,5=jupiter,6=saturn,7=uranus,8=neptune).
*     FNAME    - Wavelength of filter

****************************************************************************

*     Changes undertaken to make this run under UNIX

*     1) Output files opened as STATUS='UNKNOWN'
*     2) CHEBY common block renamed CHEBYBLK (clashes with CHEBY sub)
*     3) Function RJDATE defined as RJDATE () not RJDATE
*     4) UKT14.DAT now opened on unit 21 NOT unit 5!! This screwed up STDIN
*     5) Subroutine SPLIT re-written to use internal writes NOT for$cnv_out!
*     6) LIB$DATE_TIME replaced with call to gmtime()
*     6a) the to-UT code no longer necessary
*     6b) Renamed TIME common block as TIMEBLK as clashed with time()
*     7) The CHEBY data files converted to Sun binary - used all 16 decimal places

****************************************************************************
*/

#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "ast.h"
#include "chr.h"
#include "erfam.h"
#include "sae_par.h"
#include "merswrap.h"
#include "msg_par.h"
#include "parwrap.h"
#include "star/one.h"
#include "star/pal.h"

#include "libflu/flu.h"

/*
* This is the BLOCK DATA initialisation for the PLANET lookup
* table common block. Since all code is in a single file I put
* this at the top of the program rather than in a separate file.
* This common simply provides a number to name planet lookup table.
*/

char* planet[FLU_NPLANET] = {
        "SUN", "MERCURY", "VENUS", "MARS", "JUPITER", "SATURN",
        "URANUS", "NEPTUNE", "PLUTO", "MOON"};
int lup[FLU_NPLANET] = {11, 1, 2, 4, 5, 6, 7, 8, 9, 10};

void fluxes(int* status) {
    double cose, dec, diff, gd, hd, omega, ra, rjd,
            sdec, sgd, sra, tb90, tb857;

    double er[FLU_MAXFILT];
    double error[FLU_NPLANET][FLU_MAXFILT];
    double freq[FLU_MAXFILT][2];
    double hpbw[FLU_MAXFILT];
    double hpbw2[FLU_MAXFILT];
    double amp1[FLU_MAXFILT];
    double amp2[FLU_MAXFILT];
    double tb[FLU_MAXFILT];
    double tbnorm[FLU_NPLANET][FLU_MAXFILT];

    int i, ic, ip, ir, iq, j, nf, nb, len, np, date;

    int flu, ofl, pos, screen, repeat, curt, valid, apass, exclaim;

    FILE* fiod = 0;

    char fname[FLU_MAXFILT][FLU_FILTLEN];
    char note[FLU_MAXNOTE][FLU_NOTELEN];

    char reqbody[8];
    char reqfilt[11];
    char filt[11];
    char aline[25];
    char path[256];
    char outfile[256];

    char* string2;
    char* directory;

    size_t inote;
    time_t nticks;
    struct tm timeres;

    FluTelescope telescope;
    FluDateTime dt;

    /* Data */
    char* sysmo[] = {
            "Jan", "Feb", "Mar", "Apr", "May", "Jun",
            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};

    const double ghz857 = 857.0;
    const double ghz90 = 90.0;

    /* Check the inherited global status. */
    if (*status != SAI__OK) return;

    /* Initialise values. */
    iq = 0;
    nf = 8;

    astBegin;
    astWatch(status);

    /* Populate the telescope details common block. We use a common
     * block since 1) the numbers were originally in the code twice
     * 2) that seems to be the fluxes style 3) we may want to allow
     * a parameter to control the telescope position */
    palObs(0, "JCMT", telescope.name, sizeof(telescope.name),
            telescope.fullname, sizeof(telescope.fullname),
            &telescope.lon, &telescope.lat, &telescope.height);
    telescope.lon *= -1.0;

    /* Tell the user we are running. */
    msgBlank(status);
    msgOutif(MSG__NORM, "", "JCMT FLUXES", status);
    msgBlank(status);

    /* Zero out parameters */
    parPut0r("F_CENTRE", -1.0, status);
    parPut0r("F_WIDTH", -1.0, status);
    parPut0r("F_TOTAL", -1.0, status);
    parPut0r("F_BEAM",  -1.0, status);
    parPut0r("T_BRIGHT", -1.0, status);
    parPut0r("T_ERROR", -1.0, status);
    parPut0r("HPBW", -1.0, status);

    parPut0r("SOLID_ANG", -1.0, status);
    parPut0r("SEMI_DIAM", -1.0, status);

    /* Look for the environmental variable
     * dealing with the .DAT files and grab the name. */
    directory = getenv("FLUXES_DIR");
    if (! directory) {
        *status = SAI__ERROR;
        errRep("", "Environment variable FLUXES_DIR not set", status);
    }

    if (*status != SAI__OK) goto L9999;

    /* Planetary data? */
    parGet0l("POS", &pos, status);
    if (*status != SAI__OK) goto L9999;

    /* Flux data? */
    parGet0l("FLU", &flu, status);
    if (*status != SAI__OK) goto L9999;

    /* Check inputs so far. */
    if ((! flu) && (! pos)) {
        msgOut("", "No output requested. Exiting program.", status);
        goto L9999;
    }

    /* Display on terminal? */
    /* L401: */
    msgBlank(status);
    parGet0l("SCREEN", &screen, status);
    if (*status != SAI__OK) goto L9999;

    /* Save to a file? */
    parGet0l("OFL", &ofl, status);
    if (*status != SAI__OK) goto L9999;

    /* Get file name. */
    if (ofl) {
        /* Open the output file. */
        errMark();
        fiod = flu_asfio(
                "OUTFILE", "APPEND", &exclaim,
                outfile, sizeof(outfile), status);
        if (*status != SAI__OK) {
            errRep("",
                    "Problems opening an output file", status);
            errRlse();
            goto L9999;
        }

        /* Tell the user that the file name is duff. */
        if ((! fiod) && (! exclaim)) {
            errRep("", "Bad file name.", status);
            errRep("", "To quit, type !", status);
            errAnnul(status);
        }

        /* Cancel the error context and abort if the STATUS is bad. */
        errRlse();
        if (*status != SAI__OK) goto L9999;
        if (exclaim) goto L9999;

        /* Tell the user. */
        msgBlank(status);
        msgSetc("FN", outfile);
        msgOut("", "Data will be written to file: ^FN", status);

    }

    /*     Check inputs so far. - this check removed now that it is possible
     *     to store the results to parameters as well
     *     IF((.NOT.SCREEN).AND.(.NOT.OFL)) THEN
     *     CALL MSG_OUT(' ',
     *     :        'No output direction specified. Try again.',STATUS)
     *     GOTO 401
     *     END IF
     *     CALL MSG_BLANK(STATUS)
     *
     *     Read in date for which fluxes of planetary calibrators are
     *     required -- see ephemeris B8 et seq. */
    ip = 0;
    ic = 0;
    repeat = 0;

    /* Loop for each pass requested. */

    L1000:
    ic ++;
    if (ic != 1) {
        parCancl("PREVUT", status);
        parGet0l("PREVUT", &repeat, status);
        if (! repeat) {
            parCancl("NOW", status);
            parGet0l("NOW", &curt, status);
        }
    }
    else {
        parGet0l("NOW", &curt, status);
    }
    if (*status != SAI__OK) goto L999;
    if (screen) msgBlank(status);

    /* Avoid next section if want to use old date. */
    if (! repeat) {
        /* Find out the time/date required. */
        if (! curt) {
            /* Date section */

            /* Bypass input system if current value is to be used. */
            if (! curt) {
                /* Get input string. */
                L2:
                if (ic != 1) parCancl("DATE", status);
                parGet0c("DATE", aline, sizeof(aline), status);
                if (*status != SAI__OK) goto L999;

                /* Break up string to get M, ID, Y. */
                i = sscanf(aline, "%d %d %d", &dt.id, &dt.m, &dt.iy);

                /* Testing the date is sensible */
                if (i != 3
                        || dt.id < 1 || dt.id > 31
                        || dt.m < 1 || dt.m > 12
                        || dt.iy < 0 || dt.iy > 99) {
                    if (dt.iy < 0 || dt.iy > 99) {
                        msgOut("",
                                "YEAR OUT OF RANGE 1950-2050!", status);
                    }
                    else {
                        msgOut("",
                                "ERROR INTERPRETING DATE", status);
                    }
                    ic = ic + 1;
                    goto L2;
                }

                /* Take next century into account. */
                if (dt.iy <= 49) {
                    dt.iy += 2000;
                }
                else {
                    dt.iy += 1900;
                }

                one_strlcpy(dt.cmon, sysmo[dt.m - 1], sizeof(dt.cmon), status);
            }

            /* Time section */

            /* Bypass input system if current value is to be used. */

            if (! curt) {
                /* Get input string. */
                L3:
                if (ic != 1) parCancl("TIME", status);
                parGet0c("TIME", aline, sizeof(aline), status);
                if (*status != SAI__OK) goto L999;

                /* Break up string to get IH, IM, S. */
                i = sscanf(aline, "%d %d %lf", &dt.ih, &dt.im, &dt.s);

                /* Set values for IH, IM, S.
                 * Assume S is 0 if only 2 entries supplied
                 * Similarly for IM if only one is supplied */
                if (i == 3) {
                    dt.is = (int) (dt.s + 0.5);
                }
                else {
                    dt.s = 0.0;
                    dt.is = 0;
                }

                if (i < 2) {
                    dt.im = 0;
                }

                /* There has to be at least one entry */
                if (i < 1) {
                    dt.ih = 0;
                }

                /* Test the time value. */
                if (dt.ih < 0 || dt.ih > 23
                        || dt.im < 0 || dt.im > 59
                        || dt.s < 0.0 || dt.s >= 60.0) {
                    msgOut("",
                            "ERROR INTERPRETING TIME", status);
                    ic ++;
                    goto L3;
                }
            }
        }
        else {
            /* Get UT time/date from computer. This section
             * modified during the Linux port removing
             * TIME() and GMTIME() calls. [but there is no PSX GMTIME
             * so have to leave GMTIME in. */

            /* The system time in seconds */
            nticks = time(0);

            /* We now need the GM time. */
            gmtime_r(&nticks, &timeres);
            dt.is = timeres.tm_sec;
            dt.im = timeres.tm_min;
            dt.ih = timeres.tm_hour;
            dt.id = timeres.tm_mday;
            dt.m = timeres.tm_mon + 1;
            dt.iy = timeres.tm_year + 1900;

            dt.s = (double) dt.is;
            one_strlcpy(dt.cmon, sysmo[dt.m - 1], sizeof(dt.cmon), status);
        }
    }

    rjd = flu_rjdate(&dt);

    /* Ask for PLANET */

    L210:
    parGet0c("PLANET", reqbody, sizeof(reqbody), status);
    if (*status != SAI__OK) goto L999;

    /* Remove leading blanks and clean. */
    chrLdblk(reqbody);
    chrClean(reqbody);
    chrUcase(reqbody);
    len = strlen(reqbody);

    valid = 0;
    if (! (strcmp(reqbody, "ALL")
            && strcmp(reqbody, "all")
            && strcmp(reqbody, ""))) {
        valid = 1;
        one_strlcpy(reqbody, "ALL", sizeof(reqbody), status);
    }
    else {
        for (i = 0; i < FLU_NPLANET; i ++) {
            if (! strncmp(planet[i], reqbody, len)) {
                valid = 1;
                iq = i + 1;
            }
        }
        if (! valid) {
            msgOut("", "Invalid body name !", status);
            parCancl("PLANET", status);
            goto L210;
        }
    }

    /* Set up frequencies at which Wright's model and Ulich's work give
     * Martian brightness temperature. */

    /* Read filter file. */
    if (flu) {
        date = 10000 * (dt.iy - 1900) + 100 * dt.m + dt.id;

        /* Open the file. This is fatal if they can not be opened.
         * Should really be using FIO but this is historical.
         * SCUBA-2 starts in 2007 */
        if (date > 1070101) {
            one_snprintf(path, sizeof(path), "%s/scuba2.dat", status, directory);
        }
        else if (date > 960523) {
            one_snprintf(path, sizeof(path), "%s/scuba.dat", status, directory);
        }
        else if (date >= 920807 && date <= 960523) {
            one_snprintf(path, sizeof(path), "%s/ukt14.dat", status, directory);
        }
        else {
            one_snprintf(path, sizeof(path), "%s/ukt14_old.dat", status, directory);
        }

        flu_read_data(
                path, fname, &nf, &nb, freq,
                hpbw, hpbw2, amp1, amp2,
                tbnorm, error, &inote, note, status);
        if (*status != SAI__OK) goto L999;

        /* Ask for FILTER */
        L260:
        parGet0c("FILTER", reqfilt, sizeof(reqfilt), status);
        if (*status != SAI__OK) goto L999;

        /* Remove leading blanks and clean. */
        chrLdblk(reqfilt);
        chrClean(reqfilt);
        chrUcase(reqfilt);
        len = strlen(reqfilt);

        valid = 0;
        if (! strcmp(reqfilt, "CUSTOM")) {
            valid = flu_customfilt(
                    reqbody, fname, &nf, &nb,
                    freq, hpbw, hpbw2, amp1, amp2,
                    tbnorm[iq - 1], error[iq - 1],
                    &inote, note, status);
            freq[0][1] = 0.0;
        }
        else if (! (strcmp(reqfilt, "ALL") && strcmp(reqfilt, ""))) {
            one_strlcpy(reqfilt, "ALL", sizeof(reqfilt), status);
            valid = 1;
        }
        else {
            for (i = 0; i < nf; i ++) {
                one_strlcpy(filt, fname[i], sizeof(filt), status);
                chrUcase(filt);
                if (! strncmp(filt, reqfilt, len)) {
                    one_strlcpy(reqfilt, filt, sizeof(reqfilt), status);
                    valid = 1;
                }
            }
            if (! valid) {
                /* Could not find a matching filter name.
                 * Check to see whether the person actually asked for 1350
                 * since this is the new name of the SCUBA 1300 filter
                 * Only gets to here if the filter definition file does not
                 * have a 1350 entry */

                if (! strncmp(reqfilt, "1350", len)) {
                    one_strlcpy(reqfilt, "1300", sizeof(reqfilt), status);
                    valid = 1;
                }
                else {
                    msgSetc("FILT", reqfilt);
                    msgOut("", "Invalid filter name !: ^FILT",
                        status);
                    parCancl("FILTER", status);
                    goto L260;
                }
            }
        }
    }

    /* For fluxes calculation, obtain Martian brightness temperature. */
    if (flu) {
        tb857 = flu_tb350(rjd, status);
    }

    /* Print out the datetime and topocentric planetary positions. */

    flu_topeph(reqbody, rjd, &dt, &telescope, pos, screen, fiod, status);

    /* Do not do if fluxes not needed. */
    if (flu) {
        if (pos) ip=1;
        pos = 0;

        /* For planet (or planets in turn) work out solid angle,
         * integrated and beam-corrected flux densities.
         *
         * --------------------------------------------------------------------
         * NOTE: FLUX DATA IS ONLY AVAILABLE FOR MARS (4) THROUGH  NEPTUNE (8).
         * Make sure loop does not execute for other planets!
         * -------------------------------------------------------------------- */

        ir = -1;
        if (! strcmp(reqbody, "ALL")) {
            iq = 4;
            ir = 8;
        }
        else if (iq >= lup[3] && iq <= lup[7]) {
            ir = iq;
        }

        for (j = iq - 1; j < ir; j ++) {
            np = lup[j];
            /* Display heading on screen or file. */

            /* Planet name */
            string2 = planet[j];

            /* Screen/file. */
            if (screen) {
                msgBlank(status);
                msgBlank(status);
                msgOut("", string2, status);
            }
            if (ofl) {
                fprintf(fiod, "\n");
                fprintf(fiod, "\n");
                fprintf(fiod, "%s\n", string2);
            }

            /* Obtain Apparent Geocentric Right Ascension Declination and
             * True Distance of the planet */

            flu_geoeph(np, &ra, &dec, &gd, rjd, &telescope, status);
            omega = flu_solidangle(np, rjd, ra, dec, gd, screen, fiod, status);

            if (! strncmp(planet[j], "MARS", 4)) {
                /* Use geocentric position of Mars and the Sun to calculate the
                 * heliocentric distance of Mars */

                flu_geoeph(0, &sra, &sdec, &sgd, rjd, &telescope, status);

                /* Finds the heliocentric distance of a body from its
                 * geocentric postion and the geocentric position of the Sun. */

                cose = sin(dec * ERFA_DD2R)
                            * sin(sdec * ERFA_DD2R)
                        + cos(dec * ERFA_DD2R)
                            * cos(sdec * ERFA_DD2R)
                            * cos((sra - ra) * ERFA_DD2R);
                hd = sqrt(gd * gd + sgd * sgd - 2.0 * gd * sgd * cose);

                /* Calculate the 3.33 millimetre brightness temperature of Mars
                 * for day in question (following Ulich's work) */

                tb90 = 206.8 * sqrt(1.524 / hd);
                diff = (tb857 - tb90) / (log(ghz857) - log(ghz90));

                /* Interpolate to get Martian brightness temperatures at effective
                 * frequencies of NF filters */

                /* Do a logarithmic interpolation between two points. */

                for (i = 0; i < nf; i ++) {
                    tb[i] = tb90 + diff * (log(freq[i][0]) - log(ghz90));
                    er[i] = 0.0;
                }
            }
            else {
                for (i = 0; i < nf; i ++) {
                    tb[i] = tbnorm[j][i];
                    er[i] = error[j][i];
                }
            }

            flu_pbflux(omega, freq, tb, hpbw, hpbw2, amp1, amp2,
                    nf, fname, er, reqfilt, planet[j], screen, fiod, status);
        }
    }

    /* Loop again? */
    if (ip == 1) pos = 1;
    if (screen) msgBlank(status);

    /* Cancel loop again parameters if not first time through..
     * Needed for the FLUXNOW version. */
    if (ic != 1) parCancl("APASS", status);
    parGet0l("APASS", &apass, status);

    /* Cancel planet selection. */
    parCancl("PLANET", status);
    if (screen) msgBlank(status);

    /* Cancel filter selection. */
    parCancl("FILTER", status);
    if (screen) msgBlank(status);

    if (apass) goto L1000;

    /* Write the user notes at the end of the file. */
    if (ofl) {
        fprintf(fiod, "\n");
        for (i = 0; i < inote; i ++) {
            fprintf(fiod, "%s\n", note[i]);
        }
    }

    /* Message to tell the user what file to look in. */
    L999:
    if (ofl) {
        fclose(fiod);
        if (screen) {
            msgBlank(status);
            msgSetc("FN", outfile);
            msgOut("", "File output is in this directory. See file: ^FN", status);
            msgBlank(status);
        }
    }

    L9999: ;
}
