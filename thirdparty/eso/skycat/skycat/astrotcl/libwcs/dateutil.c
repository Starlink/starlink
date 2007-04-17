/*** File libwcs/dateutil.c
 *** March 24, 2004
 *** By Doug Mink, dmink@cfa.harvard.edu
 *** Harvard-Smithsonian Center for Astrophysics
 *** Copyright (C) 1999-2004
 *** Smithsonian Astrophysical Observatory, Cambridge, MA, USA

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Correspondence concerning WCSTools should be addressed as follows:
           Internet email: dmink@cfa.harvard.edu
           Postal address: Doug Mink
                           Smithsonian Astrophysical Observatory
                           60 Garden St.
                           Cambridge, MA 02138 USA
 */

/* Date and time conversion routines using the following conventions:
  doy = 2 floating point numbers: year and day, including fraction, of year
	*** First day of year is 1, not zero.
   dt = 2 floating point numbers: yyyy.mmdd, hh.mmssssss
   ep = fractional year, often epoch of a position including proper motion
  epb = Besselian epoch = 365.242198781-day years based on 1900.0
  epj = Julian epoch = 365.25-day years based on 2000.0
   fd = FITS date string which may be any of the following:
	yyyy.ffff (fractional year)
	dd/mm/yy (FITS standard before 2000)
	dd-mm-yy (nonstandard FITS use before 2000)
	yyyy-mm-dd (FITS standard after 1999)
	yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999)
   jd = Julian Date
   lt = Local time
  mjd = modified Julian Date = JD - 2400000.5
  ofd = FITS date string (dd/mm/yy before 2000, else no return)
 time = use fd2* with no date to convert time as hh:mm:ss.ss to sec, day, year
   ts = UT seconds since 1950-01-01T00:00 (used for ephemeris computations)
  tsi = local seconds since 1980-01-01T00:00 (used by IRAF as a time tag)
  tsu = UT seconds since 1970-01-01T00:00 (used as Unix system time)
   ut = Universal Time (UTC)
   et = Ephemeris Time (or TDB or TT)
  mst = Mean Sidereal Time
  gst = Greenwich Sidereal Time (includes nutation)
  hjd = Heliocentric Julian Date
 mhjd = modified Heliocentric Julian Date = HJD - 2400000.5

 * doy2dt (year, doy, date, time)
 *	Convert year and day of year to date as yyyy.ddmm and time as hh.mmsss
 * doy2ep, doy2epb, doy2epj (date, time)
 *	Convert year and day of year to fractional year
 * doy2fd (year, doy)
 *	Convert year and day of year to FITS date string
 * doy2mjd (year, doy)
 *	Convert year and day of year to modified Julian date
 *
 * dt2doy (date, time, year, doy)
 *	Convert date as yyyy.ddmm and time as hh.mmsss to year and day of year
 * dt2ep, dt2epb, dt2epj (date, time)
 *	Convert date as yyyy.ddmm and time as hh.mmsss to fractional year
 * dt2fd (date, time)
 *	Convert date as yyyy.ddmm and time as hh.mmsss to FITS date string
 * dt2i (date,time,iyr,imon,iday,ihr,imn,sec, ndsec)
 *	Convert yyyy.mmdd hh.mmssss to year month day hours minutes seconds
 * dt2jd (date,time)
 *	Convert date as yyyy.ddmm and time as hh.mmsss to Julian date
 * dt2mjd (date,time)
 *	Convert date as yyyy.ddmm and time as hh.mmsss to modified Julian date
 * dt2ts (date,time)
 *	Convert date (yyyy.ddmm) and time (hh.mmsss) to seconds since 1950-01-01
 * dt2tsi (date,time)
 *	Convert date (yyyy.ddmm) and time (hh.mmsss) to seconds since 1980-01-01
 * dt2tsu (date,time)
 *	Convert date (yyyy.ddmm) and time (hh.mmsss) to seconds since 1970-01-01
 *
 * ep2dt, epb2dt, epj2dt (epoch,date, time)
 *	Convert fractional year to date as yyyy.ddmm and time as hh.mmsss
 * ep2fd, epb2fd, epj2fd (epoch)
 *	Convert epoch to FITS ISO date string
 * ep2i, epb2i, epj2i (epoch,iyr,imon,iday,ihr,imn,sec, ndsec)
 *	Convert fractional year to year month day hours minutes seconds
 * ep2jd, epb2jd, epj2jd (epoch)
 *	Convert fractional year as used in epoch to Julian date
 * ep2mjd, epb2mjd, epj2mjd (epoch)
 *	Convert fractional year as used in epoch to modified Julian date
 * ep2ts, epb2ts, epj2ts (epoch)
 *	Convert fractional year to seconds since 1950.0
 *
 * et2fd (string)
 *	Convert from ET (or TDT or TT) in FITS format to UT in FITS format
 * fd2et (string)
 *	Convert from UT in FITS format to ET (or TDT or TT) in FITS format
 * jd2jed (dj)
 *	Convert from Julian Date to Julian Ephemeris Date
 * jed2jd (dj)
 *	Convert from Julian Ephemeris Date to Julian Date
 * dt2et (date, time)
 *	Convert date (yyyy.ddmm) and time (hh.mmsss) to ephemeris time
 * edt2dt (date, time)
 *	Convert ephemeris date (yyyy.ddmm) and time (hh.mmsss) to UT
 * ts2ets (tsec)
 *	Convert from UT in seconds since 1950-01-01 to ET in same format
 * ets2ts (tsec)
 *	Convert from ET in seconds since 1950-01-01 to UT in same format
 *
 * fd2ep, fd2epb, fd2epj (string)
 *	Convert FITS date string to fractional year
 *	Convert time alone to fraction of Besselian year
 * fd2doy (string, year, doy)
 *	Convert FITS standard date string to year and day of year
 * fd2dt (string, date, time)
 *	Convert FITS date string to date as yyyy.ddmm and time as hh.mmsss
 *	Convert time alone to hh.mmssss with date set to 0.0
 * fd2i (string,iyr,imon,iday,ihr,imn,sec, ndsec)
 *	Convert FITS standard date string to year month day hours min sec
 *	Convert time alone to hours min sec, year month day are zero
 * fd2jd (string)
 *	Convert FITS standard date string to Julian date
 *	Convert time alone to fraction of day
 * fd2mjd (string)
 *	Convert FITS standard date string to modified Julian date
 * fd2ts (string)
 *	Convert FITS standard date string to seconds since 1950.0
 *	Convert time alone to seconds of day
 * fd2fd (string)
 *	Convert FITS standard date string to ISO FITS date string
 * fd2of (string)
 *	Convert FITS standard date string to old-format FITS date and time
 * fd2ofd (string)
 *	Convert FITS standard date string to old-format FITS date string
 * fd2oft (string)
 *	Convert time part of FITS standard date string to FITS date string
 *
 * jd2doy (dj, year, doy)
 *	Convert Julian date to year and day of year
 * jd2dt (dj,date,time)
 *	Convert Julian date to date as yyyy.mmdd and time as hh.mmssss
 * jd2ep, jd2epb, jd2epj (dj)
 *	Convert Julian date to fractional year as used in epoch
 * jd2fd (dj)
 *	Convert Julian date to FITS ISO date string
 * jd2i (dj,iyr,imon,iday,ihr,imn,sec, ndsec)
 *	Convert Julian date to year month day hours min sec
 * jd2mjd (dj)
 *	Convert Julian date to modified Julian date
 * jd2ts (dj)
 *	Convert Julian day to seconds since 1950.0
 *
 * lt2dt()
 *	Return local time as yyyy.mmdd and time as hh.mmssss
 * lt2fd()
 *	Return local time as FITS ISO date string
 * lt2tsi()
 *	Return local time as IRAF seconds since 1980-01-01 00:00
 * lt2tsu()
 *	Return local time as Unix seconds since 1970-01-01 00:00
 * lt2ts()
 *	Return local time as Unix seconds since 1950-01-01 00:00
 *
 * mjd2doy (dj,year,doy)
 *	Convert modified Julian date to date as year and day of year
 * mjd2dt (dj,date,time)
 *	Convert modified Julian date to date as yyyy.mmdd and time as hh.mmssss
 * mjd2ep, mjd2epb, mjd2epj (dj)
 *	Convert modified Julian date to fractional year as used in epoch
 * mjd2fd (dj)
 *	Convert modified Julian date to FITS ISO date string
 * mjd2i (dj,iyr,imon,iday,ihr,imn,sec, ndsec)
 *	Convert modified Julian date to year month day hours min sec
 * mjd2jd (dj)
 *	Convert modified Julian date to Julian date
 * mjd2ts (dj)
 *	Convert modified Julian day to seconds since 1950.0
 *
 * ts2dt (tsec,date,time)
 *	Convert seconds since 1950.0 to date as yyyy.ddmm and time as hh.mmsss
 * ts2ep, ts2epb, ts2epj (tsec)
 *	Convert seconds since 1950.0 to fractional year
 * ts2fd (tsec)
 *	Convert seconds since 1950.0 to FITS standard date string
 * ts2i (tsec,iyr,imon,iday,ihr,imn,sec, ndsec)
 *	Convert sec since 1950.0 to year month day hours minutes seconds
 * ts2jd (tsec)
 *	Convert seconds since 1950.0 to Julian date
 * ts2mjd (tsec)
 *	Convert seconds since 1950.0 to modified Julian date
 * tsi2fd (tsec)
 *	Convert seconds since 1980-01-01 to FITS standard date string
 * tsi2dt (tsec,date,time)
 *	Convert seconds since 1980-01-01 to date as yyyy.ddmm, time as hh.mmsss
 * tsu2fd (tsec)
 *	Convert seconds since 1970-01-01 to FITS standard date string
 * tsu2tsi (tsec)
 *	Convert UT seconds since 1970-01-01 to local seconds since 1980-01-01
 * tsu2dt (tsec,date,time)
 *	Convert seconds since 1970-01-01 to date as yyyy.ddmm, time as hh.mmsss
 *
 * fd2gst (string)
 *      convert from FITS date Greenwich Sidereal Time
 * dt2gst (date, time)
 *      convert from UT as yyyy.mmdd hh.mmssss to Greenwich Sidereal Time
 * ts2gst (tsec)
 *      Calculate Greenwich Sidereal Time given Universal Time
 *          in seconds since 1951-01-01T0:00:00
 * fd2mst (string)
 *      convert from FITS UT date to Mean Sidereal Time
 * dt2gmt (date, time)
 *      convert from UT as yyyy.mmdd hh.mmssss to Mean Sidereal Time
 * ts2mst (tsec)
 *      Calculate Mean Sidereal Time given Universal Time
 *          in seconds since 1951-01-01T0:00:00
 * compnut (dj, dpsi, deps, eps0)
 *      Compute the longitude and obliquity components of nutation and
 *      mean obliquity from the IAU 1980 theory
 *
 * utdt (dj)
 *	Compute difference between UT and dynamical time (ET-UT)
 * ut2dt (year, doy)
 *	Current Universal Time to year and day of year
 * ut2dt (date, time)
 *	Current Universal Time to date (yyyy.mmdd) and time (hh.mmsss)
 * ut2ep(), ut2epb(), ut2epj()
 *	Current Universal Time to fractional year, Besselian, Julian epoch
 * ut2fd()
 *	Current Universal Time to FITS ISO date string
 * ut2jd()
 *	Current Universal Time to Julian Date
 * ut2mjd()
 *	Current Universal Time to Modified Julian Date
 * ut2tsi()
 *	Current Universal Time to IRAF seconds since 1980-01-01T00:00
 * ut2tsu()
 *	Current Universal Time to Unix seconds since 1970-01-01T00:00
 * ut2ts()
 *	Current Universal Time to seconds since 1950-01-01T00:00
 * isdate (string)
 *	Return 1 if string is a FITS date (old or ISO)
 *
 * Internally-used subroutines
 *
 * fixdate (iyr, imon, iday, ihr, imn, sec, ndsec)
 *	Round seconds and make sure date and time numbers are within limits
 * caldays (year, month)
 *	Calculate days in month 1-12 given year (Gregorian calendar only
 * dint (dnum)
 *	Return integer part of floating point number
 * dmod (dnum)
 *	Return Mod of floating point number
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <sys/time.h>
#include "wcs.h"
#include "fitsfile.h"

static double suntl();
static void fixdate();
static int caldays();
static double dint();
static double dmod();

static int ndec = 3;
void
setdatedec (nd)
int nd;
{ ndec = nd; return; }


/* DT2FD-- convert vigesimal date and time to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
dt2fd (date, time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;
    int nf;
    char *string;
    char tstring[32], dstring[32];
    char outform[64];

    dt2i (date, time, &iyr,&imon,&iday,&ihr,&imn,&sec, ndec);

    /* Convert to ISO date format */
    string = (char *) calloc (32, sizeof (char));

    /* Make time string */
    if (time != 0.0 || ndec > 0) {
	if (ndec == 0)
	    nf = 2;
	else
	    nf = 3 + ndec;
	if (ndec > 0) {
	    sprintf (outform, "%%02d:%%02d:%%0%d.%df", nf, ndec);
	    sprintf (tstring, outform, ihr, imn, sec);
	    }
	else {
	    sprintf (outform, "%%02d:%%02d:%%0%dd", nf);
	    sprintf (tstring, outform, ihr, imn, (int)(sec+0.5));
	    }
	}

    /* Make date string */
    if (date != 0.0)
	sprintf (dstring, "%4d-%02d-%02d", iyr, imon, iday);

    /* Make FITS (ISO) date string */
    if (date == 0.0)
	strcpy (string, tstring);
    else if (time == 0.0 && ndec < 1)
	strcpy (string, dstring);
    else
	sprintf (string, "%sT%s", dstring, tstring);

    return (string);
}


/* DT2JD-- convert from date as yyyy.mmdd and time as hh.mmsss to Julian Date
 *	   Return fractional days if date is zero */

double
dt2jd (date,time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj;		/* Julian date (returned) */
    double tsec;	/* seconds since 1950.0 */

    tsec = dt2ts (date, time);
    if (date == 0.0)
	dj = tsec / 86400.0;
    else
	dj = ts2jd (tsec);

    return (dj);
}


/* DT2MJD-- convert from date yyyy.mmdd time hh.mmsss to modified Julian Date
 *	   Return fractional days if date is zero */

double
dt2mjd (date,time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj;		/* Modified Julian date (returned) */
    double tsec;	/* seconds since 1950.0 */

    tsec = dt2ts (date, time);
    if (date == 0.0)
	dj = tsec / 86400.0;
    else
	dj = ts2jd (tsec);

    return (dj - 2400000.5);
}


/* HJD2JD-- convert  Heliocentric Julian Date to (geocentric) Julian date */

double
hjd2jd (dj, ra, dec, sys)

double	dj;	/* Heliocentric Julian date */
double	ra;	/* Right ascension (degrees) */
double	dec;	/* Declination (degrees) */
int	sys;	/* J2000, B1950, GALACTIC, ECLIPTIC */
{
    double lt;		/* Light travel difference to the Sun (days) */

    lt = suntl (dj, ra, dec, sys);

    /* Return Heliocentric Julian Date */
    return (dj - lt);
}


/* JD2HJD-- convert (geocentric) Julian date to Heliocentric Julian Date */

double
jd2hjd (dj, ra, dec, sys)

double	dj;	/* Julian date (geocentric) */
double	ra;	/* Right ascension (degrees) */
double	dec;	/* Declination (degrees) */
int	sys;	/* J2000, B1950, GALACTIC, ECLIPTIC */
{
    double lt;		/* Light travel difference to the Sun (days) */

    lt = suntl (dj, ra, dec, sys);

    /* Return Heliocentric Julian Date */
    return (dj + lt);
}


/* MHJD2MJD-- convert modified Heliocentric Julian Date to
	      modified geocentric Julian date */

double
mhjd2mjd (mhjd, ra, dec, sys)

double	mhjd;	/* Modified Heliocentric Julian date */
double	ra;	/* Right ascension (degrees) */
double	dec;	/* Declination (degrees) */
int	sys;	/* J2000, B1950, GALACTIC, ECLIPTIC */
{
    double lt;		/* Light travel difference to the Sun (days) */
    double hjd;		/* Heliocentric Julian date */

    hjd = mjd2jd (mhjd);

    lt = suntl (hjd, ra, dec, sys);

    /* Return Heliocentric Julian Date */
    return (jd2mjd (hjd - lt));
}


/* MJD2MHJD-- convert modified geocentric Julian date tp
	      modified Heliocentric Julian Date */

double
mjd2mhjd (mjd, ra, dec, sys)

double	mjd;	/* Julian date (geocentric) */
double	ra;	/* Right ascension (degrees) */
double	dec;	/* Declination (degrees) */
int	sys;	/* J2000, B1950, GALACTIC, ECLIPTIC */
{
    double lt;		/* Light travel difference to the Sun (days) */
    double	dj;	/* Julian date (geocentric) */

    dj = mjd2jd (mjd);

    lt = suntl (dj, ra, dec, sys);

    /* Return Heliocentric Julian Date */
    return (jd2mjd (dj + lt));
}


/* SUNTL-- compute light travel time to heliocentric correction in days */
/* Translated into C from IRAF SPP noao.astutils.asttools.asthjd.x */

static double
suntl (dj, ra, dec, sys)

double	dj;	/* Julian date (geocentric) */
double	ra;	/* Right ascension (degrees) */
double	dec;	/* Declination (degrees) */
int	sys;	/* J2000, B1950, GALACTIC, ECLIPTIC */
{
    double t;		/* Number of Julian centuries since J1900 */
    double manom;	/* Mean anomaly of the Earth's orbit (degrees) */
    double lperi;	/* Mean longitude of perihelion (degrees) */
    double oblq;	/* Mean obliquity of the ecliptic (degrees) */
    double eccen;	/* Eccentricity of the Earth's orbit (dimensionless) */
    double eccen2, eccen3;
    double tanom;	/* True anomaly (approximate formula) (radians) */
    double slong;	/* True longitude of the Sun from the Earth (radians) */
    double rs;		/* Distance to the sun (AU) */
    double lt;		/* Light travel difference to the Sun (days) */
    double l;		/* Longitude of star in orbital plane of Earth (radians) */
    double b;		/* Latitude of star in orbital plane of Earth (radians) */
    double epoch;	/* Epoch of obervation */
    double rs1,rs2;

    t = (dj - 2415020.0) / 36525.0;

    /* Compute earth orbital parameters */
    manom = 358.47583 + (t * (35999.04975 - t * (0.000150 + t * 0.000003)));
    lperi = 101.22083 + (t * (1.7191733 + t * (0.000453 + t * 0.000003)));
    oblq = 23.452294 - (t * (0.0130125 + t * (0.00000164 - t * 0.000000503)));
    eccen = 0.01675104 - (t * (0.00004180 + t * 0.000000126));
    eccen2 = eccen * eccen;
    eccen3 = eccen * eccen2;

    /* Convert to principle angles */
    manom = manom - (360.0 * (dint) (manom / 360.0));
    lperi = lperi - (360.0 * (dint) (lperi / 360.0));

    /* Convert to radians */
    manom = degrad (manom);
    lperi = degrad (lperi);
    oblq = degrad (oblq);

    /* True anomaly */
    tanom = manom + (2 * eccen - 0.25 * eccen3) * sin (manom) +
	    1.25 * eccen2 * sin (2 * manom) +
	    13./12. * eccen3 * sin (3 * manom);

    /* Distance to the Sun */
    rs1 = 1.0 - eccen2;
    rs2 = 1.0 + (eccen * cos (tanom));
    rs = rs1 / rs2;

    /* True longitude of the Sun seen from the Earth */
    slong = lperi + tanom + PI;

    /* Longitude and latitude of star in orbital plane of the Earth */
    epoch = jd2ep (dj);
    wcscon (sys, WCS_ECLIPTIC, 0.0, 0.0, &ra, &dec, epoch);
    l = degrad (ra);
    b = degrad (dec);

    /* Light travel difference to the Sun */
    lt = -0.005770 * rs * cos (b) * cos (l - slong);

    /* Return light travel difference */
    return (lt);
}


/* JD2DT-- convert Julian date to date as yyyy.mmdd and time as hh.mmssss */

void
jd2dt (dj,date,time)

double	dj;	/* Julian date */
double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;

    /* Convert Julian Date to date and time */
    jd2i (dj, &iyr, &imon, &iday, &ihr, &imn, &sec, 4);

    /* Convert date to yyyy.mmdd */
    if (iyr < 0) {
	*date = (double) (-iyr) + 0.01 * (double) imon + 0.0001 * (double) iday;
	*date = -(*date);
	}
    else
	*date = (double) iyr + 0.01 * (double) imon + 0.0001 * (double) iday;

    /* Convert time to hh.mmssssss */
    *time = (double) ihr + 0.01 * (double) imn + 0.0001 * sec;

    return;
}


/* JD2I-- convert Julian date to date as year, month, and day, and time hours,
          minutes, and seconds */
/*        after Fliegel and Van Flander, CACM 11, 657 (1968) */


void
jd2i (dj, iyr, imon, iday, ihr, imn, sec, ndsec)

double	dj;	/* Julian date */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */

{
    double tsec;
    double frac, dts, ts, sday;
    int jd, l, n, i, j;

    tsec = jd2ts (dj);
    /* ts2i (tsec, iyr, imon, iday, ihr, imn, sec, ndsec); */

    /* Round seconds to 0 - 4 decimal places */
    if (tsec < 0.0)
	dts = -0.5;
    else
	dts = 0.5;
    if (ndsec < 1)
	ts = dint (tsec + dts);
    else if (ndsec < 2)
	ts = dint (tsec * 10.0 + dts) / 10.0;
    else if (ndsec < 3)
	ts = dint (tsec * 100.0 + dts) / 100.0;
    else if (ndsec < 4)
	ts = dint (tsec * 1000.0 + dts) / 1000.0;
    else
	ts = dint (tsec * 10000.0 + dts) / 10000.0;

    /* Convert back to Julian Date */
    dj = ts2jd (ts);

    /* Compute time from fraction of a day */
    frac = dmod (dj, 1.0);
    if (frac < 0.5) {
	jd = (int) (dj - frac);
	sday = (frac + 0.5) * 86400.0;
	}
    else {
	jd = (int) (dj - frac) + 1;
	sday = (frac - 0.5) * 86400.0;
	}
    
    *ihr = (int) (sday / 3600.0);
    sday = sday - (double) (*ihr * 3600);
    *imn = (int) (sday / 60.0);
    *sec = sday - (double) (*imn * 60);

    /* Compute day, month, year */
    l = jd + 68569;
    n = (4 * l) / 146097;
    l = l - (146097 * n + 3) / 4;
    i = (4000 * (l + 1)) / 1461001;
    l = l - (1461 * i) / 4 + 31;
    j = (80 * l) / 2447;
    *iday = l - (2447 * j) / 80;
    l = j / 11;
    *imon = j + 2 - (12 * l);
    *iyr = 100 * (n - 49) + i + l;

    return;
}


/* JD2MJD-- convert Julian Date to Modified Julian Date */

double
jd2mjd (dj)

double	dj;	/* Julian Date */

{
    return (dj - 2400000.5);
}


/* JD2EP-- convert Julian date to fractional year as used in epoch */

double
jd2ep (dj)

double	dj;	/* Julian date */

{
    double date, time;
    jd2dt (dj, &date, &time);
    return (dt2ep (date, time));
}


/* JD2EPB-- convert Julian date to Besselian epoch */

double
jd2epb (dj)

double	dj;	/* Julian date */

{
    return (1900.0 + (dj - 2415020.31352) / 365.242198781);
}


/* JD2EPJ-- convert Julian date to Julian epoch */

double
jd2epj (dj)

double	dj;	/* Julian date */

{
    return (2000.0 + (dj - 2451545.0) / 365.25);
}


/* LT2DT-- Return local time as yyyy.mmdd and time as hh.mmssss */

void
lt2dt(date, time)

double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */

{
    time_t tsec;
    struct timeval tp;
    struct timezone tzp;
    struct tm *ts;

    gettimeofday (&tp,&tzp);

    tsec = tp.tv_sec;
    ts = localtime (&tsec);

    if (ts->tm_year < 1000)
	*date = (double) (ts->tm_year + 1900);
    else
	*date = (double) ts->tm_year;
    *date = *date + (0.01 * (double) (ts->tm_mon + 1));
    *date = *date + (0.0001 * (double) ts->tm_mday);
    *time = (double) ts->tm_hour;
    *time = *time + (0.01 * (double) ts->tm_min);
    *time = *time + (0.0001 * (double) ts->tm_sec);

    return;
}


/* LT2FD-- Return current local time as FITS ISO date string */

char *
lt2fd()
{
    time_t tsec;
    struct tm *ts;
    struct timeval tp;
    struct timezone tzp;
    int month, day, year, hour, minute, second;
    char *isotime;

    gettimeofday (&tp,&tzp);
    tsec = tp.tv_sec;

    ts = localtime (&tsec);

    year = ts->tm_year;
    if (year < 1000)
	year = year + 1900;
    month = ts->tm_mon + 1;
    day = ts->tm_mday;
    hour = ts->tm_hour;
    minute = ts->tm_min;
    second = ts->tm_sec;

    isotime = (char *) calloc (32, sizeof (char));
    sprintf (isotime, "%04d-%02d-%02dT%02d:%02d:%02d",
                      year, month, day, hour, minute, second);
    return (isotime);
}


/* LT2TSI-- Return local time as IRAF seconds since 1980-01-01 00:00 */

int
lt2tsi()
{
    return ((int)(lt2ts() - 946684800.0));
}


/* LT2TSU-- Return local time as Unix seconds since 1970-01-01 00:00 */

time_t
lt2tsu()
{
    return ((time_t)(lt2ts() - 631152000.0));
}

/* LT2TS-- Return local time as Unix seconds since 1950-01-01 00:00 */

double
lt2ts()
{
    double tsec;
    char *datestring;
    datestring = lt2fd();
    tsec = fd2ts (datestring);
    free (datestring);
    return (tsec);
}


/* MJD2DT-- convert Modified Julian Date to date (yyyy.mmdd) time (hh.mmssss) */

void
mjd2dt (dj,date,time)

double	dj;	/* Modified Julian Date */
double	*date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	*time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double tsec;

    tsec = jd2ts (dj + 2400000.5);
    ts2dt (tsec, date, time);

    return;
}


/* MJD2I-- convert Modified Julian Date to date as year, month, day and
           time as hours, minutes, seconds */

void
mjd2i (dj, iyr, imon, iday, ihr, imn, sec, ndsec)

double	dj;	/* Modified Julian Date */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */

{
    double tsec;

    tsec = jd2ts (dj + 2400000.5);
    ts2i (tsec, iyr, imon, iday, ihr, imn, sec, ndsec);
    return;
}


/* MJD2DOY-- convert Modified Julian Date to Year,Day-of-Year */

void
mjd2doy (dj, year, doy)

double	dj;	/* Modified Julian Date */
int	*year;	/* Year (returned) */
double	*doy;	/* Day of year with fraction (returned) */

{
    jd2doy (dj + 2400000.5, year, doy);
    return;
}


/* MJD2JD-- convert Modified Julian Date to Julian Date */

double
mjd2jd (dj)

double	dj;	/* Modified Julian Date */

{
    return (dj + 2400000.5);
}


/* MJD2EP-- convert Modified Julian Date to fractional year */

double
mjd2ep (dj)

double	dj;	/* Modified Julian Date */

{
    double date, time;
    jd2dt (dj + 2400000.5, &date, &time);
    return (dt2ep (date, time));
}


/* MJD2EPB-- convert Modified Julian Date to Besselian epoch */

double
mjd2epb (dj)

double	dj;	/* Modified Julian Date */

{
    return (1900.0 + (dj - 15019.81352) / 365.242198781);
}


/* MJD2EPJ-- convert Modified Julian Date to Julian epoch */

double
mjd2epj (dj)

double	dj;	/* Modified Julian Date */

{
    return (2000.0 + (dj - 51544.5) / 365.25);
}


/* MJD2FD-- convert modified Julian date to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
mjd2fd (dj)

double	dj;	/* Modified Julian date */
{
    return (jd2fd (dj + 2400000.5));
}


/* MJD2TS-- convert modified Julian date to seconds since 1950.0 */

double
mjd2ts (dj)

double	dj;	/* Modified Julian date */
{
    return ((dj - 33282.0) * 86400.0);
}


/* EP2FD-- convert fractional year to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
ep2fd (epoch)

double	epoch;	/* Date as fractional year */
{
    double tsec; /* seconds since 1950.0 (returned) */
    tsec = ep2ts (epoch);
    return (ts2fd (tsec));
}


/* EPB2FD-- convert Besselian epoch to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
epb2fd (epoch)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */
{
    double dj;		/* Julian Date */
    dj = epb2jd (epoch);
    return (jd2fd (dj));
}


/* EPJ2FD-- convert Julian epoch to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
epj2fd (epoch)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */
{
    double dj;		/* Julian Date */
    dj = epj2jd (epoch);
    return (jd2fd (dj));
}


/* EP2TS-- convert fractional year to seconds since 1950.0 */

double
ep2ts (epoch)

double	epoch;	/* Date as fractional year */
{
    double dj;
    dj = ep2jd (epoch);
    return ((dj - 2433282.5) * 86400.0);
}


/* EPB2TS-- convert Besselian epoch to seconds since 1950.0 */

double
epb2ts (epoch)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */
{
    double dj;
    dj = epb2jd (epoch);
    return ((dj - 2433282.5) * 86400.0);
}


/* EPJ2TS-- convert Julian epoch to seconds since 1950.0 */

double
epj2ts (epoch)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */
{
    double dj;
    dj = epj2jd (epoch);
    return ((dj - 2433282.5) * 86400.0);
}


/* EPB2EP-- convert Besselian epoch to fractional years */

double
epb2ep (epoch)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */
{
    double dj;
    dj = epb2jd (epoch);
    return (jd2ep (dj));
}


/* EP2EPB-- convert fractional year to Besselian epoch */

double
ep2epb (epoch)

double	epoch;	/* Fractional year */
{
    double dj;
    dj = ep2jd (epoch);
    return (jd2epb (dj));
}


/* EPJ2EP-- convert Julian epoch to fractional year */

double
epj2ep (epoch)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */
{
    double dj;
    dj = epj2jd (epoch);
    return (jd2ep (dj));
}


/* EP2EPJ-- convert fractional year to Julian epoch */

double
ep2epj (epoch)

double	epoch;	/* Fractional year */
{
    double dj;
    dj = ep2jd (epoch);
    return (jd2epj (dj));
}


/* EP2I-- convert fractional year to year month day hours min sec */

void
ep2i (epoch, iyr, imon, iday, ihr, imn, sec, ndsec)

double	epoch;	/* Date as fractional year */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */
{
    double date, time;

    ep2dt (epoch, &date, &time);
    dt2i (date, time, iyr,imon,iday,ihr,imn,sec, ndsec);
    return;
}


/* EPB2I-- convert Besselian epoch to year month day hours min sec */

void
epb2i (epoch, iyr, imon, iday, ihr, imn, sec, ndsec)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */
{
    double date, time;

    epb2dt (epoch, &date, &time);
    dt2i (date, time, iyr,imon,iday,ihr,imn,sec, ndsec);
    return;
}


/* EPJ2I-- convert Julian epoch to year month day hours min sec */

void
epj2i (epoch, iyr, imon, iday, ihr, imn, sec, ndsec)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */
{
    double date, time;

    epj2dt (epoch, &date, &time);
    dt2i (date, time, iyr,imon,iday,ihr,imn,sec, ndsec);
    return;
}


/* EP2JD-- convert fractional year as used in epoch to Julian date */

double
ep2jd (epoch)

double	epoch;	/* Date as fractional year */

{
    double dj;	/* Julian date (returned)*/
    double date, time;

    ep2dt (epoch, &date, &time);
    dj = dt2jd (date, time);
    return (dj);
}


/* EPB2JD-- convert Besselian epoch to Julian Date */

double
epb2jd (epoch)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */

{
    return (2415020.31352 + ((epoch - 1900.0) * 365.242198781));
}


/* EPJ2JD-- convert Julian epoch to Julian Date */

double
epj2jd (epoch)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */

{
    return (2451545.0 + ((epoch - 2000.0) * 365.25));
}


/* EP2MJD-- convert fractional year as used in epoch to modified Julian date */

double
ep2mjd (epoch)

double	epoch;	/* Date as fractional year */

{
    double dj;	/* Julian date (returned)*/
    double date, time;

    ep2dt (epoch, &date, &time);
    dj = dt2jd (date, time);
    return (dj - 2400000.5);
}


/* EPB2MJD-- convert Besselian epoch to modified Julian Date */

double
epb2mjd (epoch)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */

{
    return (15019.81352 + ((epoch - 1900.0) * 365.242198781));
}


/* EPJ2MJD-- convert Julian epoch to modified Julian Date */

double
epj2mjd (epoch)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */

{
    return (51544.5 + ((epoch - 2000.0) * 365.25));
}



/* EPB2EPJ-- convert Besselian epoch to Julian epoch */

double
epb2epj (epoch)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */
{
    double dj;		/* Julian date */
    dj = epb2jd (epoch);
    return (jd2epj (dj));
}


/* EPJ2EPB-- convert Julian epoch to Besselian epoch */

double
epj2epb (epoch)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */
{
    double dj;		/* Julian date */
    dj = epj2jd (epoch);
    return (jd2epb (dj));
}


/* JD2FD-- convert Julian date to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
jd2fd (dj)

double	dj;	/* Julian date */
{
    double tsec;		/* seconds since 1950.0 (returned) */
    tsec = (dj - 2433282.5) * 86400.0;
    return (ts2fd (tsec));
}


/* JD2TS-- convert Julian date to seconds since 1950.0 */

double
jd2ts (dj)

double	dj;	/* Julian date */
{
    return ((dj - 2433282.5) * 86400.0);
}


/* JD2TSI-- convert Julian date to IRAF seconds since 1980-01-01T0:00 */

double
jd2tsi (dj)

double	dj;	/* Julian date */
{
    return ((dj - 2444239.5) * 86400.0);
}


/* JD2TSU-- convert Julian date to Unix seconds since 1970-01-01T0:00 */

double
jd2tsu (dj)

double	dj;	/* Julian date */
{
    return ((dj - 2440587.5) * 86400.0);
}


/* DT2DOY-- convert yyyy.mmdd hh.mmss to year and day of year */

void
dt2doy (date, time, year, doy)

double	date;	/* Date as yyyy.mmdd */
double	time;	/* Time as hh.mmssxxxx */
int	*year;	/* Year (returned) */
double	*doy;	/* Day of year with fraction (returned) */
{
    double	dj;	/* Julian date */
    double	dj0;	/* Julian date on January 1 0:00 */
    double	date0;	/* January first of date's year */
    double	dyear;

    dyear = floor (date);
    date0 = dyear + 0.0101;
    dj0 = dt2jd (date0, 0.0);
    dj = dt2jd (date, time);
    *year = (int) (dyear + 0.00000001);
    *doy = dj - dj0 + 1.0;
    return;
}


/* DOY2DT-- convert year and day of year to yyyy.mmdd hh.mmss */

void
doy2dt (year, doy, date, time)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    double	dj;	/* Julian date */
    double	dj0;	/* Julian date on January 1 0:00 */
    double	date0;	/* January first of date's year */

    date0 = year + 0.0101;
    dj0 = dt2jd (date0, 0.0);
    dj = dj0 + doy - 1.0;
    jd2dt (dj, date, time);
    return;
}


/* DOY2EP-- convert year and day of year to fractional year as used in epoch */

double
doy2ep (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double date, time;
    doy2dt (year, doy, &date, &time);
    return (dt2ep (date, time));
}



/* DOY2EPB-- convert year and day of year to Besellian epoch */

double
doy2epb (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double dj;
    dj = doy2jd (year, doy);
    return (jd2epb (dj));
}


/* DOY2EPJ-- convert year and day of year to Julian epoch */

double
doy2epj (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double dj;
    dj = doy2jd (year, doy);
    return (jd2epj (dj));
}


/* DOY2FD-- convert year and day of year to FITS date */

char *
doy2fd (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double dj;	/* Julian date  */

    dj = doy2jd (year, doy);
    return (jd2fd (dj));
}


/* DOY2JD-- convert year and day of year to Julian date */

double
doy2jd (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double	dj0;	/* Julian date */
    double	date;	/* Date as yyyy.mmdd (returned) */
    double	time;	/* Time as hh.mmssxxxx (returned) */

    date = (double) year + 0.0101;
    time = 0.0;
    dj0 = dt2jd (date, time);
    return (dj0 + doy - 1.0);
}


/* DOY2MJD-- convert year and day of year to Julian date */

double
doy2mjd (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double	dj0;	/* Julian date */
    double	date;	/* Date as yyyy.mmdd (returned) */
    double	time;	/* Time as hh.mmssxxxx (returned) */

    date = (double) year + 0.0101;
    time = 0.0;
    dj0 = dt2jd (date, time);
    return (dj0 + doy - 1.0 - 2400000.5);
}


/* DOY2TSU-- convert from FITS date to Unix seconds since 1970-01-01T0:00 */

time_t
doy2tsu (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double dj;
    dj = doy2jd (year, doy);
    return ((time_t)jd2ts (dj));
}


/* DOY2TSI-- convert from FITS date to IRAF seconds since 1980-01-01T0:00 */

int
doy2tsi (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double dj;
    dj = doy2jd (year, doy);
    return ((int)jd2tsi (dj));
}


/* DOY2TS-- convert year, day of year to seconds since 1950 */

double
doy2ts (year, doy)

int	year;	/* Year */
double	doy;	/* Day of year with fraction */
{
    double dj;
    dj = doy2jd (year, doy);
    return (jd2tsu (dj));
}


/* FD2DOY-- convert FITS date to year and day of year */

void
fd2doy (string, year, doy)

char	*string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
int	*year;	/* Year (returned) */
double	*doy;	/* Day of year with fraction (returned) */
{
    double dj;	/* Julian date */

    dj = fd2jd (string);
    jd2doy (dj, year, doy);
    return;
}


/* JD2DOY-- convert Julian date to year and day of year */

void
jd2doy (dj, year, doy)

double	dj;	/* Julian date */
int	*year;	/* Year (returned) */
double	*doy;	/* Day of year with fraction (returned) */
{
    double date;	/* Date as yyyy.mmdd (returned) */
    double time;	/* Time as hh.mmssxxxx (returned) */
    double dj0;		/* Julian date at 0:00 on 1/1 */
    double dyear;

    jd2dt (dj, &date, &time);
    *year = (int) date;
    dyear = (double) *year;
    dj0 = dt2jd (dyear+0.0101, 0.0);
    *doy = dj - dj0 + 1.0;
    return;
}


/* TS2JD-- convert seconds since 1950.0 to Julian date */

double
ts2jd (tsec)

double	tsec;	/* seconds since 1950.0 */
{
    return (2433282.5 + (tsec / 86400.0));
}


/* TS2MJD-- convert seconds since 1950.0 to modified Julian date */

double
ts2mjd (tsec)

double	tsec;	/* seconds since 1950.0 */
{
    return (33282.0 + (tsec / 86400.0));
}


/* TS2EP-- convert seconds since 1950.0 to fractional year as used in epoch */

double
ts2ep (tsec)

double	tsec;	/* Seconds since 1950.0 */

{
    double date, time;
    ts2dt (tsec, &date, &time);
    return (dt2ep (date, time));
}


/* TS2EPB-- convert seconds since 1950.0 to Besselian epoch */

double
ts2epb (tsec)

double	tsec;	/* Seconds since 1950.0 */

{
    double dj;		/* Julian Date */
    dj = ts2jd (tsec);
    return (jd2epb (dj));
}


/* TS2EPB-- convert seconds since 1950.0 to Julian epoch */

double
ts2epj (tsec)

double	tsec;	/* Seconds since 1950.0 */

{
    double dj;		/* Julian Date */
    dj = ts2jd (tsec);
    return (jd2epj (dj));
}


/* DT2EP-- convert from date, time as yyyy.mmdd hh.mmsss to fractional year */

double
dt2ep (date, time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double epoch; /* Date as fractional year (returned) */
    double dj, dj0, dj1, date0, time0, date1;

    dj = dt2jd (date, time);
    if (date == 0.0)
	epoch = dj / 365.2422;
    else {
	time0 = 0.0;
	date0 = dint (date) + 0.0101;
	date1 = dint (date) + 1.0101;
	dj0 = dt2jd (date0, time0);
	dj1 = dt2jd (date1, time0);
	epoch = dint (date) + ((dj - dj0) / (dj1 - dj0));
	}
    return (epoch);
}


/* DT2EPB-- convert from date, time as yyyy.mmdd hh.mmsss to Besselian epoch */

double
dt2epb (date, time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj;		/* Julian date */
    double epoch;	/* Date as fractional year (returned) */
    dj = dt2jd (date, time);
    if (date == 0.0)
	epoch = dj / 365.242198781;
    else
	epoch = jd2epb (dj);
    return (epoch);
}


/* DT2EPJ-- convert from date, time as yyyy.mmdd hh.mmsss to Julian epoch */

double
dt2epj (date, time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj;		/* Julian date */
    double epoch;	/* Date as fractional year (returned) */
    dj = dt2jd (date, time);
    if (date == 0.0)
	epoch = dj / 365.25;
    else
	epoch = jd2epj (dj);
    return (epoch);
}


/* EP2DT-- convert from fractional year to date, time as yyyy.mmdd hh.mmsss */

void
ep2dt (epoch, date, time)

double epoch;	/* Date as fractional year */
double	*date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	*time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj, dj0, dj1, date0, time0, date1, epochi, epochf;

    time0 = 0.0;
    epochi = dint (epoch);
    epochf = epoch - epochi;
    date0 = epochi + 0.0101;
    date1 = epochi + 1.0101;
    dj0 = dt2jd (date0, time0);
    dj1 = dt2jd (date1, time0);
    dj = dj0 + epochf * (dj1 - dj0);
    jd2dt (dj, date, time);
    return;
}


/* EPB2DT-- convert from Besselian epoch to date, time as yyyy.mmdd hh.mmsss */

void
epb2dt (epoch, date, time)

double	epoch;	/* Besselian epoch (fractional 365.242198781-day years) */
double	*date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	*time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj;		/* Julian date */
    dj = epb2jd (epoch);
    jd2dt (dj, date, time);
}


/* EPJ2DT-- convert from Julian epoch to date, time as yyyy.mmdd hh.mmsss */

void
epj2dt (epoch, date, time)

double	epoch;	/* Julian epoch (fractional 365.25-day years) */
double	*date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	*time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double dj;		/* Julian date */
    dj = epj2jd (epoch);
    jd2dt (dj, date, time);
}


/* FD2JD-- convert FITS standard date to Julian date */

double
fd2jd (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double date, time;

    fd2dt (string, &date, &time);
    return (dt2jd (date, time));
}


/* FD2MJD-- convert FITS standard date to modified Julian date */

double
fd2mjd (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    return (fd2jd (string) - 2400000.5);
}


/* FD2TSU-- convert from FITS date to Unix seconds since 1970-01-01T0:00 */

time_t
fd2tsu (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double date, time;
    fd2dt (string, &date, &time);
    return (dt2tsu (date, time));
}


/* FD2TSI-- convert from FITS date to IRAF seconds since 1980-01-01T0:00 */

int
fd2tsi (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double date, time;
    fd2dt (string, &date, &time);
    return (dt2tsi (date, time));
}


/* FD2TS-- convert FITS standard date to seconds since 1950 */

double
fd2ts (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double date, time;
    fd2dt (string, &date, &time);
    return (dt2ts (date, time));
}


/* FD2FD-- convert any FITS standard date to ISO FITS standard date */

char *
fd2fd (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double date, time;
    fd2dt (string, &date, &time);
    return (dt2fd (date, time));
}


/* FD2OF-- convert any FITS standard date to old FITS standard date time */

char *
fd2of (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;

    fd2i (string,&iyr,&imon,&iday,&ihr,&imn,&sec, 3);

    /* Convert to old FITS date format */
    string = (char *) calloc (32, sizeof (char));
    if (iyr < 1900)
	sprintf (string, "*** date out of range ***");
    else if (iyr < 2000)
	sprintf (string, "%02d/%02d/%02d %02d:%02d:%06.3f",
		 iday, imon, iyr-1900, ihr, imn, sec);
    else if (iyr < 2900.0)
	sprintf (string, "%02d/%02d/%3d %02d:%02d:%6.3f",
		 iday, imon, iyr-1900, ihr, imn, sec);
    else
	sprintf (string, "*** date out of range ***");
    return (string);
}


/* TAI-UTC from the U.S. Naval Observatory */
/* ftp://maia.usno.navy.mil/ser7/tai-utc.dat */
static double taijd[23]={2441317.5, 2441499.5, 2441683.5, 2442048.5, 2442413.5,
	      2442778.5, 2443144.5, 2443509.5, 2443874.5, 2444239.5, 2444786.5,
	      2445151.5, 2445516.5, 2446247.5, 2447161.5, 2447892.5, 2448257.5,
	      2448804.5, 2449169.5, 2449534.5, 2450083.5, 2450630.5, 2451179.5};
static double taidt[23]={10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,
	   20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0};
static double dttab[173]={13.7,13.4,13.1,12.9,12.7,12.6,12.5,12.5,12.5,12.5,
	   12.5,12.5,12.5,12.5,12.5,12.5,12.5,12.4,12.3,12.2,12.0,11.7,11.4,
	   11.1,10.6,10.2, 9.6, 9.1, 8.6, 8.0, 7.5, 7.0, 6.6, 6.3, 6.0, 5.8,
	    5.7, 5.6, 5.6, 5.6, 5.7, 5.8, 5.9, 6.1, 6.2, 6.3, 6.5, 6.6, 6.8,
            6.9, 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.7, 7.8, 7.8,7.88,7.82,
	   7.54, 6.97, 6.40, 6.02, 5.41, 4.10, 2.92, 1.82, 1.61, 0.10,-1.02,
	  -1.28,-2.69,-3.24,-3.64,-4.54,-4.71,-5.11,-5.40,-5.42,-5.20,-5.46,
	  -5.46,-5.79,-5.63,-5.64,-5.80,-5.66,-5.87,-6.01,-6.19,-6.64,-6.44,
	  -6.47,-6.09,-5.76,-4.66,-3.74,-2.72,-1.54,-0.02, 1.24, 2.64, 3.86,
	   5.37, 6.14, 7.75, 9.13,10.46,11.53,13.36,14.65,16.01,17.20,18.24,
	  19.06,20.25,20.95,21.16,22.25,22.41,23.03,23.49,23.62,23.86,24.49,
	  24.34,24.08,24.02,24.00,23.87,23.95,23.86,23.93,23.73,23.92,23.96,
	  24.02,24.33,24.83,25.30,25.70,26.24,26.77,27.28,27.78,28.25,28.71,
	  29.15,29.57,29.97,30.36,30.72,31.07,31.35,31.68,32.18,32.68,33.15,
	  33.59,34.00,34.47,35.03,35.73,36.54,37.43,38.29,39.20,40.18,41.17,
	  42.23};


/* ET2FD-- convert from ET (or TDT or TT) in FITS format to UT in FITS format */

char *
et2fd (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double dj0, dj, tsec, dt;

    dj0 = fd2jd (string);
    dt = utdt (dj0);
    dj = dj0 - (dt / 86400.0);
    dt = utdt (dj);
    tsec = fd2ts (string);
    tsec = tsec - dt;
    return (ts2fd (tsec));
}


/* FD2ET-- convert from UT in FITS format to ET (or TDT or TT) in FITS format */

char *
fd2et (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double dj, tsec, dt;

    dj = fd2jd (string);
    dt = utdt (dj);
    tsec = fd2ts (string);
    tsec = tsec + dt;
    return (ts2fd (tsec));
}


/* DT2ET-- convert from UT as yyyy.mmdd hh.mmssss to ET in same format */

void
dt2et (date, time)
double	*date;	/* Date as yyyy.mmdd */
double	*time;	/* Time as hh.mmssxxxx
		 *if time<0, it is time as -(fraction of a day) */
{
    double dj, dt, tsec;

    dj = dt2jd (*date, *time);
    dt = utdt (dj);
    tsec = dt2ts (*date, *time);
    tsec = tsec + dt;
    ts2dt (tsec, date, time);
    return;
}


/* EDT2DT-- convert from ET as yyyy.mmdd hh.mmssss to UT in same format */

void
edt2dt (date, time)
double	*date;	/* Date as yyyy.mmdd */
double	*time;	/* Time as hh.mmssxxxx
		 *if time<0, it is time as -(fraction of a day) */
{
    double dj, dt, tsec, tsec0;

    dj = dt2jd (*date, *time);
    dt = utdt (dj);
    tsec0 = dt2ts (*date, *time);
    tsec = tsec0 + dt;
    dj = ts2jd (tsec);
    dt = utdt (dj);
    tsec = tsec0 + dt;
    ts2dt (tsec, date, time);
    return;
}


/* JD2JED-- convert from Julian Date to Julian Ephemeris Date */

double
jd2jed (dj)

double dj;	/* Julian Date */
{
    double dt;

    dt = utdt (dj);
    return (dj + (dt / 86400.0));
}


/* JED2JD-- convert from Julian Ephemeris Date to Julian Date */

double
jed2jd (dj)

double dj;	/* Julian Ephemeris Date */
{
    double dj0, dt;

    dj0 = dj;
    dt = utdt (dj);
    dj = dj0 - (dt / 86400.0);
    dt = utdt (dj);
    return (dj - (dt / 86400.0));
}


/* TS2ETS-- convert from UT in seconds since 1950-01-01 to ET in same format */

double
ts2ets (tsec)

double tsec;
{
    double dj, dt;

    dj = ts2jd (tsec);
    dt = utdt (dj);
    return (tsec + dt);
}


/* ETS2TS-- convert from ET in seconds since 1950-01-01 to UT in same format */

double
ets2ts (tsec)

double tsec;
{
    double dj, dj0, dt;

    dj0 = ts2jd (tsec);
    dt = utdt (dj0);
    dj = dj0 - (dt / 86400.0);
    dt = utdt (dj);
    return (tsec - dt);
}


/* UTDT-- Compute difference between UT and dynamical time (ET-UT) */

double
utdt (dj)

double dj;	/* Julian Date (UT) */
{
    double dt, date, time, ts, ts1, ts0, date0, yfrac, diff, cj;
    int i, iyr, iyear;

    /* If after 1972-01-01, use tabulated TAI-UT */
    if (dj >= 2441317.5) {
	dt = 0.0;
	for (i = 22;  i > 0; i--) {
	    if (dj >= taijd[i])
		dt = taidt[i];
	    }
	dt = dt + 32.84;
	}

    /* For 1800-01-01 to 1972-01-01, use table of ET-UT from AE */
    else if (dj >= 2378496.5) {
	jd2dt (dj, &date, &time);
	ts = jd2ts (dj);
	iyear = (int) date;
	iyr = iyear - 1800;
	date0 = (double) iyear + 0.0101;
	ts0 = dt2ts (date0, 0.0);
	date0 = (double) (iyear + 1) + 0.0101;
	ts1 = dt2ts (date0, 0.0);
	yfrac = (ts - ts0) / (ts1 - ts0);
	diff = dttab[iyr+1] - dttab[iyr];
	dt = dttab[iyr] + (diff * yfrac);
	}

    /* Compute back to 1600 using formula from McCarthy and Babcock (1986) */
    else if (dj >= 2305447.5) {
	cj = (dj - 2378496.5) / 36525.0;
	dt = 5.156 + 13.3066 * (cj - 0.19) * (cj - 0.19);
	}

    /* Compute back to 948 using formula from Stephenson and Morrison (1984) */
    else if (dj >= 2067309.5) {
	cj = (dj - 2378496.5) / 36525.0;
	dt = 25.5 * cj * cj;
	}

    /*Compute back to 390 BC using formula from Stephenson and Morrison (1984)*/
    else if (dj >= 0.0) {
	cj = (dj = 2378496.5) / 36525.0;
	dt = 1360.0 + (320.0 * cj) + (44.3 * cj * cj);
	}

    else
	dt = 0.0;
    return (dt);
}


/* FD2OFD-- convert any FITS standard date to old FITS standard date */

char *
fd2ofd (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;

    fd2i (string,&iyr,&imon,&iday,&ihr,&imn,&sec, 3);

    /* Convert to old FITS date format */
    string = (char *) calloc (32, sizeof (char));
    if (iyr < 1900)
	sprintf (string, "*** date out of range ***");
    else if (iyr < 2000)
	sprintf (string, "%02d/%02d/%02d", iday, imon, iyr-1900);
    else if (iyr < 2900.0)
	sprintf (string, "%02d/%02d/%3d", iday, imon, iyr-1900);
    else
	sprintf (string, "*** date out of range ***");
    return (string);
}


/* FD2OFT-- convert any FITS standard date to old FITS standard time */

char *
fd2oft (string)

char *string;	/* FITS date string, which may be:
			fractional year
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;

    fd2i (string,&iyr,&imon,&iday,&ihr,&imn,&sec, 3);

    /* Convert to old FITS date format */
    string = (char *) calloc (32, sizeof (char));
    sprintf (string, "%02d:%02d:%06.3f", ihr, imn, sec);
    return (string);
}


/* FD2DT-- convert FITS standard date to date, time as yyyy.mmdd hh.mmsss */

void
fd2dt (string, date, time)

char *string;	/* FITS date string, which may be:
		    fractional year
		    dd/mm/yy (FITS standard before 2000)
		    dd-mm-yy (nonstandard use before 2000)
		    yyyy-mm-dd (FITS standard after 1999)
		    yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
double	*date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	*time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;

    fd2i (string,&iyr,&imon,&iday,&ihr,&imn,&sec, 4);

    /* Convert date to yyyy.mmdd */
    if (iyr < 0) {
	*date = (double) (-iyr) + 0.01 * (double) imon + 0.0001 * (double) iday;
	*date = -(*date);
	}
    else
	*date = (double) iyr + 0.01 * (double) imon + 0.0001 * (double) iday;

    /* Convert time to hh.mmssssss */
    *time = (double) ihr + 0.01 * (double) imn + 0.0001 * sec;

    return;
}


/* FD2EP-- convert from FITS standard date to fractional year */

double
fd2ep (string)

char *string;	/* FITS date string, which may be:
			yyyy.ffff (fractional year)
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard FITS use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */

{
    double dj;		/* Julian date */
    dj = fd2jd (string);
    if (dj < 1.0)
	return (dj / 365.2422);
    else
	return (jd2ep (dj));
}


/* FD2EPB-- convert from FITS standard date to Besselian epoch */

double
fd2epb (string)

char *string;	/* FITS date string, which may be:
			yyyy.ffff (fractional year)
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard FITS use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */

{
    double dj;		/* Julian date */
    dj = fd2jd (string);
    if (dj < 1.0)
	return (dj / 365.242198781);
    else
	return (jd2epb (dj));
}


/* FD2EPJ-- convert from FITS standard date to Julian epoch */

double
fd2epj (string)

char *string;	/* FITS date string, which may be:
			yyyy.ffff (fractional year)
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard FITS use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */

{
    double dj;		/* Julian date */
    dj = fd2jd (string);
    if (dj < 1.0)
	return (dj / 365.25);
    else
	return (jd2epj (dj));
}


/* DT2TSU-- convert from date and time to Unix seconds since 1970-01-01T0:00 */

time_t
dt2tsu (date,time)

double	date;	/* Date as yyyy.mmdd */
double	time;	/* Time as hh.mmssxxxx
		 *if time<0, it is time as -(fraction of a day) */
{
    return ((time_t)(dt2ts (date, time) - 631152000.0));
}


/* DT2TSI-- convert from date and time to IRAF seconds since 1980-01-01T0:00 */

int
dt2tsi (date,time)

double	date;	/* Date as yyyy.mmdd */
double	time;	/* Time as hh.mmssxxxx
		 *if time<0, it is time as -(fraction of a day) */
{
    return ((int)(dt2ts (date, time) - 946684800.0));
}



/* DT2TS-- convert from date, time as yyyy.mmdd hh.mmsss to sec since 1950.0 */

double
dt2ts (date,time)

double	date;	/* Date as yyyy.mmdd
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    double tsec; /* Seconds past 1950.0 (returned) */

    double dh,dm,dd;
    int iy,im,id;

/* Calculate the number of full years, months, and days already
 * elapsed since 0h, March 1, -1 (up to most recent midnight). */

    /* convert time of day to elapsed seconds */

    /* If time is < 0, it is assumed to be a fractional day */
    if (time < 0.0)
	tsec = time * -86400.0;
    else {
	dh = (int) (time + 0.0000000001);
	dm = (int) (((time - dh) * 100.0) + 0.0000000001);
	tsec = (time * 10000.0) - (dh * 10000.0) - (dm * 100.0);
	tsec = (int) (tsec * 100000.0 + 0.0001) / 100000.0;
	tsec = tsec + (dm * 60.0) + (dh * 3600.0);
	}


    /* Calculate the number of full months elapsed since
     * the current or most recent March */
    if (date >= 0.0301) {
	iy = (int) (date + 0.0000000001);
	im = (int) (((date - (double) (iy)) * 10000.0) + 0.00000001);
	id = im % 100;
	im = (im / 100) + 9;
	if (im < 12) iy = iy - 1;
	im = im % 12;
	id = id - 1;

	/* starting with March as month 0 and ending with the following
	 * February as month 11, the calculation of the number of days
	 * per month reduces to a simple formula. the following statement
	 * determines the number of whole days elapsed since 3/1/-1 and then
	 * subtracts the 712163 days between then and 1/1/1950.  it converts
	 * the result to seconds and adds the accumulated seconds above. */
	id = id + ((im+1+im/6+im/11)/2 * 31) + ((im-im/6-im/11)/2 * 30) +
	     (iy / 4) - (iy / 100) + (iy / 400);
	dd = (double) id + (365.0 * (double) iy) - 712163.0;
	tsec = tsec + (dd * 86400.0);
	}

    return (tsec);
}


/* TS2DT-- convert seconds since 1950.0 to date, time as yyyy.mmdd hh.mmssss */

void
ts2dt (tsec,date,time)

double	tsec;	/* Seconds past 1950.0 */
double	*date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	*time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
{
    int iyr,imon,iday,ihr,imn;
    double sec;

    ts2i (tsec,&iyr,&imon,&iday,&ihr,&imn,&sec, 4);

    /* Convert date to yyyy.mmdd */
    if (iyr < 0) {
	*date = (double) (-iyr) + 0.01 * (double) imon + 0.0001 * (double) iday;
	*date = -(*date);
	}
    else
	*date = (double) iyr + 0.01 * (double) imon + 0.0001 * (double) iday;

    /* Convert time to hh.mmssssss */
    *time = (double) ihr + 0.01 * (double) imn + 0.0001 * sec;

    return;
}


/* TSI2DT-- Convert seconds since 1980-01-01 to date yyyy.ddmm, time hh.mmsss */

void
tsi2dt (isec,date,time)

int	isec;	/* Seconds past 1980-01-01 */
double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    ts2dt (tsi2ts (isec), date, time);
}


/* TSI2FD-- Convert seconds since 1980-01-01 to FITS standard date string */

char *
tsi2fd (isec)

int	isec;	/* Seconds past 1980-01-01 */
{
    return (ts2fd (tsi2ts (isec)));
}


/* TSI2TS-- Convert seconds since 1980-01-01 to seconds since 1950-01-01 */

double
tsi2ts (isec)
int	isec;	/* Seconds past 1980-01-01 */
{
    return ((double) isec + 946684800.0);
}


/* TSU2FD-- Convert seconds since 1970-01-01 to FITS standard date string */

char *
tsu2fd (isec)
time_t	isec;	/* Seconds past 1970-01-01 */
{
    return (ts2fd (tsu2ts (isec)));
}


/* TSU2DT-- Convert seconds since 1970-01-01 to date yyyy.ddmm, time hh.mmsss */

void
tsu2dt (isec,date,time)
time_t	isec;	/* Seconds past 1970-01-01 */
double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    ts2dt (tsu2ts (isec), date, time);
}


/* TSU2TS-- Convert seconds since 1970-01-01 to seconds since 1950-01-01 */

double
tsu2ts (isec)
time_t	isec;	/* Seconds past 1970-01-01 */
{
    return ((double) isec + 631152000.0);
}

/* TSU2TSI-- UT seconds since 1970-01-01 to local seconds since 1980-01-01 */

int
tsu2tsi (isec)
time_t	isec;	/* Seconds past 1970-01-01 */
{
    double date, time;
    struct tm *ts;

    /* Get local time  from UT seconds */
    ts = localtime (&isec);
    if (ts->tm_year < 1000)
	date = (double) (ts->tm_year + 1900);
    else
	date = (double) ts->tm_year;
    date = date + (0.01 * (double) (ts->tm_mon + 1));
    date = date + (0.0001 * (double) ts->tm_mday);
    time = (double) ts->tm_hour;
    time = time + (0.01 * (double) ts->tm_min);
    time = time + (0.0001 * (double) ts->tm_sec);
    return ((int)(dt2ts (date, time) - 631152000.0));
}


/* TS2FD-- convert seconds since 1950.0 to FITS date, yyyy-mm-ddThh:mm:ss.ss */

char *
ts2fd (tsec)

double	tsec;	/* Seconds past 1950.0 */
{
    double date, time;

    ts2dt (tsec, &date, &time);
    return (dt2fd (date, time));
}


/* DT2I-- convert vigesimal date and time to year month day hours min sec */

void
dt2i (date, time, iyr, imon, iday, ihr, imn, sec, ndsec)

double	date;	/* Date as yyyy.mmdd (returned)
		    yyyy = calendar year (e.g. 1973)
		    mm = calendar month (e.g. 04 = april)
		    dd = calendar day (e.g. 15) */
double	time;	/* Time as hh.mmssxxxx (returned)
		    *if time<0, it is time as -(fraction of a day)
		    hh = hour of day (0 .le. hh .le. 23)
		    nn = minutes (0 .le. nn .le. 59)
		    ss = seconds (0 .le. ss .le. 59)
		  xxxx = tenths of milliseconds (0 .le. xxxx .le. 9999) */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */

{
    double t,d;

    t = time;
    if (date < 0.0)
	d = -date;
    else
	d = date;

    /* Extract components of time */
    *ihr = dint (t + 0.000000001);
    t = 100.0 * (t - (double) *ihr);
    *imn = dint (t + 0.0000001);
    *sec = 100.0 * (t - (double) *imn);

    /* Extract components of date */
    *iyr = dint (d + 0.00001);
    d = 100.0 * (d - (double) *iyr);
    if (date < 0.0)
	*iyr = - *iyr;
    *imon = dint (d + 0.001);
    d = 100.0 * (d - (double) *imon);
    *iday = dint (d + 0.1);

   /* Make sure date and time are legal */
    fixdate (iyr, imon, iday, ihr, imn, sec, ndsec);

    return;
}


/* FD2I-- convert from FITS standard date to year, mon, day, hours, min, sec */

void
fd2i (string, iyr, imon, iday, ihr, imn, sec, ndsec)

char	*string; /* FITS date string, which may be:
			yyyy.ffff (fractional year)
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard FITS use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */

{
    double tsec, fday, hr, mn;
    int i;
    char *sstr, *dstr, *tstr, *cstr, *nval, *fstr;

    /* Initialize all returned data to zero */
    *iyr = 0;
    *imon = 0;
    *iday = 0;
    *ihr = 0;
    *imn = 0;
    *sec = 0.0;

    /* Return if no input string */
    if (string == NULL)
	return;

    /* Check for various non-numeric characters */
    sstr = strchr (string,'/');
    dstr = strchr (string,'-');
    if (dstr == string)
	dstr = strchr (string+1, '-');
    fstr = strchr (string, '.');
    tstr = strchr (string,'T');
    if (tstr == NULL)
	tstr = strchr (string, 'Z');
    if (fstr != NULL && tstr != NULL && fstr > tstr)
	fstr = NULL;
    cstr = strchr (string,':');

    /* Original FITS date format: dd/mm/yy */
    if (sstr > string) {
	*sstr = '\0';
	*iday = (int) atof (string);
	if (*iday > 31) {
	    *iyr = *iday;
	    if (*iyr >= 0 && *iyr <= 49)
		*iyr = *iyr + 2000;
	    else if (*iyr < 1000)
		*iyr = *iyr + 1900;
	    *sstr = '/';
	    nval = sstr + 1;
	    sstr = strchr (nval,'/');
	    if (sstr > string) {
		*sstr = '\0';
		*imon = (int) atof (nval);
		*sstr = '/';
		nval = sstr + 1;
		*iday = (int) atof (nval);
		}
	    else
		return;
	    }
	else {
	    *sstr = '/';
	    nval = sstr + 1;
	    sstr = strchr (nval,'/');
	    if (sstr == NULL)
		sstr = strchr (nval,'-');
	    if (sstr > string) {
		*sstr = '\0';
		*imon = (int) atof (nval);
		*sstr = '/';
		nval = sstr + 1;
		*iyr = (int) atof (nval);
		if (*iyr >= 0 && *iyr <= 49)
		    *iyr = *iyr + 2000;
		else if (*iyr < 1000)
		    *iyr = *iyr + 1900;
		}
	    else
		return;
	    }
	}

    /* New FITS date format: yyyy-mm-ddThh:mm:ss[.sss] */
    else if (dstr > string) {
	*dstr = '\0';
	*iyr = (int) atof (string);
	*dstr = '-';
	nval = dstr + 1;
	dstr = strchr (nval,'-');
	*imon = 1;
	*iday = 1;

	/* Decode year, month, and day */
	if (dstr > string) {
	    *dstr = '\0';
	    *imon = (int) atof (nval);
	    *dstr = '-';
	    nval = dstr + 1;
	    if (tstr > string)
		*tstr = '\0';
	    *iday = (int) atof (nval);

	    /* If fraction of a day is present, turn it into a time */
	    if (fstr != NULL) {
		fday = atof (fstr);
		hr = fday * 24.0;
		*ihr = (int) hr;
		mn = 60.0 * (hr - (double) *ihr);
		*imn = (int) mn;
		*sec = 60.0 * (mn - (double) *imn);
		}

	    if (tstr > string)
		*tstr = 'T';
	    }

	/* If date is > 31, it is really year in old format */
	if (*iday > 31) {
	    i = *iyr;
	    if (*iday < 100)
		*iyr = *iday + 1900;
	    else
		*iyr = *iday;
	    *iday = i;
	    }
	}

    /* In rare cases, a FITS time is entered as an epoch */
    else if (tstr == NULL && cstr == NULL && isnum (string)) {
	tsec = ep2ts (atof (string));
	ts2i (tsec,iyr,imon,iday,ihr,imn,sec, ndsec);
	return;
	}

    /* Extract time, if it is present */
    if (tstr > string || cstr > string) {
	if (tstr > string)
	    nval = tstr + 1;
	else
	    nval = string;
	cstr = strchr (nval,':');
	if (cstr > string) {
	    *cstr = '\0';
	    *ihr = (int) atof (nval);
	    *cstr = ':';
	    nval = cstr + 1;
	    cstr = strchr (nval,':');
	    if (cstr > string) {
		*cstr = '\0';
		*imn = (int) atof (nval);
		*cstr = ':';
		nval = cstr + 1;
		*sec = atof (nval);
		}
	    else
		*imn = (int) atof (nval);
	    }
	else
	    *ihr = (int) atof (nval);
	}
    else
	ndsec = -1;

   /* Make sure date and time are legal */
    fixdate (iyr, imon, iday, ihr, imn, sec, ndsec);

    return;
}


/* TS2I-- convert sec since 1950.0 to year month day hours minutes seconds */

void
ts2i (tsec,iyr,imon,iday,ihr,imn,sec, ndsec)

double	tsec;	/* seconds since 1/1/1950 0:00 */
int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */

{
    double t,days, ts, dts;
    int nc,nc4,nly,ny,m,im;

    /* Round seconds to 0 - 4 decimal places */
    ts = tsec + 61530883200.0;
    if (ts < 0.0)
	dts = -0.5;
    else
	dts = 0.5;
    if (ndsec < 1)
	t = dint (ts + dts) * 10000.0;
    else if (ndsec < 2)
	t = dint (ts * 10.0 + dts) * 1000.0;
    else if (ndsec < 3)
	t = dint (ts * 100.0 + dts) * 100.0;
    else if (ndsec < 4)
	t = dint (ts * 1000.0 + dts) * 10.0;
    else
	t = dint (ts * 10000.0 + dts);
    ts = t / 10000.0;

    /* Time of day (hours, minutes, seconds */
    *ihr = (int) (dmod (ts/3600.0, 24.0));
    *imn = (int) (dmod (ts/60.0, 60.0));
    *sec = dmod (ts, 60.0);

    /* Number of days since 0 hr 0/0/0000 */
    days = dint ((t / 864000000.0) + 0.000001);

    /* Number of leap centuries (400 years) */
    nc4 = (int) ((days / 146097.0) + 0.00001);

    /* Number of centuries since last /400 */
    days = days - (146097.0 * (double) (nc4));
    nc = (int) ((days / 36524.0) + 0.000001);
    if (nc > 3) nc = 3;

    /* Number of leap years since last century */
    days = days - (36524.0 * nc);
    nly = (int) ((days / 1461.0) + 0.0000000001);

    /* Number of years since last leap year */
    days = days - (1461.0 * (double) nly);
    ny = (int) ((days / 365.0) + 0.00000001);
    if (ny > 3) ny = 3;

    /* Day of month */
    days = days - (365.0 * (double) ny);
    if (days < 0) {
	m = 0;
	*iday = 29;
	}
    else {
	*iday = (int) (days + 0.00000001) + 1;
	for (m = 1; m <= 12; m++) {
	    im = (m + ((m - 1) / 5)) % 2;
	    /* fprintf (stderr,"%d %d %d %d\n", m, im, *iday, nc); */
	    if (*iday-1 < im+30) break;
	    *iday = *iday - im - 30;
	    }
	}

    /* Month */
    *imon = ((m+1) % 12) + 1;

    /* Year */
    *iyr = nc4*400 + nc*100 + nly*4 + ny + m/11;

   /* Make sure date and time are legal */
    fixdate (iyr, imon, iday, ihr, imn, sec, ndsec);

    return;
}


/* UT2DOY-- Current Universal Time as year, day of year */

void
ut2doy (year, doy)

int	*year;	/* Year (returned) */
double	*doy;	/* Day of year (returned) */
{
    double date, time;
    ut2dt (&date, &time);
    dt2doy (date, time, year, doy);
    return;
}


/* UT2DT-- Current Universal Time as date (yyyy.mmdd) and time (hh.mmsss) */

void
ut2dt(date, time)

double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    time_t tsec;
    struct timeval tp;
    struct timezone tzp;
    struct tm *ts;

    gettimeofday (&tp,&tzp);

    tsec = tp.tv_sec;
    ts = gmtime (&tsec);

    if (ts->tm_year < 1000)
	*date = (double) (ts->tm_year + 1900);
    else
	*date = (double) ts->tm_year;
    *date = *date + (0.01 * (double) (ts->tm_mon + 1));
    *date = *date + (0.0001 * (double) ts->tm_mday);
    *time = (double) ts->tm_hour;
    *time = *time + (0.01 * (double) ts->tm_min);
    *time = *time + (0.0001 * (double) ts->tm_sec);

    return;
}


/* UT2EP-- Return current Universal Time as fractional year */

double
ut2ep()
{
    return (jd2ep (ut2jd()));
}


/* UT2EPB-- Return current Universal Time as Besselian epoch */

double
ut2epb()
{
    return (jd2epb (ut2jd()));
}


/* UT2EPJ-- Return current Universal Time as Julian epoch */

double
ut2epj()
{
    return (jd2epj (ut2jd()));
}


/* UT2FD-- Return current Universal Time as FITS ISO date string */

char *
ut2fd()
{
    int year, month, day, hour, minute, second;
    time_t tsec;
    struct timeval tp;
    struct timezone tzp;
    struct tm *ts;
    char *isotime;

    gettimeofday (&tp,&tzp);
    tsec = tp.tv_sec;
    ts = gmtime (&tsec);

    year = ts->tm_year;
    if (year < 1000)
	year = year + 1900;
    month = ts->tm_mon + 1;
    day = ts->tm_mday;
    hour = ts->tm_hour;
    minute = ts->tm_min;
    second = ts->tm_sec; 

    isotime = (char *) calloc (32, sizeof (char));
    sprintf (isotime, "%04d-%02d-%02dT%02d:%02d:%02d",
		      year, month, day, hour, minute, second);
    return (isotime);
}


/* UT2JD-- Return current Universal Time as Julian Date */

double
ut2jd()
{
    return (fd2jd (ut2fd()));
}


/* UT2MJD-- convert current UT to Modified Julian Date */

double
ut2mjd ()

{
    return (ut2jd() - 2400000.5);
}

/* UT2TS-- current Universal Time as IRAF seconds since 1950-01-01T00:00 */

double
ut2ts()
{
    double tsec;
    char *datestring;
    datestring = ut2fd();
    tsec = fd2ts (datestring);
    free (datestring);
    return (tsec);
}


/* UT2TSI-- current Universal Time as IRAF seconds since 1980-01-01T00:00 */

int
ut2tsi()
{
    return ((int)(ut2ts() - 946684800.0));
}


/* UT2TSU-- current Universal Time as IRAF seconds since 1970-01-01T00:00 */

time_t
ut2tsu()
{
    return ((time_t)(ut2ts () - 631152000.0));
}


/* FD2GST-- convert from FITS date to Greenwich Sidereal Time */

char *
fd2gst (string)

char	*string;	/* FITS date string, which may be:
			  fractional year
			  dd/mm/yy (FITS standard before 2000)
			  dd-mm-yy (nonstandard use before 2000)
			  yyyy-mm-dd (FITS standard after 1999)
			  yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double tsec, gsec, date, time;

    tsec = fd2ts (string);
    gsec = ts2gst (tsec);
    ts2dt (gsec, &date, &time);
    date = 0.0;
    return (dt2fd (date, time));
}


/* DT2GST-- convert from UT as yyyy.mmdd hh.mmssss to Greenwich Sidereal Time*/

void
dt2gst (date, time)
double  *date;  /* Date as yyyy.mmdd */
double  *time;  /* Time as hh.mmssxxxx
                 *if time<0, it is time as -(fraction of a day) */
{
    double tsec, gsec;

    tsec = dt2ts (*date, *time);
    gsec = ts2gst (tsec);
    ts2dt (gsec, date, time);
    *date = 0.0;
    return;
}


/* TS2GST - calculate Greenwich Sidereal Time given Universal Time
 *	    in seconds since 1951-01-01T0:00:00
 *	    Return UT seconds to start of day plus sidereal time of day
 */

double
ts2gst (tsec)

double tsec;	/* time since 1950.0 in UT seconds */
{
    double dpsi;	/* Nutation in longitude (radians) */
    double deps;	/* Nutation in obliquity (radians) */

    double gst;	/* Greenwich Sidereal Time in seconds since 0:00 */
    double tsd,ts,esec,obl,eqnx, dj;
    int its;

    /* Elapsed time as of 0:00 UT */
    if (tsec >= 0.0) {
	its = (int) (tsec + 0.5);
	tsd = (double) (its % 86400);
	ts = tsec - tsd;
	}
    else {
	its = (int) (-tsec + 0.5);
	tsd = (double) (86400 - (its % 86400));
	ts = tsec - tsd;
	}

    /* Mean sidereal time */
    gst = ts2mst (tsec);

    /* Ephemeris Time (TDB or TT)*/
    esec = ts2ets (tsec);

    /* Nutation and obliquity */
    dj = ts2jd (esec);
    compnut (dj, &dpsi, &deps, &obl);

    /* Correct obliquity for nutation */
    obl = obl + deps;

    /* Equation of the equinoxes */
    eqnx = (dpsi * cos (obl)) * 13750.98708;

    /* Apparent sidereal time at 0:00 ut */
    gst = gst + eqnx;

    /* Current sidereal time */
    gst = gst + (tsd * 1.0027379093);
    gst = dmod (gst,86400.0);

    return (gst + ts);
}


/* FD2MST-- convert from FITS date Mean Sidereal Time */

char *
fd2mst (string)

char	*string;	/* FITS date string, which may be:
			  fractional year
			  dd/mm/yy (FITS standard before 2000)
			  dd-mm-yy (nonstandard use before 2000)
			  yyyy-mm-dd (FITS standard after 1999)
			  yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */
{
    double tsec, gsec, date, time;

    tsec = fd2ts (string);
    gsec = ts2mst (tsec);
    ts2dt (gsec, &date, &time);
    date = 0.0;
    return (dt2fd (date, time));
}


/* DT2MST-- convert from UT as yyyy.mmdd hh.mmssss to Mean Sidereal Time
	    in the same format */

void
dt2mst (date, time)
double  *date;  /* Date as yyyy.mmdd */
double  *time;  /* Time as hh.mmssxxxx
                 *if time<0, it is time as -(fraction of a day) */
{
    double tsec, gsec;
    tsec = dt2ts (*date, *time);
    gsec = ts2mst (tsec);
    ts2dt (gsec, date, time);
    *date = 0.0;
    return;
}


/* TS2MST - calculate Greenwich Mean Sidereal Time given Universal Time
 *	    in seconds since 1951-01-01T0:00:00
 */

double
ts2mst (tsec)

double tsec;	/* time since 1950.0 in UT seconds */
{
    double gst;	/* Greenwich Sidereal Time in seconds since 0:00 */
    double t, tsd, ts;
    int its;

    /* Elapsed time as of 0:00 UT */
    if (tsec >= 0.0) {
	its = (int) (tsec + 0.5);
	tsd = (double) (its % 86400);
	ts = tsec - tsd;
	}
    else {
	its = (int) (-tsec + 0.5);
	tsd = (double) (86400 - (its % 86400));
	ts = tsec - tsd;
	}

    /* Mean sidereal time */
    t = (18262.5 + (ts / 86400.0)) / 36525.0;
    gst = 23925.8360 + (8640184.5420 * t) + (0.0929 * t * t);

    return (gst + ts);
}


/* Compute the longitude and obliquity components of nutation and
 * mean obliquity from the IAU 1980 theory
 * References:
 *    Final Report of the IAU Working Group on Nutation,
 *    Chairman P.K.Seidelmann, 1980.
 *    Kaplan,G.H., 1981, USNO Circular No. 163, pa3-6.
 *
 * From Fortran code by P.T. Wallace   Starlink   september 1987
 */

void
compnut (dj, dpsi, deps, eps0)

double dj;	/* TDB (loosely ET or TT) as Julian Date */
double *dpsi;	/* Nutation in longitude (returned) */
double *deps;	/* Nutation in obliquity (returned) */
double *eps0;	/* Mean obliquity (returned) */
{
    double t2as,as2r,u2r;
    double t,el,el2,el3;
    double elp,elp2;
    double f,f2,f4;
    double d,d2,d4;
    double om,om2;
    double dp,de;
    double a;

    /* Turns to arc seconds */
    t2as = 1296000.0;

    /* Arc seconds to radians */
    as2r = 0.00004848136811095359949;

    /* Units of 0.0001 arcsec to radians */
    u2r = as2r / 10000.0;

    /* Basic epoch J2000.0 to current epoch in Julian Centuries */
    t = (dj - 2400000.5  - 51544.5 ) / 36525.0;

    /* Fundamental arguments in the FK5 reference system */

    /* mean longitude of the moon minus mean longitude of the moon's perigee */
    el = as2r*(485866.733 + (1325.0 * t2as+715922.633 + (31.310 +0.064*t)*t)*t);

    /* mean longitude of the sun minus mean longitude of the sun's perigee */
    elp = as2r*(1287099.804 + (99.0 * t2as+1292581.224 + (-0.577 -0.012*t)*t)*t);

    /* mean longitude of the moon minus mean longitude of the moon's node */
    f = as2r*(335778.877 + (1342.0 * t2as+295263.137 + (-13.257 + 0.011*t)*t)*t);

    /* mean elongation of the moon from the sun */
    d = as2r*(1072261.307 + (1236.0 * t2as+1105601.328 + (-6.891 + 0.019*t)*t)*t);

    /* longitude of the mean ascending node of the lunar orbit on the */
    /*  ecliptic, measured from the mean equinox of date */
    om = as2r * (450160.280 + (-5.0 * t2as-482890.539 + (7.455 +0.008*t)*t)*t);

    /* Multiples of arguments */
    el2 = el + el;
    el3 = el2 + el;
    elp2 = elp + elp;
    f2 = f + f;
    f4 = f2 + f2;
    d2 = d + d;
    d4 = d2 + d2;
    om2 = om + om;

    /* Series for the nutation */
    dp = 0.0;
    de = 0.0;

    /* 106 */
    dp = dp + sin (elp+d);

    /* 105 */
    dp = dp - sin (f2 + d4 + om2);

    /* 104 */
    dp = dp + sin (el2 + d2);

    /* 103 */
    dp = dp - sin (el - f2 + d2);

    /* 102 */
    dp = dp - sin (el + elp - d2 + om);

    /* 101 */
    dp = dp - sin (-elp + f2 + om);

    /* 100 */
    dp = dp - sin (el - f2 - d2);

    /* 99 */
    dp = dp - sin (elp + d2);

    /* 98 */
    dp = dp - sin (f2 - d + om2);

    /* 97 */
    dp = dp - sin (-f2 + om);

    /* 96 */
    dp = dp + sin (-el - elp + d2 + om);

    /* 95 */
    dp = dp + sin (elp + f2 + om);

    /* 94 */
    dp = dp - sin (el + f2 - d2);

    /* 93 */
    dp = dp + sin(el3 + f2 - d2 + om2);

    /* 92 */
    dp = dp + sin(f4 - d2 + om2);

    /* 91 */
    dp = dp - sin(el + d2 + om);

    /* 90 */
    dp = dp - sin(el2 + f2 + d2 + om2);

    /* 89 */
    a = el2 + f2 - d2 + om;
    dp = dp + sin(a);
    de = de - cos(a);

    /* 88 */
    dp = dp + (sin(el - elp - d2));

    /* 87 */
    dp = dp + (sin(-el + f4 + om2));

    /* 86 */
    a = -el2 + f2 + d4 + om2;
    dp = dp - sin(a);
    de = de + cos(a);

    /* 85 */
    a = el + f2 + d2 + om;
    dp = dp - sin(a);
    de = de + cos(a);

    /* 84 */
    a = el + elp + f2 - d2 + om2;
    dp = dp + sin(a);
    de = de - cos(a);

    /* 83 */
    dp = dp - sin(el2 - d4);

    /* 82 */
    a = -el + f2 + d4 + om2;
    dp = dp - (2.0 * sin(a));
    de = de + cos(a);

    /* 81 */
    a = -el2 + f2 + d2 + om2;
    dp = dp + sin(a);
    de = de - cos(a);

    /* 80 */
    dp = dp - sin(el - d4);

    /* 79 */
    a = -el + om2;
    dp = dp + sin(a);
    de = de - cos(a);

    /* 78 */
    a = f2 + d + om2;
    dp = dp + (2.0 * sin(a));
    de = de - cos(a);

    /* 77 */
    dp = dp + (2.0 * sin(el3));

    /* 76 */
    a = el + om2;
    dp = dp - (2.0 * sin(a));
    de = de + cos(a);

    /* 75 */
    a = el2 + om;
    dp = dp + (2.0 * sin(a));
    de = de - cos(a);

    /* 74 */
    a =  - el + f2 - d2 + om;
    dp = dp - (2.0 * sin(a));
    de = de + cos(a);

    /* 73 */
    a = el + elp + f2 + om2;
    dp = dp + (2.0 * sin(a));
    de = de - cos(a);

    /* 72 */
    a = -elp + f2 + d2 + om2;
    dp = dp - (3.0 * sin(a));
    de = de + cos(a);

    /* 71 */
    a = el3 + f2 + om2;
    dp = dp - (3.0 * sin(a));
    de = de + cos(a);

    /* 70 */
    a = -el2 + om;
    dp = dp - (2.0 * sin(a));
    de = de + cos(a);

    /* 69 */
    a = -el - elp + f2 + d2 + om2;
    dp = dp - (3.0 * sin(a));
    de = de + cos(a);

    /* 68 */
    a = el - elp + f2 + om2;
    dp = dp - (3.0 * sin(a));
    de = de + cos(a);

    /* 67 */
    dp = dp + (3.0 * sin(el + f2));

    /* 66 */
    dp = dp - (3.0 * sin(el + elp));

    /* 65 */
    dp = dp - (4.0 * sin(d));

    /* 64 */
    dp = dp + (4.0 * sin(el - f2));

    /* 63 */
    dp = dp - (4.0 * sin(elp - d2));

    /* 62 */
    a = el2 + f2 + om;
    dp = dp - (5.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 61 */
    dp = dp + (5.0 * sin(el - elp));

    /* 60 */
    a = -d2 + om;
    dp = dp - (5.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 59 */
    a = el + f2 - d2 + om;
    dp = dp + (6.0 * sin(a));
    de = de - (3.0 * cos(a));

    /* 58 */
    a = f2 + d2 + om;
    dp = dp - (7.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 57 */
    a = d2 + om;
    dp = dp - (6.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 56 */
    a = el2 + f2 - d2 + om2;
    dp = dp + (6.0 * sin(a));
    de = de - (3.0 * cos(a));

    /* 55 */
    dp = dp + (6.0 * sin(el + d2));

    /* 54 */
    a = el + f2 + d2 + om2;
    dp = dp - (8.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 53 */
    a = -elp + f2 + om2;
    dp = dp - (7.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 52 */
    a = elp + f2 + om2;
    dp = dp + (7.0 * sin(a));
    de = de - (3.0 * cos(a));

    /* 51 */
    dp = dp - (7.0 * sin(el + elp - d2));

    /* 50 */
    a = -el + f2 + d2 + om;
    dp = dp - (10.0 * sin(a));
    de = de + (5.0 * cos(a));

    /* 49 */
    a = el - d2 + om;
    dp = dp - (13.0 * sin(a));
    de = de + (7.0 * cos(a));

    /* 48 */
    a = -el + d2 + om;
    dp = dp + (16.0 * sin(a));
    de = de - (8.0 * cos(a));

    /* 47 */
    a =  - el + f2 + om;
    dp = dp + (21.0 * sin(a));
    de = de - (10.0 * cos(a));

    /* 46 */
    dp = dp + (26.0 * sin(f2));
    de = de - cos(f2);

    /* 45 */
    a = el2 + f2 + om2;
    dp = dp - (31.0 * sin(a));
    de = de + (13.0 * cos(a));

    /* 44 */
    a = el + f2 - d2 + om2;
    dp = dp + (29.0 * sin(a));
    de = de - (12.0 * cos(a));

    /* 43 */
    dp = dp + (29.0 * sin(el2));
    de = de - cos(el2);

    /* 42 */
    a = f2 + d2 + om2;
    dp = dp - (38.0 * sin(a));
    de = de + (16.0 * cos(a));

    /* 41 */
    a = el + f2 + om;
    dp = dp - (51.0 * sin(a));
    de = de + (27.0 * cos(a));

    /* 40 */
    a =  -el + f2 + d2 + om2;
    dp = dp - (59.0 * sin(a));
    de = de + (26.0 * cos(a));

    /* 39 */
    a =  -el + om;
    dp = dp + ((-58.0 - 0.1 * t) * sin(a));
    de = de + (32.0 * cos(a));

    /* 38 */
    a = el + om;
    dp = dp + ((63.0 + 0.1 * t) * sin(a));
    de = de - (33.0 * cos(a));

    /* 37 */
    dp = dp + (63.0 * sin(d2));
    de = de - (2.0 * cos(d2));

    /* 36 */
    a =  -el + f2 + om2;
    dp = dp + (123.0 * sin(a));
    de = de - (53.0 * cos(a));

    /* 35 */
    a = el - d2;
    dp = dp - (158.0 * sin(a));
    de = de - cos(a);

    /* 34 */
    a = el + f2 + om2;
    dp = dp - (301.0 * sin(a));
    de = de + ((129.0 - 0.1 * t) * cos(a));

    /* 33 */
    a = f2 + om;
    dp = dp + ((-386.0  - 0.4 * t) * sin(a));
    de = de + (200.0 * cos(a));

    /* 32 */
    dp = dp + ((712.0  + 0.1 * t) * sin(el));
    de = de - (7.0 * cos(el));

    /* 31 */
    a = f2 + om2;
    dp = dp + ((-2274.0  - 0.2 * t) * sin(a));
    de = de + ((977.0  - 0.5 * t) * cos(a));

    /* 30 */
    dp = dp - sin(elp + f2 - d2);

    /* 29 */
    dp = dp + sin(-el + d + om);

    /* 28 */
    dp = dp + sin(elp + om2);

    /* 27 */
    dp = dp - sin(elp - f2 + d2);

    /* 26 */
    dp = dp + sin(-f2 + d2 + om);

    /* 25 */
    dp = dp + sin(el2 + elp - d2);

    /* 24 */
    dp = dp - (4.0 * sin(el - d));

    /* 23 */
    a = elp + f2 - d2 + om;
    dp = dp + (4.0 * sin(a));
    de = de - (2.0 * cos(a));

    /* 22 */
    a = el2 - d2 + om;
    dp = dp + (4.0 * sin(a));
    de = de - (2.0 * cos(a));

    /* 21 */
    a = -elp + f2 - d2 + om;
    dp = dp - (5.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 20 */
    a = -el2 + d2 + om;
    dp = dp - (6.0 * sin(a));
    de = de + (3.0 * cos(a));

    /* 19 */
    a = -elp + om;
    dp = dp - (12.0 * sin(a));
    de = de + (6.0 * cos(a));

    /* 18 */
    a = elp2 + f2 - d2 + om2;
    dp = dp + ((-16.0  + (0.1 * t)) * sin(a));
    de = de + (7.0 * cos(a));

    /* 17 */
    a = elp + om;
    dp = dp - (15.0 * sin(a));
    de = de + (9.0 * cos(a));

    /* 16 */
    dp = dp + ((17.0  - (0.1 * t)) * sin(elp2));

    /* 15 */
    dp = dp - (22.0 * sin(f2 - d2));

    /* 14 */
    a = el2 - d2;
    dp = dp + (48.0 * sin(a));
    de = de + cos(a);

    /* 13 */
    a = f2 - d2 + om;
    dp = dp + ((129.0 + (0.1 * t)) * sin(a));
    de = de - (70.0 * cos(a));

    /* 12 */
    a =  - elp + f2 - d2 + om2;
    dp = dp + ((217.0  - 0.5 * t) * sin(a));
    de = de + ((-95.0  + 0.3 * t) * cos(a));

    /* 11 */
    a = elp + f2 - d2 + om2;
    dp = dp + ((-517.0  + (1.2 * t)) * sin(a));
    de = de + ((224.0  - (0.6 * t)) * cos(a));

    /* 10 */
    dp = dp + ((1426.0 - (3.4 * t)) * sin(elp));
    de = de + ((54.0 - (0.1 * t)) * cos(elp));

    /* 9 */
    a = f2 - d2 + om2;
    dp = dp + ((-13187.0  - (1.6 * t)) * sin(a));
    de = de + ((5736.0  - (3.1 * t)) * cos(a));

    /* 8 */
    dp = dp + sin(el2 - f2 + om);

    /* 7 */
    a =  -elp2 + f2 - d2 + om;
    dp = dp - (2.0 * sin(a));
    de = de + (1.0 * cos(a));

    /* 6 */
    dp = dp - (3.0 * sin(el - elp - d));

    /* 5 */
    a =  - el2 + f2 + om2;
    dp = dp - (3.0 * sin(a));
    de = de + (1.0 * cos(a));

    /* 4 */
    dp = dp + (11.0 * sin (el2 - f2));

    /* 3 */
    a =  - el2 + f2 + om;
    dp = dp + (46.0 * sin(a));
    de = de - (24.0 * cos(a));

    /*  2 */
    dp = dp + ((2062.0 + (0.2 * t)) * sin(om2));
    de = de + ((-895.0 + (0.5 * t)) * cos(om2));

    /* 1 */
    dp = dp + ((-171996.0 - (174.2 * t)) * sin(om));
    de = de + ((92025.0 + (8.9 * t)) * cos(om));

    /* Convert results to radians */
    *dpsi = dp * u2r;
    *deps = de * u2r;

    /* Mean Obliquity */
    *eps0 = as2r * (84381.448 + (-46.8150 + (-0.00059 + (0.001813*t)*t)*t));

    return;
}


/* ISDATE - Return 1 if string is an old or ISO FITS standard date */

int
isdate (string)

char	*string; /* Possible FITS date string, which may be:
			dd/mm/yy (FITS standard before 2000)
			dd-mm-yy (nonstandard FITS use before 2000)
			yyyy-mm-dd (FITS standard after 1999)
			yyyy-mm-ddThh:mm:ss.ss (FITS standard after 1999) */

{
    int iyr;	/* year (returned) */
    int imon;	/* month (returned) */
    int iday;	/* day (returned) */
    int i;
    char *sstr, *dstr, *tstr, *nval;

    /* Translate string from ASCII to binary */
    if (string == NULL) 
	return (0);

    sstr = strchr (string,'/');
    dstr = strchr (string,'-');
    if (dstr == string)
	dstr = strchr (string+1,'-');
    tstr = strchr (string,'T');

    /* Original FITS date format: dd/mm/yy */
    if (sstr > string) {
	*sstr = '\0';
	iday = (int) atof (string);
	*sstr = '/';
	nval = sstr + 1;
	sstr = strchr (nval,'/');
	if (sstr == NULL)
	    sstr = strchr (nval,'-');
	if (sstr > string) {
	    *sstr = '\0';
	    imon = (int) atof (nval);
	    *sstr = '/';
	    nval = sstr + 1;
	    iyr = (int) atof (nval);
	    if (iyr < 1000)
		iyr = iyr + 1900;
	    }
	if (imon > 0 && iday > 0)
	    return (1);
	else
	    return (0);
	}

    /* New FITS date format: yyyy-mm-ddThh:mm:ss[.sss] */
    else if (dstr > string) {
	*dstr = '\0';
	iyr = (int) atof (string);
	nval = dstr + 1;
	*dstr = '-';
	dstr = strchr (nval,'-');
	imon = 0;
	iday = 0;

	/* Decode year, month, and day */
	if (dstr > string) {
	    *dstr = '\0';
	    imon = (int) atof (nval);
	    *dstr = '-';
	    nval = dstr + 1;
	    if (tstr > string)
		*tstr = '\0';
	    iday = (int) atof (nval);
	    if (tstr > string)
		*tstr = 'T';
	    }

	/* If day is > 31, it is really year in old format */
	if (iday > 31) {
	    i = iyr;
	    if (iday < 100)
		iyr = iday + 1900;
	    else
		iyr = iday;
	    iday = i;
	    }
	if (imon > 0 && iday > 0)
	    return (1);
	else
	    return (0);
	}

    /* If FITS date is entered as an epoch, return 0 anyway */
    else
	return (0);
}


/* Round seconds and make sure date and time numbers are within limits */

static void
fixdate (iyr, imon, iday, ihr, imn, sec, ndsec)

int	*iyr;	/* year (returned) */
int	*imon;	/* month (returned) */
int	*iday;	/* day (returned) */
int	*ihr;	/* hours (returned) */
int	*imn;	/* minutes (returned) */
double	*sec;	/* seconds (returned) */
int	ndsec;	/* Number of decimal places in seconds (0=int) */
{
    double days;

    /* Round seconds to 0 - 4 decimal places (no rounding if <0, >4) */
    if (ndsec == 0)
	*sec = dint (*sec + 0.5);
    else if (ndsec < 2)
	*sec = dint (*sec * 10.0 + 0.5) / 10.0;
    else if (ndsec < 3)
	*sec = dint (*sec * 100.0 + 0.5) / 100.0;
    else if (ndsec < 4)
	*sec = dint (*sec * 1000.0 + 0.5) / 1000.0;
    else if (ndsec < 5)
	*sec = dint (*sec * 10000.0 + 0.5) / 10000.0;

    /* Adjust minutes and hours */
    if (*sec > 60.0) {
	*sec = *sec - 60.0;
	*imn = *imn + 1;
	}
    if (*imn > 60) {
	*imn = *imn - 60;
	*ihr = *ihr + 1;
	}

    /* Return if no date */
    if (*iyr == 0 && *imon == 0 && *iday == 0)
	return;

   /* Adjust date */
    if (*ihr > 23) {
	*ihr = *ihr - 24;
	*iday = *iday + 1;
	}
    days = caldays (*iyr, *imon);
    if (*iday > days) {
	*iday = *iday - days;
	*imon = *imon + 1;
	}
    if (*iday < 1) {
	*imon = *imon - 1;
	if (*imon < 1) {
	    *imon = *imon + 12;
	    *iyr = *iyr - 1;
	    }
	days = caldays (*iyr, *imon);
	*iday = *iday + days;
	}
    if (*imon < 1) {
	*imon = *imon + 12;
	*iyr = *iyr - 1;
	days = caldays (*iyr, *imon);
	if (*iday > days) {
	    *iday = *iday - days;
	    *imon = *imon + 1;
	    }
	}
    if (*imon > 12) {
	*imon = *imon - 12;
	*iyr = *iyr + 1;
	}
    return;
}


/* Calculate days in month 1-12 given year (Gregorian calendar only) */

static int
caldays (year, month)

int	year;	/* 4-digit year */
int	month;	/* Month (1=January, 2=February, etc.) */
{
    if (month < 1) {
	month = month + 12;
	year = year + 1;
	}
    if (month > 12) {
	month = month - 12;
	year = year + 1;
	}
    switch (month) {
	case 1:
	    return (31);
	case 2:
	    if (year%400 == 0)
		return (29);
	    else if (year%100 == 0)
		return (28);
	    else if (year%4 == 0)
		return (29);
	    else
		return (28);
	case 3:
	    return (31);
	case 4:
	    return (30);
	case 5:
	    return (31);
	case 6:
	    return (30);
	case 7:
	    return (31);
	case 8:
	    return (31);
	case 9:
	    return (30);
	case 10:
	    return (31);
	case 11:
	    return (30);
	case 12:
	    return (31);
	default:
	    return (0);
	}
}


static double
dint (dnum)

double	dnum;
{
    double dn;

    if (dnum < 0.0)
	dn = -floor (-dnum);
    else
	dn = floor (dnum);
    return (dn);
}


static double
dmod (dnum, dm)

double	dnum, dm;
{
    double dnumx, dnumi, dnumf;
    if (dnum < 0.0)
	dnumx = -dnum;
    else
	dnumx = dnum;
    dnumi = dint (dnumx / dm);
    if (dnum < 0.0)
	dnumf = dnum + (dnumi * dm);
    else if (dnum > 0.0)
	dnumf = dnum - (dnumi * dm);
    else
	dnumf = 0.0;
    return (dnumf);
}

/* Jul  1 1999	New file, based on iolib/jcon.f and iolib/vcon.f and hgetdate()
 * Oct 21 1999	Fix declarations after lint
 * Oct 27 1999	Fix bug to return epoch if fractional year input
 * Dec  9 1999	Fix bug in ts2jd() found by Pete Ratzlaff (SAO)
 * Dec 17 1999	Add all unimplemented conversions
 * Dec 20 1999	Add isdate(); leave date, time strings unchanged in fd2i()
 * Dec 20 1999	Make all fd2*() subroutines deal with time alone
 *
 * Jan  3 2000	In old FITS format, year 100 is assumed to be 2000
 * Jan 11 2000	Fix epoch to date conversion so .0 is 0:00, not 12:00
 * Jan 21 2000	Add separate Besselian and Julian epoch computations
 * Jan 28 2000	Add Modified Julian Date conversions
 * Mar  2 2000	Implement decimal places for FITS date string
 * Mar 14 2000	Fix bug in dealing with 2000-02-29 in ts2i()
 * Mar 22 2000	Add lt2* and ut2* to get current time as local and UT
 * Mar 24 2000	Fix calloc() calls
 * Mar 24 2000	Add tsi2* and tsu2* to convert IRAF and Unix seconds
 * May  1 2000	In old FITS format, all years < 1000 get 1900 added to them
 * Aug  1 2000	Make ep2jd and jd2ep consistently starting at 1/1 0:00
 *
 * Jan 11 2001	Print all messages to stderr
 * May 21 2001	Add day of year conversions
 * May 25 2001	Allow fraction of day in FITS date instead of time
 *
 * Apr  8 2002	Change all long declaration to time_t
 * May 13 2002	Fix bugs found by lint
 * Jul  5 2002	Fix bug in fixdate() so fractional seconds come out
 * Jul  8 2002	Fix rounding bug in t2i()
 * Jul  8 2002	Try Fliegel and Van Flandern's algorithm for JD to UT date
 * Jul  8 2002	If first character of string is -, check for other -'s in isdate
 * Sep 10 2002	Add ET/TDT/TT conversion from UT subroutines
 * Sep 10 2002	Add sidereal time conversions
 *
 * Jan 30 2003	Fix typo in ts2gst()
 * Mar  7 2003	Add conversions for heliocentric julian dates
 * May 20 2003	Declare nd in setdatedec()
 * Jul 18 2003	Add code to parse Las Campanas dates
 *
 * Mar 24 2004	If ndec > 0, add UT to FITS date even if it is 0:00:00
 */
