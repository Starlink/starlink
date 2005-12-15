/* File libwcs/dateutil.c
 * August 1, 2000
 * By Doug Mink
 */

/* Date and time conversion routines using the following conventions:
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
  mjd = modified Julian Date = Julian date - 2400000.5
  ofd = FITS date string (dd/mm/yy before 2000, else no return)
 time = use fd2* with no date to convert time as hh:mm:ss.ss to sec, day, year
   ts = UT seconds since 1950-01-01T00:00 (used for ephemeris computations)
  tsi = local seconds since 1980-01-01T00:00 (used by IRAF as a time tag)
  tsu = UT seconds since 1970-01-01T00:00 (used as Unix system time)
   lt = Local time
   ut = Universal Time (UTC)

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
 * fd2ep, fd2epb, fd2epj (string)
 *	Convert FITS date string to fractional year
 *	Convert time alone to fraction of Besselian year
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
 * ut2dt(date, time)
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
#include "fitsfile.h"

static void fixdate();
static int caldays();
static double dint();
static double dmod();
static int ndec = 3;
void
setdatedec (nd)
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
    if (time != 0.0) {
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
    else if (time == 0.0)
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


/* JD2DT-- convert Julian date to date as yyyy.mmdd and time as hh.mmssss */

void
jd2dt (dj,date,time)

double	dj;	/* Julian date */
double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    double tsec;

    tsec = jd2ts (dj);
    ts2dt (tsec, date, time);

    return;
}


/* JD2I-- convert Julian date to date as yyyy.mmdd and time as hh.mmssss */

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

    tsec = jd2ts (dj);
    ts2i (tsec, iyr, imon, iday, ihr, imn, sec, ndsec);
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

long
lt2tsu()
{
    return ((long)(lt2ts() - 631152000.0));
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


/* MJD2I-- convert Modified Julian Date to date as yyyy.mmdd and time as hh.mmssss */

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

long
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


/* FD2FD-- convert any FITS standard date to old FITS standard date */

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

long
dt2tsu (date,time)

double	date;	/* Date as yyyy.mmdd */
double	time;	/* Time as hh.mmssxxxx
		 *if time<0, it is time as -(fraction of a day) */
{
    return ((long)(dt2ts (date, time) - 631152000.0));
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
long	isec;	/* Seconds past 1970-01-01 */
{
    return (ts2fd (tsu2ts (isec)));
}


/* TSU2DT-- Convert seconds since 1970-01-01 to date yyyy.ddmm, time hh.mmsss */

void
tsu2dt (isec,date,time)
long	isec;	/* Seconds past 1970-01-01 */
double	*date;	/* Date as yyyy.mmdd (returned) */
double	*time;	/* Time as hh.mmssxxxx (returned) */
{
    ts2dt (tsu2ts (isec), date, time);
}


/* TSU2TS-- Convert seconds since 1970-01-01 to seconds since 1950-01-01 */

double
tsu2ts (isec)
long	isec;	/* Seconds past 1970-01-01 */
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
    d = date;

    /* Extract components of time */
    *ihr = dint (t + 0.000000001);
    t = 100.0 * (t - (double) *ihr);
    *imn = dint (t + 0.0000001);
    *sec = 100.0 * (t - (double) *imn);

    /* Extract components of date */
    *iyr = dint (d + 0.00001);
    d = 100.0 * (d - (double) *iyr);
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
    double tsec;
    int i;
    char *sstr, *dstr, *tstr, *cstr, *nval;

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
    tstr = strchr (string,'T');
    cstr = strchr (string,':');

    /* Original FITS date format: dd/mm/yy */
    if (sstr > string) {
	*sstr = '\0';
	*iday = (int) atof (string);
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
	    if (tstr > string)
		*tstr = 'T';
	    }

	/* If year is < 32, it is really day of month in old format */
	if (*iyr < 32 || *iday > 31) {
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
    double t,days;
    int isec,ihms,nc,nc4,nly,ny,m,im;

    /* Round seconds to 0 - 4 decimal places */
    if (ndsec < 1)
	t = dint (tsec + 61530883200.5) * 10000.0;
    else if (ndsec < 2)
	t = dint ((tsec + 61530883200.0) * 10.0 + 0.5) * 1000.0;
    else if (ndsec < 3)
	t = dint ((tsec + 61530883200.0) * 100.0 + 0.5) * 100.0;
    else if (ndsec < 3)
	t = dint ((tsec + 61530883200.0) * 1000.0 + 0.5) * 10.0;
    else
	t = dint ((tsec + 61530883200.0) * 10000.0 + 0.5);

    /* Time of day (hours, minutes, seconds, .1 msec) */
    *ihr = (int) (dmod (t/36000000.0, 24.0));
    *imn = (int) (dmod (t/60000.0, 60.0));
    if (tsec >= 0) {
	ihms = (int) (dmod (tsec+0.000001, 1.0) * 10000.0);
	isec = (int) (dmod (tsec+0.000001, 60.0));
	}
    else {
	ihms = (int) (dmod (tsec-0.000001, 1.0) * 10000.0);
	isec = (int) (dmod (tsec-0.000001, 60.0));
	}

    /* Seconds */
    *sec = (double) isec + 0.0001 * (double) ihms;

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
	    /* printf ("%d %d %d %d\n", m, im, *iday, nc); */
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

long
ut2tsu()
{
    return ((long)(ut2ts () - 631152000.0));
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

	/* If year is < 32, it is really day of month in old format */
	if (iyr < 32 || iday > 31) {
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
	*sec = dint ((*sec * 10.0 + 0.5) / 10.0);
    else if (ndsec < 3)
	*sec = dint ((*sec * 100.0 + 0.5) / 100.0);
    else if (ndsec < 4)
	*sec = dint ((*sec * 1000.0 + 0.5) / 1000.0);
    else if (ndsec < 5)
	*sec = dint ((*sec * 10000.0 + 0.5) / 10000.0);

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
 */
