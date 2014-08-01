#ifndef PALMACDEF
#define PALMACDEF

/*
*+
*  Name:
*     palmac.h

*  Purpose:
*     Macros used by the PAL library

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Include file

*  Description:
*     A collection of useful macros provided and used by the PAL library

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*

*  History:
*     2012-02-08 (TIMJ):
*        Initial version.
*        Adapted with permission from the Fortran SLALIB library.
*     2012-04-13 (DSB):
*        Added PAL__DR2H and PAL__DR2S
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software: you can redistribute it and/or
*     modify it under the terms of the GNU Lesser General Public
*     License as published by the Free Software Foundation, either
*     version 3 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU Lesser General Public License for more details.
*
*     You should have received a copy of the GNU Lesser General
*     License along with this program.  If not, see
*     <http://www.gnu.org/licenses/>.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Pi */
static const double PAL__DPI = 3.1415926535897932384626433832795028841971693993751;

/* 2Pi */
static const double PAL__D2PI = 6.2831853071795864769252867665590057683943387987502;

/* pi/2:  90 degrees in radians */
static const double PAL__DPIBY2 = 1.5707963267948966192313216916397514420985846996876;

/* pi/180:  degrees to radians */
static const double PAL__DD2R = 0.017453292519943295769236907684886127134428718885417;

/* Radians to arcseconds */
static const double PAL__DR2AS = 2.0626480624709635515647335733077861319665970087963e5;

/* Arcseconds to radians */
static const double PAL__DAS2R = 4.8481368110953599358991410235794797595635330237270e-6;

/* Radians to degrees */
static const double PAL__DR2D = 57.295779513082320876798154814105170332405472466564;

/* Hours to radians */
static const double PAL__DH2R = 0.26179938779914943653855361527329190701643078328126;

/* Radians to hours */
static const double PAL__DR2H = 3.8197186342054880584532103209403446888270314977709;

/* Radians to seconds of time */
static const double PAL__DR2S = 1.3750987083139757010431557155385240879777313391975e4;

/* Seconds of time to radians */
static const double PAL__DS2R = 7.272205216643039903848712e-5;

/* Start of SLA modified Julian date epoch */
static const double PAL__MJD0 = 2400000.5;

/* Light time for 1 AU (sec) */
static const double PAL__CR = 499.004782;

/* Seconds per day */
static const double PAL__SPD = 86400.0;

/* Km per sec to AU per tropical century
   = 86400 * 36524.2198782 / 149597870 */
static const double PAL__VF = 21.095;

/*  Radians per year to arcsec per century. This needs to be a macro since it
    is an expression including other constants. */
#define PAL__PMF (100.0*60.0*60.0*360.0/PAL__D2PI);

/* Mean sidereal rate - the rotational angular velocity of Earth
   in radians/sec from IERS Conventions (2003). */
static const double PAL__SR = 7.2921150e-5;

/*  Gaussian gravitational constant (exact) */
static const double PAL__GCON = 0.01720209895;

/* DINT(A) - truncate to nearest whole number towards zero (double) */
#define DINT(A) ((A)<0.0?ceil(A):floor(A))

/* DNINT(A) - round to nearest whole number (double) */
#define DNINT(A) ((A)<0.0?ceil((A)-0.5):floor((A)+0.5))

/* DMAX(A,B) - return maximum value - evaluates arguments multiple times */
#define DMAX(A,B) ((A) > (B) ? (A) : (B) )

/* DMIN(A,B) - return minimum value - evaluates arguments multiple times */
#define DMIN(A,B) ((A) < (B) ? (A) : (B) )

/* We actually prefer to use C99 copysign() but we define this here as a backup
   but it will not detect -0.0 so is not useful for palDfltin. */
/* DSIGN(A,B) - magnitude of A with sign of B (double) */
#define DSIGN(A,B) ((B)<0.0?-fabs(A):fabs(A))

#endif
