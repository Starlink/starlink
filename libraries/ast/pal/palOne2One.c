/*
*+
*  Name:
*     palOne2One

*  Purpose:
*     File containing simple PAL wrappers for SLA routines that are identical in SOFA

*  Invocation:
*     Matches SLA API

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Description:
*     Some SOFA/ERFA routines are identical to their SLA counterparts. PAL provides
*     direct counterparts to these although it is generally a better idea to
*     use the SOFA/ERFA routine directly in new code.
*
*     The PAL routines with direct equivalents in SOFA/ERFA are:
*     - palCldj
*     - palDbear
*     - palDaf2r
*     - palDav2m
*     - palDcc2s
*     - palDcs2c
*     - palDd2tf
*     - palDimxv
*     - palDm2av
*     - palDjcl
*     - palDmxm
*     - palDmxv
*     - palDpav
*     - palDr2af
*     - palDr2tf
*     - palDranrm
*     - palDsep
*     - palDsepv
*     - palDtf2d
*     - palDtf2r
*     - palDvdv
*     - palDvn
*     - palDvxv
*     - palEpb
*     - palEpb2d
*     - palEpj
*     - palEpj2d
*     - palEqeqx
*     - palFk5hz
*     - palGmst
*     - palGmsta
*     - palHfk5z
*     - palRefcoq

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Do not call these functions from other PAL functions. Always use
*       the SOFA/ERFA routines directly in new code.
*     - These are implemented as real functions rather than C preprocessor
*       macros so there may be a performance penalty in using the PAL
*       version instead of the SOFA/ERFA version.
*     - Routines that take MJDs have SOFA/ERFA equivalents that have an explicit
*       MJD offset included.
*     - palEqeqx, palGmst and palGmsta use the IAU 2006 precession model.

*  History:
*     2012-02-10 (TIMJ):
*        Initial version
*        Adapted with permission from the Fortran SLALIB library.
*     2012-03-23 (TIMJ):
*        Update prologue.
*     2012-05-09 (DSBJ):
*        Move palDrange into a separate file.
*     2014-07-15 (TIMJ):
*        SOFA now has palRefcoq equivalent.
*     {enter_further_changes_here}

*  Copyright:
*     Copyeight (C) 2014 Tim Jenness
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

#include "pal.h"
#include "palmac.h"
#include "pal1sofa.h"

void palCldj ( int iy, int im, int id, double *djm, int *j ) {
  double djm0;
  *j = eraCal2jd( iy, im, id, &djm0, djm );
}

double palDbear ( double a1, double b1, double a2, double b2 ) {
  return eraPas( a1, b1, a2, b2 );
}

/* Arguments differ slightly. Assumes that the sign is always positive
   and dealt with externally. */
void palDaf2r ( int ideg, int iamin, double asec, double *rad, int *j ) {
  *j = eraAf2a( ' ', ideg, iamin, asec, rad );
}

void palDav2m ( double axvec[3], double rmat[3][3] ) {
  eraRv2m( axvec, rmat );
}

void palDcc2s ( double v[3], double *a, double *b ) {
  eraC2s( v, a, b );
}

void palDcs2c ( double a, double b, double v[3] ) {
  eraS2c( a, b, v );
}

void palDd2tf ( int ndp, double days, char *sign, int ihmsf[4] ) {
  eraD2tf( ndp, days, sign, ihmsf );
}

void palDimxv ( double dm[3][3], double va[3], double vb[3] ) {
  eraTrxp( dm, va, vb );
}

void palDm2av ( double rmat[3][3], double axvec[3] ) {
  eraRm2v( rmat, axvec );
}

/* Requires additional SLA MJD reference date */
void palDjcl ( double djm, int *iy, int *im, int *id, double *fd, int *j ) {
  *j = eraJd2cal( PAL__MJD0, djm, iy, im, id, fd );
}

void palDmxm ( double a[3][3], double b[3][3], double c[3][3] ) {
  eraRxr( a, b, c );
}

void palDmxv ( double dm[3][3], double va[3], double vb[3] ) {
  eraRxp( dm, va, vb );
}

double palDpav ( double v1[3], double v2[3] ) {
  return eraPap( v1, v2 );
}

void palDr2af ( int ndp, double angle, char *sign, int idmsf[4] ) {
  eraA2af( ndp, angle, sign, idmsf );
}

void palDr2tf( int ndp, double angle, char *sign, int ihmsf[4] ) {
  eraA2tf( ndp, angle, sign, ihmsf );
}

double palDranrm ( double angle ) {
  return eraAnp( angle );
}

double palDsep ( double a1, double b1, double a2, double b2 ) {
  return eraSeps( a1, b1, a2, b2 );
}

double palDsepv ( double v1[3], double v2[3] ) {
  return eraSepp( v1, v2 );
}

/* Assumes that the sign is always positive and is dealt with externally */
void palDtf2d ( int ihour, int imin, double sec, double *days, int *j ) {
  *j = eraTf2d( ' ', ihour, imin, sec, days );
}

/* Assumes that the sign is dealt with outside this routine */
void palDtf2r ( int ihour, int imin, double sec, double *rad, int *j ) {
  *j = eraTf2a( ' ', ihour, imin, sec, rad );
}

double palDvdv ( double va[3], double vb[3] ) {
  return eraPdp( va, vb );
}

/* Note that the arguments are flipped */
void palDvn ( double v[3], double uv[3], double *vm ) {
  eraPn( v, vm, uv );
}

void palDvxv ( double va[3], double vb[3], double vc[3] ) {
  eraPxp( va, vb, vc );
}

/* Requires additional SLA MJD reference date */
double palEpb ( double date ) {
  return eraEpb( PAL__MJD0, date );
}

double palEpb2d ( double epb ) {
  double djm0, djm;
  eraEpb2jd( epb, &djm0, &djm );
  return djm;
}

/* Requires additional SLA MJD reference date */
double palEpj ( double date ) {
  return eraEpj( PAL__MJD0, date );
}

double palEpj2d ( double epj ) {
  double djm0, djm;
  eraEpj2jd( epj, &djm0, &djm );
  return djm;
}

/* Requires additional SLA MJD reference date */
double palEqeqx ( double date ) {
  return eraEe06a( PAL__MJD0, date );
}

/* Do not use palEvp just yet */

void palFk5hz ( double r5, double d5, double epoch,
                double *rh, double *dh ) {
  /* Need to convert epoch to Julian date first */
  double date1, date2;
  eraEpj2jd( epoch, &date1, &date2 );
  eraFk5hz( r5, d5, date1, date2, rh, dh );
}

/* Note that SOFA/ERFA has more accurate time arguments
   and we use the 2006 precession model */
double palGmst ( double ut1 ) {
  return eraGmst06( PAL__MJD0, ut1, PAL__MJD0, ut1 );
}

/* Slightly better but still not as accurate as SOFA/ERFA */

double palGmsta( double date, double ut ) {
  date += PAL__MJD0;
  return eraGmst06( date, ut, date, ut );
}

void palHfk5z ( double rh, double dh, double epoch,
                double *r5, double *d5, double *dr5, double *dd5 ) {
  /* Need to convert epoch to Julian date first */
  double date1, date2;
  eraEpj2jd( epoch, &date1, &date2 );
  eraHfk5z( rh, dh, date1, date2, r5, d5, dr5, dd5 );
}

void palRefcoq ( double tdk, double pmb, double rh, double wl,
                 double *refa, double *refb ) {
  /* Note that SLA (and therefore PAL) uses units of kelvin
     but SOFA/ERFA uses deg C */
  eraRefco( pmb, tdk - 273.15, rh, wl, refa, refb );
}
