/*
*+
*  Name:
*     palOne2One

*  Purpose:
*     File containing simple PAL wrappers for SLA routines that are identical in SOFA

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Description:
*     Some SOFA routines are identical to their SLA counterparts. PAL provides
*     direct counterparts to these although it is generally a better idea to
*     use the SOFA routine directly in new code.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Do not call these functions from other PAL functions. Always use
*       the SOFA routines directly in new code.
*     - These are implemented as real functions rather than C preprocessor
*       macros so there may be a performance penalty in using the PAL
*       version instead of the SOFA version.
*     - Routines that take MJDs have SOFA equivalents that have an explicit
*       MJD offset included.
*     - palGeoc uses the WGS84 model.
*     - palGmst uses the IAU 2006 precession.

*  History:
*     2012-02-10 (TIMJ):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2012 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
*    USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "palmac.h"
#include "sofa.h"

void palCldj ( int iy, int im, int id, double *djm, int *j ) {
  double djm0;
  *j = iauCal2jd( iy, im, id, &djm0, djm );
}

double palDbear ( double a1, double b1, double a2, double b2 ) {
  return iauPas( a1, b1, a2, b2 );
}

/* Arguments differ slightly. Assumes that the sign is always positive
   and dealt with externally. */
void palDaf2r ( int ideg, int iamin, double asec, double *rad, int *j ) {
  *j = iauAf2a( ' ', ideg, iamin, asec, rad );
}

void palDav2m ( double axvec[3], double rmat[3][3] ) {
  iauRv2m( axvec, rmat );
}

void palDcc2s ( double v[3], double *a, double *b ) {
  iauC2s( v, a, b );
}

void palDcs2c ( double a, double b, double v[3] ) {
  iauS2c( a, b, v );
}

void palDd2tf ( int ndp, double days, char *sign, int ihmsf[4] ) {
  iauD2tf( ndp, days, sign, ihmsf );
}

void palDimxv ( double dm[3][3], double va[3], double vb[3] ) {
  iauTrxp( dm, va, vb );
}

void palDm2av ( double rmat[3][3], double axvec[3] ) {
  iauRm2v( rmat, axvec );
}

/* Requires additional SLA MJD reference date */
void palDjcl ( double djm, int *iy, int *im, int *id, double *fd, int *j ) {
  *j = iauJd2cal( PAL__MJD0, djm, iy, im, id, fd );
}

void palDmxm ( double a[3][3], double b[3][3], double c[3][3] ) {
  iauRxr( a, b, c );
}

void palDmxv ( double dm[3][3], double va[3], double vb[3] ) {
  iauRxp( dm, va, vb );
}

double palDpav ( double v1[3], double v2[3] ) {
  return iauPap( v1, v2 );
}

double palDrange ( double angle ) {
  return iauAnpm( angle );
}

double palDranrm ( double angle ) {
  return iauAnp( angle );
}

double palDsep ( double a1, double b1, double a2, double b2 ) {
  return iauSeps( a1, b1, a2, b2 );
}

double palDsepv ( double v1[3], double v2[3] ) {
  return iauSepp( v1, v2 );
}

/* Assumes that the sign is always positive and is dealt with externally */
void palDtf2d ( int ihour, int imin, double sec, double *days, int *j ) {
  *j = iauTf2d( ' ', ihour, imin, sec, days );
}

/* Assumes that the sign is dealt with outside this routine */
void palDtf2r ( int ihour, int imin, double sec, double *rad, int *j ) {
  *j = iauTf2a( ' ', ihour, imin, sec, rad );
}

double palDvdv ( double va[3], double vb[3] ) {
  return iauPdp( va, vb );
}

/* Note that the arguments are flipped */
void palDvn ( double v[3], double uv[3], double *vm ) {
  iauPn( v, vm, uv );
}

void palDvxv ( double va[3], double vb[3], double vc[3] ) {
  iauPxp( va, vb, vc );
}

/* Requires additional SLA MJD reference date */
double palEpb ( double date ) {
  return iauEpb( PAL__MJD0, date );
}

double palEpb2d ( double epb ) {
  double djm0, djm;
  iauEpb2jd( epb, &djm0, &djm );
  return djm;
}

/* Requires additional SLA MJD reference date */
double palEpj ( double date ) {
  return iauEpj( PAL__MJD0, date );
}

double palEpj2d ( double epj ) {
  double djm0, djm;
  iauEpj2jd( epj, &djm0, &djm );
  return djm;
}

/* Requires additional SLA MJD reference date */
double palEqeqx ( double date ) {
  return iauEe06a( PAL__MJD0, date );
}

/* Do not use palEvp just yet */

void palFk5hz ( double r5, double d5, double epoch,
                double *rh, double *dh ) {
  /* Need to convert epoch to Julian date first */
  double date1, date2;
  iauEpj2jd( epoch, &date1, &date2 );
  iauFk5hz( r5, d5, date1, date2, rh, dh );
}

void palGeoc ( double p, double h, double *r, double *z ) {
  double xyz[3];
  const double elong = 0.0;   /* Use zero longitude */
  const double AU = 1.49597870E11;
  /* WGS84 looks to be the closest match */
  iauGd2gc( 1, elong, p, h, xyz );
  *r = xyz[0] / (AU * cos(elong) );
  *z = xyz[2] / AU;
}

/* Note that SOFA has more accurate time arguments and we use the 2006 precession model */
double palGmst ( double ut1 ) {
  return iauGmst06( PAL__MJD0, ut1, PAL__MJD0, ut1 );
}

void palHfk5z ( double rh, double dh, double epoch,
                double *r5, double *d5, double *dr5, double *dd5 ) {
  /* Need to convert epoch to Julian date first */
  double date1, date2;
  iauEpj2jd( epoch, &date1, &date2 );
  iauHfk5z( rh, dh, date1, date2, r5, d5, dr5, dd5 );
}

