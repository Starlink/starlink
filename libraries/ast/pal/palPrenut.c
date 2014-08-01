/*
*+
*  Name:
*     palPrenut

*  Purpose:
*     Form the matrix of bias-precession-nutation (IAU 2006/2000A)

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPrenut( double epoch, double date, double rmatpn[3][3] )

*  Arguments:
*     epoch = double (Returned)
*        Julian epoch for mean coordinates.
*     date = double (Returned)
*        Modified Julian Date (JD-2400000.5) for true coordinates.
*     rmatpn = double[3][3] (Returned)
*        combined NPB matrix

*  Description:
*     Form the matrix of bias-precession-nutation (IAU 2006/2000A).
*     The epoch and date are TT (but TDB is usually close enough).
*     The matrix is in the sense   v(true)  =  rmatpn * v(mean).

*  Authors:
*     PTW: Pat Wallace (STFC)
*     {enter_new_authors_here}

*  History:
*     2012-02-10 (PTW):
*        Initial version.
*        Adapted with permission from the Fortran SLALIB library.
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

#include "pal.h"
#include "palmac.h"
#include "pal1sofa.h"

void palPrenut ( double epoch, double date, double rmatpn[3][3] ){

/* Local Variables: */
   double bpa;
   double bpia;
   double bqa;
   double chia;
   double d1;
   double d2;
   double eps0;
   double epsa;
   double gam;
   double oma;
   double pa;
   double phi;
   double pia;
   double psi;
   double psia;
   double r1[3][3];
   double r2[3][3];
   double thetaa;
   double za;
   double zetaa;

/* Specified Julian epoch as a 2-part JD. */
   eraEpj2jd( epoch, &d1, &d2 );

/* P matrix, from specified epoch to J2000.0. */
   eraP06e( d1, d2, &eps0, &psia, &oma, &bpa, &bqa, &pia, &bpia, &epsa,
            &chia, &za, &zetaa, &thetaa, &pa, &gam, &phi, &psi );
   eraIr( r1 );
   eraRz( -chia, r1 );
   eraRx( oma, r1 );
   eraRz( psia, r1 );
   eraRx( -eps0, r1 );

/* NPB matrix, from J2000.0 to date. */
   eraPnm06a( PAL__MJD0, date, r2 );

/* NPB matrix, from specified epoch to date. */
   eraRxr( r2, r1, rmatpn );
}
