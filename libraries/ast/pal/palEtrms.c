/*
*+
*  Name:
*     palEtrms

*  Purpose:
*     Compute the E-terms vector

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palEtrms ( double ep, double ev[3] );

*  Arguments:
*     ep = double (Given)
*        Besselian epoch
*     ev = double [3] (Returned)
*        E-terms as (dx,dy,dz)

*  Description:
*     Computes the E-terms (elliptic component of annual aberration)
*     vector.
*
*     Note the use of the J2000 aberration constant (20.49552 arcsec).
*     This is a reflection of the fact that the E-terms embodied in
*     existing star catalogues were computed from a variety of
*     aberration constants.  Rather than adopting one of the old
*     constants the latest value is used here.
*
*  See also:
*     - Smith, C.A. et al., 1989.  Astr.J. 97, 265.
*     - Yallop, B.D. et al., 1989.  Astr.J. 97, 274.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-12 (TIMJ):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1996 Rutherford Appleton Laboratory
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

void palEtrms ( double ep, double ev[3] ) {

  /* Use the J2000 aberration constant */
  const double ABCONST = 20.49552;

  double t, e, e0, p, ek, cp;

  /*  Julian centuries since B1950 */
  t = (ep - 1950.) * .0100002135903;

  /*  Eccentricity */
  e = .01673011 - (t * 1.26e-7 + 4.193e-5) * t;

  /*  Mean obliquity */
  e0 = (84404.836 - ((t * .00181 + .00319) * t + 46.8495) * t) *
    PAL__DAS2R;

  /*  Mean longitude of perihelion */
  p = (((t * .012 + 1.65) * t + 6190.67) * t + 1015489.951) *
    PAL__DAS2R;

  /*  E-terms */
  ek = e * ABCONST * PAL__DAS2R;
  cp = cos(p);
  ev[0] = ek * sin(p);
  ev[1] = -ek * cp * cos(e0);
  ev[2] = -ek * cp * sin(e0);

}
