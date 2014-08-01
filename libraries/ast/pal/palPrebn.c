/*
*+
*  Name:
*     palPrebn

*  Purpose:
*     Generate the matrix of precession between two objects (old)

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPrebn ( double bep0, double bep1, double rmatp[3][3] );

*  Arguments:
*     bep0 = double (Given)
*        Beginning Besselian epoch.
*     bep1 = double (Given)
*        Ending Besselian epoch
*     rmatp = double[3][3] (Returned)
*        precession matrix in the sense V(BEP1) = RMATP * V(BEP0)

*  Description:
*     Generate the matrix of precession between two epochs,
*     using the old, pre-IAU1976, Bessel-Newcomb model, using
*     Kinoshita's formulation

*  Authors:
*     PTW: Pat Wallace (STFC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  See Also:
*     Kinoshita, H. (1975) 'Formulas for precession', SAO Special
*     Report No. 364, Smithsonian Institution Astrophysical
*     Observatory, Cambridge, Massachusetts.

*  History:
*     2012-02-12(TIMJ):
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

void palPrebn ( double bep0, double bep1, double rmatp[3][3] ) {

  double t,bigt, zeta, theta, z, tas2r, w;

  /* Interval between basic epoch B1850.0 and beginning epoch in TC */
  bigt = (bep0-1850)/100.;

  /*  Interval over which precession required, in tropical centuries */
  t = (bep1-bep0)/100.;

  /* Euler angles */
  tas2r = t * PAL__DAS2R;
  w = 2303.5548 + ( 1.39720 + 0.000059 * bigt) * bigt;

  zeta = ( w + ( 0.30242 - 0.000269 * bigt + 0.017996 * t ) * t ) * tas2r;
  z = ( w + ( 1.09478 + 0.000387 * bigt + 0.018324 * t ) * t ) * tas2r;
  theta = ( 2005.1125 + ( -0.85294 - 0.000365 * bigt ) * bigt +
	    (-0.42647 - 0.000365 * bigt - 0.041802 * t ) * t ) * tas2r;

  /*  Rotation matrix */
  palDeuler("ZYZ", -zeta, theta, -z, rmatp);

}
