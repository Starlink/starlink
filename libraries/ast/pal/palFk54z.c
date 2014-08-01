/*
*+
*  Name:
*     palFk54z

*  Purpose:
*     Convert a J2000.0 FK5 star position to B1950.0 FK4 assuming
*     zero proper motion and parallax.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     palFk54z( double r2000, double d2000, double bepoch, double *r1950,
*               double *d1950, double *dr1950, double *dd1950 )

*  Arguments:
*     r2000 = double (Given)
*        J2000.0 FK5 RA (radians).
*     d2000 = double (Given)
*        J2000.0 FK5 Dec (radians).
*     bepoch = double (Given)
*         Besselian epoch (e.g. 1950.0).
*     r1950 = double * (Returned)
*        B1950 FK4 RA (radians) at epoch "bepoch".
*     d1950 = double * (Returned)
*        B1950 FK4 Dec (radians) at epoch "bepoch".
*     dr1950 = double * (Returned)
*        B1950 FK4 proper motion (RA) (radians/trop.yr)).
*     dr1950 = double * (Returned)
*        B1950 FK4 proper motion (Dec) (radians/trop.yr)).

*  Description:
*     This function converts star positions from the IAU 1976,
*     FK5, Fricke system to the Bessel-Newcomb, FK4 system.

*  Notes:
*     - The proper motion in RA is dRA/dt rather than cos(Dec)*dRA/dt.
*     - Conversion from Julian epoch 2000.0 to Besselian epoch 1950.0
*     only is provided for.  Conversions involving other epochs will
*     require use of the appropriate precession functions before and
*     after this function is called.
*     - The FK5 proper motions, the parallax and the radial velocity
*      are presumed zero.
*     - It is the intention that FK5 should be a close approximation
*     to an inertial frame, so that distant objects have zero proper
*     motion;  such objects have (in general) non-zero proper motion
*     in FK4, and this function returns those fictitious proper
*     motions.
*     - The position returned by this function is in the B1950
*     reference frame but at Besselian epoch BEPOCH.  For comparison
*     with catalogues the "bepoch" argument will frequently be 1950.0.

*  Authors:
*     PTW: Pat Wallace (STFC)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2012-02-13 (DSB):
*        Initial version with documentation taken from Fortran SLA
*        Adapted with permission from the Fortran SLALIB library.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 1995 Rutherford Appleton Laboratory
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
#include "pal1sofa.h"

void palFk54z( double r2000, double d2000, double bepoch, double *r1950,
               double *d1950, double *dr1950, double *dd1950 ){

/* Local Variables: */
   double r, d, px, rv, y;

/* FK5 equinox J2000 (any epoch) to FK4 equinox B1950 epoch B1950. */
   palFk524( r2000, d2000, 0.0, 0.0, 0.0, 0.0, &r, &d, dr1950, dd1950,
             &px, &rv );

/* Fictitious proper motion to epoch "bepoch". */
   y = bepoch - 1950.0;
   *r1950 = r + *dr1950*y;
   *d1950 = d + *dd1950*y;

}

