/*
*+
*  Name:
*     palPm

*  Purpose:
*     Apply corrections for proper motion a star RA,Dec

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palPm ( double r0, double d0, double pr, double pd,
*                  double px, double rv, double ep0, double ep1,
*                  double *r1, double *d1 );

*  Arguments:
*     r0 = double (Given)
*        RA at epoch ep0 (radians)
*     d0 = double (Given)
*        Dec at epoch ep0 (radians)
*     pr = double (Given)
*        RA proper motion in radians per year.
*     pd = double (Given)
*        Dec proper motion in radians per year.
*     px = double (Given)
*        Parallax (arcsec)
*     rv = double (Given)
*        Radial velocity (km/sec +ve if receding)
*     ep0 = double (Given)
*        Start epoch in years, assumed to be Julian.
*     ep1 = double (Given)
*        End epoch in years, assumed to be Julian.
*     r1 = double * (Returned)
*        RA at epoch ep1 (radians)
*     d1 = double * (Returned)
*        Dec at epoch ep1 (radians)

*  Description:
*     Apply corrections for proper motion to a star RA,Dec using the
*     SOFA routine iauStarpm.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Uses iauStarpm but ignores the status returns from that routine.
*       In particular note that parallax should not be zero when the
*       proper motions are non-zero. SLA/F allows parallax to be zero.
*     - Assumes all epochs are Julian epochs.

*  History:
*     2012-03-02 (TIMJ):
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA.

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include "pal.h"
#include "sofa.h"

void palPm ( double r0, double d0, double pr, double pd,
             double px, double rv, double ep0, double ep1,
             double *r1, double *d1 ) {

  int status;
  double ep1a, ep1b, ep2a, ep2b;
  double pmr2, pmd2, px2, rv2;

  /* SOFA requires the epochs in TDB MJD so we have to
     assume that the supplied epochs are Julian years */
  iauEpj2jd( ep0, &ep1a, &ep1b );
  iauEpj2jd( ep1, &ep2a, &ep2b );

  status = iauStarpm( r0, d0, pr, pd, px, rv,
                      ep1a, ep1b, ep2a, ep2b,
                      r1, d1,
                      &pmr2, &pmd2, &px2, &rv2 );

}
