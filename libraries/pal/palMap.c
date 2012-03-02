/*
*+
*  Name:
*     palMap

*  Purpose:
*     Convert star RA,Dec from mean place to geocentric apparent

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     void palMap( double rm, double dm, double pr, double pd,
*                  double px, double rv, double eq, double date,
*                  double *ra, double *da );

*  Arguments:
*     rm = double (Given)
*        Mean RA (radians)
*     dm = double (Given)
*        Mean declination (radians)
*     pr = double (Given)
*        RA proper motion, changes per Julian year (radians)
*     pd = double (Given)
*        Dec proper motion, changes per Julian year (radians)
*     px = double (Given)
*        Parallax (arcsec)
*     rv = double (Given)
*        Radial velocity (km/s, +ve if receding)
*     eq = double (Given)
*        Epoch and equinox of star data (Julian)
*     date = double (Given)
*        TDB for apparent place (JD-2400000.5)
*     ra = double * (Returned)
*        Apparent RA (radians)
*     dec = double * (Returned)
*        Apparent dec (radians)

*  Description:
*     Convert star RA,Dec from mean place to geocentric apparent.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Calls palMappa and palMapqk
*     - The reference systems and timescales used are IAU 2006.

*  History:
*     2012-03-01 (TIMJ):
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

void palMap( double rm, double dm, double pr, double pd,
             double px, double rv, double eq, double date,
             double *ra, double *da ) {

  double amprms[21];

  /* Star independent parameters */
  palMappa( eq, date, amprms );

  /* Mean to apparent */
  palMapqk( rm, dm, pr, pd, px, rv, amprms, ra, da );

}
