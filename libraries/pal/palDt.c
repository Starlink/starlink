/*
*+
*  Name:
*     palDt

*  Purpose:
*     Estimate the offset between dynamical time and UT

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palDt( double epoch );

*  Arguments:
*     epoch = double (Given)
*        Julian epoch (e.g. 1850.0)

*  Returned Value:
*     palDt = double
*        Rough estimate of ET-UT (after 1984, TT-UT) at the
*        given epoch, in seconds.

*  Description:
*     Estimate the offset between dynamical time and Universal Time
*     for a given historical epoch.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - Depending on the epoch, one of three parabolic approximations
*       is used:
*
*         before 979    Stephenson & Morrison's 390 BC to AD 948 model
*         979 to 1708   Stephenson & Morrison's 948 to 1600 model
*         after 1708    McCarthy & Babcock's post-1650 model
*
*       The breakpoints are chosen to ensure continuity:  they occur
*       at places where the adjacent models give the same answer as
*       each other.
*     - The accuracy is modest, with errors of up to 20 sec during
*       the interval since 1650, rising to perhaps 30 min by 1000 BC.
*       Comparatively accurate values from AD 1600 are tabulated in
*       the Astronomical Almanac (see section K8 of the 1995 AA).
*     - The use of double-precision for both argument and result is
*       purely for compatibility with other SLALIB time routines.
*     - The models used are based on a lunar tidal acceleration value
*       of -26.00 arcsec per century.
*
*  See Also:
*     Explanatory Supplement to the Astronomical Almanac,
*     ed P.K.Seidelmann, University Science Books (1992),
*     section 2.553, p83.  This contains references to
*     the Stephenson & Morrison and McCarthy & Babcock
*     papers.

*  History:
*     2012-03-08 (TIMJ):
*        Initial version with documentation from SLA/F.
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

double palDt ( double epoch ) {

  double t,w,s;

  /* Centuries since 1800 */
  t = (epoch - 1800.0) / 100.0;

  /* Select model */
  if ( epoch >= 1708.185161980887 ) {

    /* Post-1708: use McCarthy & Babcock */
    w = t - 0.19;
    s = 5.156 + 13.3066 * w * w;

  } else if ( epoch >= 979.0258204760233 ) {

    /* 978-1708: use Stephenson & Morrison's 948-1600 model */
    s = 25.5 * t * t;

  } else {

    /* Pre-979: use Stephenson & Morrison's 390 BC to AD 948 model */
    s = 1360.0 + (320.0 + 44.3*t) * t;

  }

  return s;

}
