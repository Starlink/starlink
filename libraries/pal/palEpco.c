/*
*+
*  Name:
*     palEpco

*  Purpose:
*     Convert an epoch into the appropriate form - 'B' or 'J'

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     double palEpco( char k0, char k, double e );

*  Arguments:
*     k0 = char (Given)
*       Form of result: 'B'=Besselian, 'J'=Julian
*     k = char (Given)
*       Form of given epoch: 'B' or 'J'.

*  Description:
*     Converts a Besselian or Julian epoch to a Julian or Besselian
*     epoch.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - The result is always either equal to or very close to
*       the given epoch E.  The routine is required only in
*       applications where punctilious treatment of heterogeneous
*       mixtures of star positions is necessary.
*     - k and k0 are case insensitive. This differes slightly from the
*       Fortran SLA implementation.
*     - k and k0 are not validated. They are interpreted as follows:
*       o If k0 and k are the same the result is e
*       o If k0 is 'b' or 'B' and k isn't the conversion is J to B.
*       o In all other cases, the conversion is B to J.

*  History:
*     2012-03-01 (TIMJ):
*        Initial version. Documentation from SLA/F.
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
#include "sofa.h"

#include <ctype.h>

double palEpco( char k0, char k, double e ) {

  double new_epoch = 0.0;
  double djm;
  double djm0;

  /* Use upper case */
  k0 = toupper( k0 );
  k = toupper( k );

  if (k == k0) {
    new_epoch = e;
  } else if (k0 == 'B') {
    iauEpj2jd( e, &djm0, &djm );
    new_epoch = iauEpb( djm0, djm );
  } else {
    iauEpb2jd( e, &djm0, &djm );
    new_epoch = iauEpj( djm0, djm );
  }
  return new_epoch;
}
