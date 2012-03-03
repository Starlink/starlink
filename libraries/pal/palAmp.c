/*
*+
*  Name:
*     palAmp

*  Purpose:
*     Convert star RA,Dec from geocentric apparaent to mean place.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*      void palAmp ( double ra, double da, double date, double eq,
*                    double *rm, double *dm );

*  Arguments:
*     ra = double (Given)
*        Apparent RA (radians)
*     dec = double (Given)
*        Apparent Dec (radians)
*     date = double (Given)
*        TDB for apparent place (JD-2400000.5)
*     eq = double (Given)
*        Equinox: Julian epoch of mean place.
*     rm = double * (Returned)
*        Mean RA (radians)
*     dm = double * (Returned)
*        Mean Dec (radians)

*  Description:
*     Convert star RA,Dec from geocentric apparent to mean place. The
*     mean coordinate system is close to ICRS. See palAmpqk for details.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  Notes:
*     - See palMappa and palAmpqk for details.

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

void palAmp ( double ra, double da, double date, double eq,
              double *rm, double *dm ) {
  double amprms[21];
  palMappa( eq, date, amprms );
  palAmpqk( ra, da, amprms, rm, dm );
}
