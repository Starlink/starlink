      SUBROUTINE IRA_ISCS( LIST, STATUS )
*+
*  Name:
*     IRA_ISCS

*  Purpose:
*     Return a list of supported sky coordinate systems.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA_ISCS( LIST, STATUS )

*  Description:
*     A string is returned containing a list of names identifying the
*     supported sky coordinate systems. The names in the output list
*     are separated by commas.
*
*     By default, Equatorial and Ecliptic coordinates are referred to
*     the mean equinox of Besselian epoch 1950.0. The calling
*     application can override this default by appending a string known
*     as an "equinox specifier" to the end of the SCS name (in fact all
*     IRA routines will accept any unambiguous abbreviation of the SCS
*     name). An equinox specifier consists of a year with upto 4
*     decimal places, preceded with the letter B or J to indicate a
*     Besselian or Julian epoch, and enclosed in parentheses. The
*     named coordinate system is then referred to the mean equinox of
*     the epoch given in the equinox specifier. The following are
*     examples of legal SCS values; EQUATORIAL(B1950), EQUAT(J2000),
*     ECLIP, ECLIP(1983.2534), etc. If the date is not preceded with
*     either B or J (as in the last example), a Besselian epoch is
*     assumed if the date is less than 1984.0, and a Julian epoch is
*     assumed otherwise.
*
*     The currently supported sky coordinate systems are:
*
*     EQUATORIAL
*
*            The longitude axis is Right Ascension, the latitude axis
*            is Declination. Other legal names can be made by appending
*            an equinox specifier (eg EQUATORIAL(B1983.5) ). If no
*            equinox specifier is added, the coordinates are referred
*            to the mean equinox of Besselian epoch 1950.0. If the
*            equinox is described by a Besselian epoch, the old FK4
*            Bessel-Newcomb system is used. If a Julian epoch is used,
*            the new IAU 1976, FK5, Fricke system is used.
*
*     GALACTIC
*
*            The longitude axis is galactic longitude and the latitude
*            axis is galactic latitude, given in the IAU 1958 galactic
*            coordinate system.
*
*     ECLIPTIC
*
*            The longitude axis is ecliptic longitude and the latitude
*            axis is ecliptic latitude. Other legal names can be made
*            by appending an equinox specifier (eg ECLIPTIC(B1983.5) ).
*            If no equinox specifier is added, the coordinates are
*            referred to the mean equinox of Besselian epoch 1950.0.
*
*     AZEL
*
*            The longitude axis is horizon azimuth and the latitude axis is
*            horizon elevation.
*
*     All sky coordinate values supplied to, or returned from any IRA
*     routine, are given in units of radians.

*  Arguments:
*     LIST = CHARACTER * ( * ) (Returned)
*        The list of supported sky coordinate system names. The
*        character variable supplied for this argument should have a
*        declared size equal to the value of parameter IRA__SZCLS. If
*        the supplied string is not long enough to hold all the names, a
*        warning message is given, but no error status is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     27-APR-1991 (DSB):
*        Modified fro IRA version 2.
*     9-SEP-2005 (DSB):
*        Added AZEL.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRA_ERR'          ! IRA errors

*  Arguments Returned:
      CHARACTER LIST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return the list of sky coordinate system names currently supported
*  by the other IRA routines. The length of this string is stored in
*  parameter IRA__SZCLS which should be updated when new sky coordinate
*  systems are added to the list.
      LIST = 'EQUATORIAL,GALACTIC,ECLIPTIC,AZEL'

*  If the list was truncated, give a warning message.
      IF( LEN( LIST ) .LT. IRA__SZCLS ) THEN
         CALL MSG_OUT( 'IRA_ISCS_MSG1', 'WARNING - List of supported '//
     :            'sky coordinate systems had to be truncated', STATUS )
      END IF

      END
