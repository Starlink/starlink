      LOGICAL FUNCTION KPG1_ISSCS( SCS, EQU, BJ, NAME, STATUS )
*+
*  Name:
*     KPG1_ISSCS

*  Purpose:
*     Extracts the epoch of the reference equinox from a string
*     specifying an IRAS90 Sky Co-ordinate System.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = KPG1_ISSCS( SCS, EQU, BJ, NAME, STATUS )

*  Description:
*     If, on entry, the argument SCS contains an explicit IRAS90 equinox
*     specifier (see SUN/163), the epoch contained within it is returned
*     in argument EQU as a double precision value, and argument BJ is
*     returned equal to the character "B" or "J" depending on whether the
*     epoch is Besselian or Julian. If there is no equinox specifier in
*     argument SCS on entry, then the default of B1950 is returned.
*
*     If the sky co-ordinate system specified by SCS is not referred to
*     the equinox (e.g.GALACTIC) then EQU is returned equal to the
*     Starlink "BAD" value VAL__BADD, and BJ is returned blank.
*
*     The argument NAME is returned holding the full (unabbreviated)
*     name of the sky co-ordinate system without any equinox specifier.
*     On exit, the argument SCS holds the full name plus an explicit
*     equinox specifier (for systems which are referred to the
*     equinox). Thus, if SCS contained "EQUAT" on entry, it would
*     contain "EQUATORIAL(B1950)" on exit.

*  Arguments:
*     SCS = CHARACTER * ( * ) (Given and Returned)
*        On entry this should contain an IRAS90 SCS name (or any unambiguous
*        abbreviation), with or without an equinox specifier. On exit,
*        it contains the full SCS name with an explicit equinox
*        specifier (for those sky co-ordinate systems which are referred
*        to the equinox). If no equinox specifier is present on entry,
*        then a value of B1950 is used (if required). This variable
*        should have a declared length of 25.
*     EQU = DOUBLE PRECISION (Returned)
*        The epoch of the reference equinox. This is extracted
*        from any explicit equinox specifier contained in SCS on entry.
*        If there is no equinox specifier in SCS, a value of 1950.0
*        is returned. If the sky co-ordinate system is not referred to
*        the equinox (e.g. GALACTIC) the Starlink "BAD" value (VAL__BADD)
*        is returned, irrespective of any equinox specifier in SCS.
*     BJ = CHARACTER * ( * ) (Returned)
*        Returned holding either the character B or J. Indicates if
*        argument EQU gives a Besselian or Julian epoch. Returned blank
*        if the sky co-ordinate system is not referred to the equinox.
*     NAME = CHARACTER * ( * ) (Returned)
*        The full name of the sky co-ordinate system without any equinox
*        specifier. This variable should have a declared length of 25.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Function Value:
*     KPG1_ISSCS = LOGICAL
*        Returned .TRUE. if the supplied string is a valid IRAS90 SCS
*        specifier, and .FALSE. otherwise (no error is reported).

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JAN-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRA_PAR'          ! IRAS90 astrometry library constants

*  Arguments Given and Returned:
      CHARACTER SCS*(*)

*  Arguments Returned:
      DOUBLE PRECISION EQU
      CHARACTER BJ*(*)
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Initialise the returned function value.
      KPG1_ISSCS = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the NAME argument is long enough.
      IF( LEN( NAME ) .LT. IRA__SZSCS ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_ISSCS_ERR1',
     :   'KPG1_ISSCS: Declared size of argument NAME is too small '//
     :   '(programming error).', STATUS )
         GO TO 999
      END IF

*  Call IRA1_CHSCS to do the work
      CALL IRA1_CHSCS( SCS, NAME, EQU, BJ, STATUS )
      IF( STATUS .EQ. SAI__OK ) THEN

*  Copy the full SCS name to the returned value of SCS.
         SCS = NAME

*  If the SCS is referred to the equinox, add an explicit equinox
*  specifier to the returned value of SCS.
         IF ( BJ .NE. ' ' ) CALL IRA_SETEQ( EQU, BJ, SCS, STATUS )

*  Return .TRUE. for the function value to indicate a good SCS.
         KPG1_ISSCS = .TRUE.

*  If an error has occurred, annul it.
      ELSE
         CALL ERR_ANNUL( STATUS )
      END IF

 999  CONTINUE

      END
