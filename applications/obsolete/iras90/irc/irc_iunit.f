      SUBROUTINE IRC_IUNIT( LIST, STATUS )
*+
*  Name:
*     IRC_IUNIT

*  Purpose:
*     Return a list of legal values for NDF component UNITS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_IUNIT( LIST, STATUS )

*  Description:
*     A string is returned containing a list of names identifying the
*     legal values of the NDF component UNITS. The values are separated
*     by commas. The currently recognised values are given by the
*     following symbolic constants:
*
*     IRC__F      - Flux values in units of Pico-Watts (i.e. 1.0E-12 of
*                   a Watt) per square metre.
*
*     IRC__J      - Flux density values in Janskys.
*
*     IRC__JPS    - Surface brightness in Janskys per steradian.
*
*     IRC__MJPS   - Surface brightness in Mega-Janskys per steradian.
*
*     IRC__FPS    - Surface brightness in Pico-Watts per square metre,
*                   per steradian.

*  Arguments:
*     LIST = CHARACTER * ( * ) (Returned)
*        The list of recognized values for NDF component UNITS. The
*        character variable supplied for this argument should have a
*        declared size equal to the value of parameter IRC__SZULS. If
*        the supplied string is not long enough to hold all the names, a
*        warning message is given, but no error status is returned. Each
*        individual value within the string has a maximum length given
*        by the symbolic constant IRC__SZUNI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JAN-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors

*  Arguments Returned:
      CHARACTER LIST*(*)

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Return the list of currently recognised values of UNITS. The length
*  of this string is stored in parameter IRC__SZULS which should be
*  updated when new values are added to the list.
      LIST = IRC__F//','//IRC__FPS//','//IRC__J//','//IRC__JPS//','//
     :       IRC__MJPS

*  If the list was truncated, give a warning message.
      IF( LEN( LIST ) .LT. IRC__SZULS ) THEN
         CALL MSG_OUT( 'IRC_IUNIT_MSG1', 'IRC__IUNIT: List of valid'//
     :   ' values for CRDD NDF component UNITS was truncated', STATUS )
      END IF

      END
