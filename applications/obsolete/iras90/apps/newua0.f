      SUBROUTINE NEWUA0( PARAM, UNITS, STATUS )
*+
*  Name:
*     NEWUA0

*  Purpose:
*     Get a standard CRDD file or image units system from the
*     environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NEWUA0( PARAM, UNITS, STATUS )

*  Description:
*     This routine uses the specified parameter to obtain a string
*     identifying one of the standard system of image or CRDD file
*     units.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     UNITS = CHARACTER * ( * ) (Returned)
*        The obtained units.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRI_PAR'          ! IRI constants.

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      CHARACTER UNITS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_SIMLR

*  Local Variables:
      CHARACTER CLIST*(IRC__SZULS)! List of legal CRDD units.
      CHARACTER LIST*255         ! List of legal CRDD units.


      INTEGER IAT                ! Position of last non-blank character.


      LOGICAL OK                 ! True if the units are recognised by
                                 ! subroutine IRI_CHECK.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise a string to hold the list of legal image units.
      IAT = 0
      CALL CHR_APPND( IRI__UNITS//',', LIST, IAT )

*  Append CRDD units which are not included in the image units list.
      IF( INDEX( ','//LIST, ','//IRC__F//',' ) .EQ. 0 ) THEN
         LIST( IAT + 1 : ) = IRC__F//','
         IAT = CHR_LEN( LIST )
      END IF

      IF( INDEX( ','//LIST, ','//IRC__J//',' ) .EQ. 0 ) THEN
         LIST( IAT + 1 : ) = IRC__J//','
         IAT = CHR_LEN( LIST )
      END IF

      IF( INDEX( ','//LIST, ','//IRC__JPS//',' ) .EQ. 0 ) THEN
         LIST( IAT + 1 : ) = IRC__JPS//','
         IAT = CHR_LEN( LIST )
      END IF

      IF( INDEX( ','//LIST, ','//IRC__MJPS//',' ) .EQ. 0 ) THEN
         LIST( IAT + 1 : ) = IRC__MJPS//','
         IAT = CHR_LEN( LIST )
      END IF

      IF( INDEX( ','//LIST, ','//IRC__FPS//',' ) .EQ. 0 ) THEN
         LIST( IAT + 1 : ) = IRC__FPS//','
         IAT = CHR_LEN( LIST )
      END IF

*  Get a value for the parameter selected from the full list of standard
*  units.
      CALL PAR_CHOIC( PARAM, ' ', LIST( : IAT - 1 ), .FALSE., UNITS,
     :                STATUS )

*  PAR_CHOIC returns the units in upper case. Perform any necessary case
*  conversion to ensure that the returned string is one of the standard
*  systems.
      IF( CHR_SIMLR( UNITS, IRC__F ) ) THEN
         UNITS = IRC__F

      ELSE IF( CHR_SIMLR( UNITS, IRC__J ) ) THEN
         UNITS = IRC__J

      ELSE IF( CHR_SIMLR( UNITS, IRC__JPS ) ) THEN
         UNITS = IRC__JPS

      ELSE IF( CHR_SIMLR( UNITS, IRC__MJPS ) ) THEN
         UNITS = IRC__MJPS

      ELSE IF( CHR_SIMLR( UNITS, IRC__JPS ) ) THEN
         UNITS = IRC__JPS

*  If the supplied value was none of the CRDD values, try looking for a
*  matching image value.
      ELSE
         CALL IRI_CHECK( UNITS, OK, STATUS )

      END IF

      END
