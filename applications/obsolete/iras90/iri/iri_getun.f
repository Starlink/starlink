      SUBROUTINE IRI_GETUN( PARAM, DEFLT, UNITS, STATUS )
*+
*  Name:
*     IRI_GETUN

*  Purpose:
*     Get a standard units system from the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRI_GETUN( PARAM, DEFLT, UNITS, STATUS )

*  Description:
*     This routine uses the specified parameter to obtain a string
*     identifying one of the standard system of image units. The
*     returned string is equal to the value of one of the IRI symbolic
*     constants (including case). If an error occurs (including a null
*     parameter status), UNITS is returned blank.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     DEFLT = CHARACTER * ( * ) (Given)
*        The default value to suggest to the user.
*     UNITS = CHARACTER * ( * ) (Returned)
*        The obtained units.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRI_PAR'          ! IRI constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      CHARACTER DEFLT*(*)

*  Arguments Returned:
      CHARACTER UNITS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL OK                 ! True if the units are recognised by
                                 ! subroutine IRI_CHECK.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a value for the parameter selected from the full list of standard
*  units.
      CALL PAR_CHOIC( PARAM, DEFLT, IRI__UNITS, .FALSE., UNITS, STATUS )

*  PAR_CHOIC returns the units in upper case. Perform any necessary case
*  conversion to ensure that the returned string is one of the standard
*  systems.
      CALL IRI_CHECK( UNITS, OK, STATUS )

*  If the selected system of units has not yet been included in routine
*  IRI_CHECK, report an error.
      IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U', UNITS )
         CALL ERR_REP( 'IRI_GETUN_ERR1',
     :  'IRI_GETUN: The "^U" system of units is not yet supported by '//
     :  'subroutine IRI_CHECK.', STATUS )

      END IF

*  If an error has occurred, return a blank value for UNITS.
      IF( STATUS .NE. SAI__OK ) UNITS = ' '

      END
