      SUBROUTINE GRP1_LUNIT( UNIT, STATUS )
*+
*  Name:
*     GRP1_LUNIT

*  Purpose:
*     Find an unused Fortran logical IO unit number.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_LUNIT( UNIT, STATUS )

*  Description:
*     This routine used the Fortran INQUIRE statement to check
*     a range of logical units until one is found which is not
*     currently in use.

*  Arguments:
*     UNIT = INTEGER (Returned)
*        A free logical unit number. If no free units can be found, then
*        an error is reported. In this case, UNIT is returned holding
*        the value of parameter GRP__INVUN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1992 (DSB):
*        Original version
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_ERR'          ! GRP error values

*  Arguments Returned:
      INTEGER UNIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL EXISTS             ! True if the unit number exists.
      INTEGER I                  ! Unit count.
      INTEGER IOS                ! The Fortran IO status value.
      LOGICAL OPENED             ! True if the unit number is in use.

*.

*  Ensure UNIT gets returned holding GRP__INVUN if an error condition
*  exists on entry.
      UNIT = GRP__INVUN

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round, checking each unit number.
      DO I = GRP__MINUN, GRP__MAXUN
         IF( UNIT .EQ. GRP__INVUN ) THEN

            INQUIRE( I, EXIST = EXISTS, OPENED = OPENED, IOSTAT = IOS )

*  If an IO error was detected, report an error.
            IF( IOS. NE. 0 ) THEN
               STATUS = GRP__FIOER
               CALL MSG_SETI( 'UNIT', I )
               CALL ERR_FIOER( 'TEXT', IOS )
               CALL ERR_REP( 'GRP1_LUNIT_ERR1',
     :'GRP1_LUNIT: Error inquiring status of Fortran unit ^UNIT: ^TEXT',
     :                       STATUS )
               GO TO 999
            END IF

*  If the unit exists, and is not in use, return it.
            IF( EXISTS .AND. .NOT. OPENED ) UNIT = I

         END IF
      END DO

*  If no free unit number could be found, report an error.
 999  CONTINUE
      IF( UNIT .EQ. GRP__INVUN ) THEN
         STATUS = GRP__NOLUN
         CALL ERR_REP( 'GRP1_LUNIT_ERR2',
     : 'GRP1_LUNIT: No free Fortran I/O unit numbers can be found.',
     :                 STATUS )
      END IF


      END
