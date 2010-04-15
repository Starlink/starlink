      SUBROUTINE FIND04( PCROSS, PINSCA, MENU, STATUS )
*+
*  Name:
*     FIND04

*  Purpose:
*     Subroutine requests a region size from the user and applies it for
*     all sources

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FIND04( PCROSS, PINSCA, MENU, STATUS )

*  Description:
*     Subroutine requests a region size from the user and applies it for
*     all sources.

*  Arguments:
*     PCROSS = CHARACTER * ( * ) (Given)
*        Parameter CROSSCAN size of req. region in cross scan direction
*        in arc minutes
*     PINSCA = CHARACTER * ( * ) (Given)
*        Parameter INSCAN size of req. region in in scan direction
*        in arc minutes
*     MENU = CHARACTER * ( 1 ) (Given and Returned)
*        Choice from enter size and wavebands menu
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  External Routines Used:
*     ERR:
*        ERR_ANNUL
*     MSG:
*        MSG_OUT
*     PAR:
*        PAR_CANCL, PAR_DEF0R, PAR_GET0R

*  Authors:
*     DCP: Diana Parsons (IPMAF/RAL)
*     {enter_new_authors_here}

*  History:
*     17-SEP-1991 (DCP):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_PAR'          ! IRAS 90 General constants
      INCLUDE 'IRA_PAR'          ! IRAS Astrometry constants
      INCLUDE 'IRA_ERR'          ! IRAS Astrometry errors
      INCLUDE 'MSG_PAR'          ! Message reporting constants
      INCLUDE 'MSG_ERR'          ! Message reporting errors
      INCLUDE 'ERR_PAR'          ! Error reporting constants
      INCLUDE 'ERR_ERR'          ! Error reporting errors
      INCLUDE 'PAR_ERR'          ! Parameter errors

*  Global Variables:
      INCLUDE 'FICOMN' ! Common blocks for FINDCRDD

*  Arguments Given:
      CHARACTER * ( * )  PCROSS
      CHARACTER * ( * )  PINSCA
      CHARACTER * ( 1 )  MENU

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL ALCROS                ! Cross scan size in arc minutes for
                                 ! all sources
      REAL ALCROR                ! Cross scan size in radians for
                                 ! all sources
      REAL ALINSC                ! In scan size in arc minutes for
                                 ! all sources
      REAL ALINSR                ! In scan size in radians for
                                 ! all sources
      REAL DECROS                ! Default cross scan value
      REAL DEINSC                ! Default in scan value
      INTEGER II                 ! DO loop control variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the dynamic default for the cross scan size to 0
      DECROS = 0.0
      CALL PAR_DEF0R( PCROSS, DECROS, STATUS )

*  Obtain the cross scan size in arc minutes from the user
      CALL PAR_GET0R( PCROSS, ALCROS, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
      CALL PAR_CANCL( PCROSS, STATUS )

*  Set the dynamic default for the inscan size to 120.0
      DEINSC = 120.0
      CALL PAR_DEF0R( PINSCA, DEINSC, STATUS )

*  Obtain the in scan size in arc minutes from the user
      CALL PAR_GET0R( PINSCA, ALINSC, STATUS )

*  Cancel the parameter so that a new value is obtained next time
*  through this section
      CALL PAR_CANCL( PINSCA, STATUS )

*  Check whether the parameter was abort !!
      IF ( STATUS .EQ. PAR__ABORT ) RETURN

*  Check whether the source name was ! indicating that the user does not
*  want to continue this action
      IF ( STATUS .NE. PAR__NULL ) THEN

*  Change the arc minute values for cross scan and in scan to radians
         ALCROR = ALCROS * AMTOR
         ALINSR = ALINSC * AMTOR

*  Store these values in the cross scan and in scan sizes for each
*  source
         DO 100 II = 1, NOFSO

*  Store in scan value in SOINSZ
            SOINSZ( II ) = ALINSR

*  Store cross scan value in SOCRSZ
            SOCRSZ( II ) = ALCROR

 100     CONTINUE

      END IF

*  Display a message if either parameter was entered as !
      IF ( STATUS .EQ. PAR__NULL ) THEN

*  If any of the parameters was entered as !, annul the error message,
*  which sets the status to SAI__OK.
         CALL ERR_ANNUL( STATUS )
         CALL MSG_OUT( ' ',
     :   ' The cross scan or in scan size has been entered as a !',
     :   STATUS )
         CALL MSG_OUT( ' ',
     :   ' Therefore no change has been made to any source size ',
     :   STATUS )

      END IF

*  Change the menu parameter back to 'M' to get menu in FIND26
         MENU = 'M'

      END
