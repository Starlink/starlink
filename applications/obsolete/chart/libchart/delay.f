      SUBROUTINE DELAY( STATUS )
*+
*   If the Device is Picture Oriented and is a Tektronix
*   then this routine will cause a delay until a <CR> is
*   is input, then the screen of the Device in Question
*   will be cleared.
*
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*   History:
*
*     8-DEC-1988
*       Modified by Peter Allan (MAVAD::PMA) and Tim Wilkins (MAVAD::TNW)
*       to use GKS 7.2 instead of GKS 6.2.
*     25-FEB-1993
*       Modified by AJJB to use PAR_GET0C instead of RDUSER.
*     3-MAR-1993 (AJJB):
*       STATUS argument added.
*-

*  Status:
      INTEGER STATUS             ! Global status

      CHARACTER*20 TEXT
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MAIN'
      INCLUDE 'PLOTDAT'

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (PIC) THEN

*   Read Response to  message from Terminal

         CALL PAR_GET0C( 'CONTINUE', TEXT, STATUS )

*   Clear the Workstation at the End of the Plot

         CALL GCLRWK (IWKID,1)

      ENDIF

      END

