      SUBROUTINE TTYHOLD( RESPONSE, STATUS )
*+
*   This simple subroutine asks waits for input of
*   a simple YES/NO

*   If the response is YES (or C/R) it returns .TRUE. by the RESPONSE
*   argument. If the response is NO, it returns .FALSE.
*
*   Arguments:
*     RESPONSE = LOGICAL (Returned)
*        .TRUE. if users inputs yes,.FALSE. if no
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*   History:
*
*   25-FEB-1993
*   Modified by AJJB to use PAR_DEF0L and PAR_GET0L instead of RDKEYL.
*     4-MAR-1993 (AJJB):
*       STATUS argument added.
*     5-MAR-1993 (AJJB):
*       RESPONSE argument added and call to EXIT removed, so that if
*       the user responds NO a logical value is returned to the calling
*       routine rather than terminating execution.
*-

      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status
      LOGICAL RESPONSE

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      RESPONSE=.TRUE.
      CALL PAR_DEF0L( 'MORE', RESPONSE, STATUS )
      CALL PAR_GET0L( 'MORE', RESPONSE, STATUS )
      CALL PAR_CANCL( 'MORE', STATUS )
      RETURN
      END
