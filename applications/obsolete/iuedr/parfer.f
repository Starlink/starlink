      SUBROUTINE PARFER( STR, STATUS )
*+
*  Name:
*     SUBROUTINE PARFER
*
*  Description:
*     Generate error message for parameters.
*     Handle NULL parameter response status if not done
*     by command-local handler.
*
*  History:
*     Martin Clayton     05-DEC-94     IUEDR Vn. 3.2
*
*-

*  Type Definitions.
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Local Constants:
      INTEGER ARB
      INTEGER ERR
      PARAMETER ( ARB = 100, ERR = -3 )

*  Arguments Given:
      BYTE STR( ARB )        ! The parameter name.

*  Status:
      INTEGER STATUS         ! Global status.
*.

*  Flush if Parameter Cancel signal is present.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_FLUSH( STATUS )
         STATUS = ERR
      END IF

*  Generate IUEDR-specific error message.
      CALL ERRSTR( STR )
      CALL ERROUT( ': parameter error\\', STATUS )

      END
