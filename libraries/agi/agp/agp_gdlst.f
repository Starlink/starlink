      SUBROUTINE AGP_GDLST( STATUS )
*+
*  Name:
*     AGP_GDLST

*  Purpose:
*     Lists all known graphics devices.

*  Invocation:
*     CALL AGP_GDLST( STATUS )

*  Description:
*     This routine lists all known graphics devices, using MSG_OUT.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Authors:
*     DSB: Davis Berry (STARLINK)

*  History:
*     31-OCT-2001 (DSB):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'AGP_CONST'

*  Global Variables:
      INCLUDE 'AGP_COM'

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AGP1_INIT          ! Initializes AGP common blocks

*  Local Variables:
      CHARACTER BUF*80            ! Output text buffer
      INTEGER I                   ! Loop count
      INTEGER IAT                 ! Used length of string
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN


      CALL MSG_BLANK( STATUS )

      DO I = 1, AGP__NDEV
         IAT = 0
         BUF = ' '
         CALL CHR_APPND( AGP_GTY( I ), BUF, IAT )
         IAT = AGP__SZGTY + 1

         IF( AGP_PFN( I ) .EQ. ' ' ) THEN
            CALL CHR_APPND( '(/', BUF, IAT )
         ELSE
            CALL CHR_APPND( '(', BUF, IAT )
            CALL CHR_APPND( AGP_PFN( I ), BUF, IAT )
            CALL CHR_APPND( '/', BUF, IAT )
         END IF

         CALL CHR_APPND( AGP_PTY( I ), BUF, IAT )
         CALL CHR_APPND( ')', BUF, IAT )
         IAT = AGP__SZGTY + AGP__SZPFN + AGP__SZPTY + 2
         CALL CHR_APPND( AGP_DSC( I ), BUF, IAT )
         CALL MSG_OUT( ' ', BUF( : IAT ), STATUS )
      END DO

      CALL MSG_BLANK( STATUS )

      END
