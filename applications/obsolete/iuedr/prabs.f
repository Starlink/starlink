      SUBROUTINE PRABS( STATUS )

*+
*  Name:
*     SUBROUTINE PRABS

*  Purpose:
*     Print absolute calibration tables.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRABS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*     Paul Rees          05-OCT-88     IUEDR Vn. 2.0
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMABS'

*  Local Variables:
      INTEGER I          ! loop index
      INTEGER I1         ! loop index
      INTEGER I2         ! loop limit
      INTEGER J          ! loop index

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Banner.
      CALL LINE_WCONT( '%p%2w Absolute Calibration:\\' )
      CALL PRTBUF( STATUS )

*   EXPOSURE.
      CALL LINE_WCONT( '%p%4w Exposure times (seconds):\\' )
      CALL PRTBUF( STATUS )
      DO I = 1, NAPER
         CALL LINE_WRITS( '%p%6w %s: \\', APERS( 1, I ) )
         CALL LINE_WRITF( '%g\\', TSECS( I ) )
         CALL PRTBUF( STATUS )
      END DO
      CALL LINE_WCONT( '%p%4w end.\\' )
      CALL PRTBUF( STATUS )

*   FSCALE.
      CALL LINE_WCONT( '%p%4w Flux Scale Factors:\\' )
      CALL PRTBUF( STATUS )
      DO I = 1, NAPER
         CALL LINE_WRITS( '%p%6w %s: \\', APERS( 1, I ) )
         CALL LINE_WRITF( '%g\\', FSCALE( I ) )
         CALL PRTBUF( STATUS )
      END DO
      CALL LINE_WCONT( '%p%4w end.\\' )
      CALL PRTBUF( STATUS )

*   Basic components.
      IF ( .NOT. NOABS ) THEN
         CALL LINE_WRITS( '%p%4w Type %s,\\', ABSTP )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITS( '%p%4w Source ''%s''.\\', ABSID )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p%4w Variation with THDA (%.4f,\\',
     :                    TSEN( 1 ) )
         CALL LINE_WRITF( '%.4f),\\', TSEN( 2 ) )
         CALL LINE_WRITF( ' and DATE (%g,\\', DSEN( 1 ) )
         CALL LINE_WRITF( '%g).\\', DSEN( 2 ) )
         CALL PRTBUF( STATUS )

*      Print table.
         CALL LINE_WCONT( '%p%4w Sensitivity Function:\\' )
         CALL PRTBUF( STATUS )
         CALL LINE_WCONT( '%p%2w \\' )
         DO J = 1, 3
            CALL LINE_WCONT( '%5w(A)\\' )
            CALL LINE_WCONT( '%3w(cgs/A/FN)\\' )
         END DO
         CALL PRTBUF( STATUS )

         DO I1 = 1, NABS, 3
            I2 = MIN( NABS, I1 + 2 )
            CALL LINE_WCONT( '%p%2w \\' )
            DO I = I1, I2
               CALL LINE_WRITF( '    %6.1f\\', XABS( I ) )
               CALL LINE_WRITF( ' %9.2e \\', YCORR( I ) )
            END DO
            CALL PRTBUF( STATUS )
         END DO
         CALL LINE_WCONT( '%p%4w end.\\' )
         CALL PRTBUF( STATUS )
      END IF

*   End.
      CALL LINE_WCONT( '%p%2w end.\\' )
      CALL PRTBUF( STATUS )
      CALL PRTEOL( STATUS )

      END
