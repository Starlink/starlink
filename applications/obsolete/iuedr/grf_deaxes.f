      SUBROUTINE GRF_DEAXES( STATUS )
*+
*  Name:
*     SUBROUTINE GRF_DEAXES

*  Purpose:
*     Design axes limits.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRF_DEAXES( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     The graph axes limits are determined from information in CMGRAF
*     and (optionally) in CMPLOT.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version.
*     12-JAN-88 (PCTR):
*       IUEDR Vn. 2.0
*       Conversion to FORTRAN.
*       Conversion to GKS 7.2 graphics.
*     08-MAY-89 (PCTR):
*       IUEDR Vn. 2.1
*       Some restructuring and final conversion to SGP/16 style.
*     28-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMPLOT'

*  Status:
      INTEGER STATUS   ! Global status.

*  Local Variables:
      REAL XLS( 2 )    ! X-axis limits.
      REAL YLS( 2 )    ! Y-axis limits.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   X-reversal.
      IF ( XLIM( 1 ) .GT. XLIM( 2 ) ) THEN
         XREV = 1
         CALL MSC_RSWAP( XLIM( 1 ), XLIM( 2 ) )

      ELSE
         XREV = 0
      END IF

*   Y-reversal.
      IF ( YLIM( 1 ) .GT. YLIM( 2 ) ) THEN
         YREV = 1
         CALL MSC_RSWAP( YLIM( 1 ), YLIM( 2 ) )

      ELSE
         YREV = 0
      END IF

*   Don't do anything if RESET is .FALSE. and axes have been drawn.
      IF ( .NOT.RESET .AND. DRAWN ) THEN
         GO TO 999
      END IF

*   Design axes limits if needed.
      IF ( NOXL .AND. NOYL ) THEN
         CALL GRF_VQLIM( 3, NPOINT, XPLOT, QPLOT, XLS )
         IF ( XLS( 1 ) .NE. XLS( 2 ) ) THEN
            IF ( XLIM( 1 ) .EQ. XLIM( 2 ) ) THEN
               XLIM( 1 ) = XLS( 1 )
               XLIM( 2 ) = XLS( 2 )

            ELSE
               XLIM( 1 ) = MIN( XLIM( 1 ), XLS( 1 ) )
               XLIM( 2 ) = MAX( XLIM( 2 ), XLS( 2 ) )
            END IF
         END IF

         IF ( XLIM( 1 ) .NE. XLIM( 2 ) ) THEN
            CALL GRF_VQLIM( 3, NPOINT, YPLOT, QPLOT, YLS )
            IF ( YLS( 1 ) .NE. YLS( 2 ) ) THEN
               IF ( YLIM( 1 ) .EQ. YLIM( 2 ) ) THEN
                  YLIM( 1 ) = YLS( 1 )
                  YLIM( 2 ) = YLS( 2 )

               ELSE
                  YLIM( 1 ) = MIN( YLIM( 1 ), YLS( 1 ) )
                  YLIM( 2 ) = MAX( YLIM( 2 ), YLS( 2 ) )
               END IF
            END IF
         END IF

      ELSE IF ( NOXL ) THEN
         CALL GRF_XYLIM( YLIM, 3, NPOINT, YPLOT, XPLOT, QPLOT, XLS )
         IF ( XLS( 1 ) .NE. XLS( 2 ) ) THEN
            IF ( XLIM( 1 ) .EQ. XLIM( 2 ) ) THEN
               XLIM( 1 ) = XLS( 1 )
               XLIM( 2 ) = XLS( 2 )

            ELSE
               XLIM( 1 ) = MIN( XLIM( 1 ), XLS( 1 ) )
               XLIM( 2 ) = MAX( XLIM( 2 ), XLS( 2 ) )
            END IF
         END IF

      ELSE IF ( NOYL ) THEN
         CALL GRF_XYLIM( XLIM, 3, NPOINT, XPLOT, YPLOT, QPLOT, YLS )
         IF ( YLS( 1 ) .NE. YLS( 2 ) ) THEN
            IF ( YLIM( 1 ) .EQ. YLIM( 2 ) ) THEN
               YLIM( 1 ) = YLS( 1 )
               YLIM( 2 ) = YLS( 2 )

            ELSE
               YLIM( 1 ) = MIN( YLIM( 1 ), YLS( 1 ) )
               YLIM( 2 ) = MAX( YLIM( 2 ), YLS( 2 ) )
            END IF
         END IF
      END IF

*   Check that a result was obtained.
      IF ( XLIM( 1 ).EQ.XLIM( 2 ) .OR. YLIM( 1 ).EQ.YLIM( 2 ) ) THEN
         CALL ERROUT( 'Error: axis limits undefined\\', STATUS )
      END IF

 999  CONTINUE

      END
