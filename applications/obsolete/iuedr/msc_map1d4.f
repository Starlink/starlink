      SUBROUTINE MSC_MAP1D4( NF, XF, YF, NR, XR, YR, STATUS )
*+
*  Name:
*     SUBROUTINE MSC_MAP1D4

*  Purpose:
*     1D mapping using damped parabolas (4th order).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSC_MAP1D4( NF, XF, YF, NR, XR, YR, STATUS )

*  Arguments:
*     NF = INTEGER (Given)
*
*     XF = REAL*8 ( NF ) (Given)
*
*     YF = REAL*8 ( NF ) (Given)
*
*     NR = INTEGER (Given)
*
*     XR = REAL*8 ( NR ) (Given)
*
*     YR = REAL*8 ( NR ) (Returned)
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-???-?? (JRG)
*     28-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     19-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NF

      REAL*8 XF( NF )
      REAL*8 YF( NF )

      INTEGER NR

      REAL*8 XR( NR )

*  Arguments Returned:
      REAL*8 YR( NR )

*  Status:
      INTEGER STATUS     ! Global status.

*  Local Constants:
      INTEGER ERR        ! Error status.
      PARAMETER ( ERR = -3 )

*  Local Variables:
      REAL*8 C( 3 )
      REAL*8 CB( 3 )
      REAL*8 CF( 3 )
      REAL*8 DX
      REAL*8 DY
      REAL*8 DYDX
      REAL*8 WDIV
      REAL*8 WF
      REAL*8 XX

      INTEGER I          ! Loop index.
      INTEGER J1
      INTEGER K
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check for sensible request.
      IF ( NR .LT. 1 ) THEN
         GO TO 999
      END IF

      IF ( NF .LT. 3 ) THEN

*      Case nf < 3.
         IF ( NF .LT. 2 ) THEN
            STATUS = ERR
            GO TO 999
         END IF

         DYDX = ( YF( 2 ) - YF( 1 ) ) / ( XF( 2 ) - XF( 1 ) )
         DY = YF( 1 )
         DO I = 1, NR
            DX = XR( I ) - XF( 1 )
            YR( I ) = DY + DYDX * DX
         END DO

      ELSE

*      First segment.
         CALL MSC_FIT1D3( XF, YF, CF )

         DO K = 1, 3
            C( K ) = CF( K )
         END DO

         J1 = 2
         DO I = 1, NR
            XX = XR( I )

 120        CONTINUE
            IF ( XX .GT. XF( J1 ) ) THEN
               IF ( J1 .NE. NF ) THEN
                  J1 = J1 + 1
                  IF ( J1 .LT. NF ) THEN

*                  Inner segments
                     DO K = 1, 3
                        CB( K ) = CF( K )
                     END DO

                     CALL MSC_FIT1D3( XF( J1 - 1 ), YF( J1 - 1 ), CF )

                     IF ( XX .LE. XF( J1 ) ) THEN
                        WF = 0.5
                        WDIV = ABS( CF( 3 ) ) + ABS( CB( 3 ) )
                        IF ( WDIV .GT. 0.0 ) THEN
                           WF = ABS( CB( 3 ) ) / WDIV
                        END IF
                        DO K = 1, 3
                           C( K ) = WF * ( CF( K ) - CB( K ) ) + CB( K )
                        END DO
                     END IF
                     GO TO 120

                  ELSE

*                  Last segment
                     DO K = 1, 3
                        C( K ) = CF( K )
                     END DO
                  END IF
               END IF
            END IF

            IF ( XX .LT. XF( 1 ) ) THEN
               XX = XF( 1 )
            END IF
            IF ( XX .GT. XF( NF ) ) THEN
               XX = XF( NF )
            END IF
            WF = ( C( 3 ) * XX + C( 2 ) ) * XX + C( 1 )
            YR( I ) = WF
         END DO
      END IF

 999  CONTINUE

      END
