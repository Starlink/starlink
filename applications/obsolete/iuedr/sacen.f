      SUBROUTINE SACEN
*+
*  Name:
*     SUBROUTINE SACEN

*  Purpose:
*     Save centroid template after extraction. The contents of CMCEN are
*     optionally used to update the CMTEM common block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SACEN

*  Method:
*     The Censv is used to indicate whether the updating takes place.
*     If so, then CMCEN is mapped onto a limited wavelength grid.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     25-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMDISH'
      INCLUDE 'CMCEN'
      INCLUDE 'CMWAV'
      INCLUDE 'CMTEM'

*  Local Variables:
      REAL*8 CBOT( 100 )  ! Normalisation weights.
      REAL*8 CTOP( 100 )  ! Folded centroids.
      REAL*8 F1
      REAL*8 F2
      REAL*8 W1
      REAL*8 W2

      INTEGER I           ! Accumulator.
      INTEGER K           ! Loop index.
      INTEGER M           ! Order.
*.

*   Exit if not going to save extracted centroids in dataset.
      IF ( .NOT. CENSV ) THEN
         GO TO 999
      END IF

*   Find entry in CMTEM.
      IF ( NOTEM ) THEN
         NTEMO = 0
      END IF
      CALL ALTEM( CORD, M )

*   Check on available slot.
      IF ( M .LE. 0 ) THEN
         GO TO 999
      END IF

      NOTEM = .FALSE.

*   Mark calibration file as needing change.
      CALL MODCAL

*   Form evenly spaced template grid (MAXTEM points).
      NTEMS( M ) = 100
      TEMDW( M ) = ( WAV( NWAV ) - WAV( 1 ) ) / DBLE( NTEMS( M ) - 1 )
      TEMW0( M ) = WAV( 1 )

      DO K = 1, NTEMS( M )
         CTOP( K ) = 0.0
         CBOT( K ) = 0.0
      END DO

*   Fold extracted centroids onto template grid.
      I = 1

      DO K = 2, NTEMS( M )
         W2 = DBLE( K - 1 ) * TEMDW( M ) + TEMW0( M )
         W1 = W2 - TEMDW( M )

         DO WHILE ( .TRUE. )
            IF ( .NOT. ( I .LE. NWAV ) ) THEN
               GO TO 100
            END IF

            IF ( WAV( I ) .LT. W1 ) THEN
               I = I + 1

            ELSE IF ( WAV( I ) .GT. W2 ) THEN
               GO TO 100

            ELSE
               F1 = ABS( WAV( I ) - W1 ) / ( W2 - W1 )
               F2 = ABS( WAV( I ) - W2 ) / ( W2 - W1 )
               CTOP( K - 1 ) = CTOP( K - 1 ) + F1 * SCEN( I )
               CTOP( K ) = CTOP( K ) + F2 * SCEN( I )
               CBOT( K - 1 ) = CBOT( K - 1 ) + F1
               CBOT( K ) = CBOT( K ) + F2
               I = I + 1
            END IF
         END DO
 100     CONTINUE
      END DO

*   Normalise folded centroids.
      DO K = 1, NTEMS( M )
         IF ( CBOT( K ) .GT. 0.0 ) THEN
            TEMCEN( K, M ) = CTOP( K ) / CBOT( K )

         ELSE
            TEMCEN( K, M ) = 0.0
         END IF
      END DO

 999  CONTINUE

      END
