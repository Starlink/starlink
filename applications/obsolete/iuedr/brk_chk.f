      SUBROUTINE BRK_CHK( KS, NOKS, M1, M2, TKS, TNOKS, NSM, NSIG )
*+
*  Name:
*     SUBROUTINE BRK_CHK

*  Purpose:
*     To flag points in the raw data file KS which differ
*     at the NSIG sigma level from data in the array of
*     smoothed data points, TKS

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BRK_CHK( KS, NOKS, M1, M2, TKS, TNOKS, NSM, NSIG )

*  Arguments:
*     M1 = INTEGER (Given)
*        Index of first Order to be processed.
*     M2 = INTEGER (Given)
*        Index of last order to be processed.
*     KS = REAL*8( M1 : M2 ) (Given)
*
*     NOKS = LOGICAL( M1 : M2 ) (Given)
*
*     TKS = REAL*8( 150 ) (Given)
*
*     TNOKS = INTEGER ( 150 ) (Returned)
*
*     NSM = INTEGER (Returned)
*
*     NSIG = INTEGER (Returned)
*

*  Method:
*     See Barker (1984).

*  Authors:
*     IDH: Ian Howarth (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     ??-AUG-84 (IDH):
*       IUEDR Vn. 1.3
*     29-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     14-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      INTEGER M1
      INTEGER M2

      REAL*8 KS( M1 : M2 )

      LOGICAL NOKS( M1 : M2 )

      REAL*8 TKS( 150 )

*  Arguments Returned:
      INTEGER TNOKS( 150 )
      INTEGER NSM
      INTEGER NSIG

*  Local Variables:
      REAL*8 DIFF
      REAL*8 SD
      REAL*8 SUMSQ

      INTEGER I
      INTEGER IKS( 150 )
      INTEGER ITKS( 150 )
      INTEGER I1
      INTEGER I2
      INTEGER J
      INTEGER SUMWT
      INTEGER WEIGHT
*.

*  Initialise IKS and ITKS.
      DO I = M1, M2
         IKS( I ) = 1
         ITKS( I ) = 1
         IF ( NOKS( I ) ) THEN
            IKS( I ) = 0
         END IF
         IF ( TNOKS( I ) .NE. 0 ) THEN
            ITKS( I ) = 0
         END IF
      END DO

*  Loop over all data.
      DO I = M1, M2

*     Calculate running 's.d.' over +/- NSM channels.
         I1 = MAX( M1, I - NSM )
         I2 = MIN( M2, I + NSM )

*     Initialise accumulators.
         SUMSQ = 0.0
         SUMWT = 0

         DO J = I1, I2

*        Weight is 0 for either point bad; 1 if both points good.
            WEIGHT = ITKS( J ) * ABS( IKS( J ) )
            DIFF = KS( J ) - TKS( J )
            SUMSQ = SUMSQ + WEIGHT * DIFF * DIFF
            SUMWT = SUMWT + WEIGHT
         END DO

*     Set points to be rejected (ultimately) to Q=-1, thus
*     retaining them as 'good' for the current iteration.
         IF ( SUMWT.GT.1 .AND. SUMSQ.GT.0.0 ) THEN
            SD = SQRT( SUMSQ / ( SUMWT - 1 ) )
            DO J = I1, I2
               DIFF = ABS( KS( J ) - TKS( J ) )
               IF ( ( DIFF / ( NSIG * SD ) ) .GT. 1.0 ) THEN
                  IKS( J ) = -1
               END IF
            END DO
         END IF
      END DO

*  Load final NOKS before returning.
      DO J = M1, M2
         I1 = MAX( 0, IKS( J ) )
         NOKS( J ) = .FALSE.
         IF ( I1 .EQ. 0 ) THEN
            NOKS( J ) = .TRUE.
         END IF
      END DO

      END
