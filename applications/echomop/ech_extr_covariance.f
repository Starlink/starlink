      SUBROUTINE ECH_EXTR_COVARIANCE( MAX_SKY_PIXELS, INP_NPOLY,
     :           SPACE, DEK_BELOW, DEK_ABOVE, SKY_MASK, VARMAT, STATUS )
*+
*  Name:
*     ECHOMOP - ECH_EXTR_COVARIANCE

*  Purpose:
*     Calculates covariance matrix for sky estimate

*  Description:
*     For accurate computation of the uncertainties the variance
*     in the sky estimate must be included.
*     This contributes a term Sum over i and j of w(i)w(j)V(i,j) where
*     V(i,j) is the co-variance between the sky estimates on
*     pixel i and j and w(i) is the weight used on the ith pixel. The
*     variance matrix has to be estimated from the sky pixels used and
*     the polynomial order. We make the approximation of constant variance
*     on each sky pixel for a given row.
*     First compute the matrix to be inverted (30/06/88 TRM)

*  Invocation:
*     CALL ECH_EXTR_COVARIANCE( MAX_SKY_PIXELS, INP_NPOLY,
*    :     SPACE, DEK_BELOW, DEK_ABOVE, SKY_MASK, VARMAT, STATUS )

*  Arguments:
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum number of pixels in increment.
*     INP_NPOLY = INTEGER (Given)
*        Degree of polynomial used to model sky.
*     DEK_BELOW = INTEGER (Given)
*        Position of lower edge of order.
*     DEK_ABOVE = INTEGER (Given)
*        Position of upper edge of order.
*     SPACE = DOUBLE (Given and Returned)
*        Space array for fitting.
*     VARMAT = REAL (Returned)
*        Co-variance matrix.
*     SKY_MASK = INTEGER (Given and Returned)
*        Mask flagging which pixels contribute to the sky.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     Calculate covariance probabilities
*     Store identity matrix
*     Invert with NAG routine F04AEF
*     Compute unscaled variance matrix

*  Bugs:
*     None known.

*  Authors:
*     Dave Mills STARLINK (ZUVAD::DMILLS)

*  History:
*     1992 Sept 1 : Initial release

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER DEK_ABOVE
      INTEGER DEK_BELOW
      INTEGER MAX_SKY_PIXELS
      INTEGER SKY_NPOLY
      INTEGER INP_NPOLY

*  Arguments Returned:
      DOUBLE PRECISION SPACE( MAX_COVAR_SIZE )
      REAL VARMAT( MAX_SKY_PIXELS, MAX_SKY_PIXELS )
      INTEGER SKY_MASK( -MAX_SKY_PIXELS / 2 : MAX_SKY_PIXELS / 2 )

*  Status:
      INTEGER STATUS

*  Local Variables:
      REAL SUMD
      REAL XIK
      REAL XJL
      REAL XI
      REAL XJ

      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER L
      INTEGER ITASK
      INTEGER IND
      INTEGER IND1
      INTEGER IND2
      INTEGER IND3
      INTEGER IND4
      INTEGER IND5

*  Functions Called:
      EXTERNAL ECH_FATAL_ERROR
      LOGICAL ECH_FATAL_ERROR
*.

*  If we enter with a fatal error code set up, then RETURN immediately.
      IF ( ECH_FATAL_ERROR( STATUS ) ) RETURN

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

      SKY_NPOLY = INP_NPOLY
      IF ( SKY_NPOLY .GT. 15 ) SKY_NPOLY = SKY_NPOLY / 2 - 7

*  Calculate covariance probabilities.
      DO J = 1, SKY_NPOLY
         DO I = 1, J
            SUMD = 0.0
            L = I + J - 2
            DO K = 1, MAX_SKY_PIXELS
               IF ( SKY_MASK( K - MAX_SKY_PIXELS / 2 - 1 ) .EQ. 1 ) THEN
                  IF ( L .EQ. 0 ) THEN
                     SUMD = SUMD + 1.0

                  ELSE IF ( L .EQ. 1 ) THEN
                     SUMD = SUMD + REAL( K ) / REAL( MAX_SKY_PIXELS )

                  ELSE
                     SUMD = SUMD + ( REAL( K ) /
     :                      REAL( MAX_SKY_PIXELS ) ) ** L
                  END IF
               END IF
            END DO
            SPACE( SKY_NPOLY * ( J - 1 ) + I ) = SUMD
            SPACE( SKY_NPOLY * ( I - 1 ) + J ) = SUMD
         END DO
      END DO

*  Store identity matrix.
      DO I = SKY_NPOLY * SKY_NPOLY + 1, 2 * SKY_NPOLY * SKY_NPOLY
         SPACE( I ) = 0.D0
      END DO
      IND1 = SKY_NPOLY * SKY_NPOLY
      DO I = 1, SKY_NPOLY
         SPACE( IND1 + SKY_NPOLY * ( I - 1 ) + I ) = 1.D0
      END DO

*  Invert with NAG routine F04AEF.
      IND1 = IND1 + 1
      IND2 = IND1 + SKY_NPOLY * SKY_NPOLY
      IND3 = IND2 + SKY_NPOLY * SKY_NPOLY
      IND4 = IND3 + SKY_NPOLY
      IND5 = IND4 + SKY_NPOLY * SKY_NPOLY

*  NAG Version.
*      CALL F04AEF( SPACE, SKY_NPOLY, SPACE( IND1 ), SKY_NPOLY,
*     :     SKY_NPOLY, SKY_NPOLY, SPACE( IND2 ), SKY_NPOLY,
*     :     SPACE( IND3 ), SPACE( IND4 ), SKY_NPOLY,
*     :     SPACE( IND5 ), SKY_NPOLY, STATUS )

*  PDA Version.
      ITASK = 1
      STATUS = 0
      CALL PDA_DGEFS( SPACE, SKY_NPOLY, SKY_NPOLY, SPACE( IND1 ),
     :     ITASK, IND, SPACE( IND3 ), SPACE( IND4 ), STATUS )
      ITASK = 2
      DO I = 2, SKY_NPOLY
         CALL PDA_DGEFS( SPACE, SKY_NPOLY, SKY_NPOLY,
     :        SPACE( IND1 + ( I - 1 ) * SKY_NPOLY ),
     :        ITASK, IND, SPACE( IND3 ), SPACE( IND4 ), STATUS )
      END DO

*  Inverted matrix held in space(ind2) for NAG version.
*      IND2 = IND2 - 1

*  Inverted matrix held in space(ind1) for PDA version.
      IND1 = IND1 - 1

*  Compute unscaled variance matrix.
      DO J = 1, DEK_ABOVE - DEK_BELOW + 1
         XJ = REAL( MAX_SKY_PIXELS / 2 + DEK_BELOW + J - 1 ) /
     :        REAL( MAX_SKY_PIXELS )
         DO I = 1, J
            XI = REAL( MAX_SKY_PIXELS / 2 + DEK_BELOW + I - 1 ) /
     :           REAL( MAX_SKY_PIXELS )
            SUMD = 0.0
            DO K = 1, SKY_NPOLY
               IF ( K .EQ. 1 ) THEN
                  XIK = 1.0

               ELSE IF ( K .EQ. 2 ) THEN
                  XIK = XI

               ELSE
                  XIK = XI ** ( K - 1 )
               END IF
               DO L = 1, SKY_NPOLY
                  IF ( L .EQ. 1 ) THEN
                     XJL = 1.0

                  ELSE IF ( L .EQ. 2 ) THEN
                     XJL = XJ

                  ELSE
                     XJL = XJ ** ( L - 1 )
                  END IF
*  NAG version.
*                  SUMD = SUMD + XIK * SPACE( IND2 + SKY_NPOLY *
*     :                   ( K - 1 ) + L ) * XJL

*  PDA version.
                  SUMD = SUMD + XIK * SPACE( IND1 + SKY_NPOLY *
     :                   ( K - 1 ) + L ) * XJL
               END DO
            END DO
            VARMAT( I, J ) = SUMD
            VARMAT( J, I ) = SUMD
         END DO
      END DO

      END
