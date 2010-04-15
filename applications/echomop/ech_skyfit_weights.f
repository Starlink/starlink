      SUBROUTINE ECH_SKYFIT_WEIGHTS(
     :           NO_PIXELS,
     :           MAX_SKY_PIXELS,
     :           VNORM,
     :           REJECTS,
     :           VARMAT,
     :           DATA,
     :           ERRORS,
     :           WEIGHTS,
     :           PROFILE,
     :           STATUS
     :          )
*+
*  Name:
*     ECHOMOP - ECH_SKYFIT_WEIGHTS

*  Purpose:
*     Improve estimate of sky pixel weights.

*  Description:
*     This routine improves the weights for optimal extraction by taking
*     into account possible correlation between fitted sky values.

*  Method :
*     If SKYFIT = .TRUE., modify weights to include uncertainty in
*     sky fit optimised variances equivalent to maximising
*
*        sum over I W(I)**2 * V(I) + sum over I,J W(I)*W(J)*V(I,J)
*        ---------------------------------------------------------
*                          ( sum W(I)*F(I) ) ** 2
*
*     where:
*
*        W(I) is the weight on pixel I,
*        V(I) is the variance on the value of pixel I,
*        V(I,J) is the covariance of the skyfit between pixels I, and J,
*        F(I) is the normalised profile at pixel I.
*
*     This reduces to solving
*
*        A(I,J) W(J) = F(I)
*
*     where A(I,J) = V(I,J) I not equal to J
*
*     and
*
*        A(I,I) = V(I) + V(I,J)
*
*     NAG routine F04ASF used to solve these simultaneous equations.
*     (which makes this stage slow)

*  Invocation:
*     CALL ECH_SKYFIT_WEIGHTS(
*     :    NO_PIXELS,
*     :    MAX_SKY_PIXELS,
*     :    VNORM,
*     :    REJECTS,
*     :    VARMAT,
*     :    DATA,
*     :    ERRORS,
*     :    WEIGHTS,
*     :    PROFILE,
*     :    STATUS
*     :   )

*  Arguments:
*     NO_PIXELS = INTEGER (Given)
*        Number of pixels in increment.
*     VNORM = REAL (Given)
*        Normal variance.
*     VARMAT = REAL (Given)
*        Covariance matrix.
*     DATA = REAL (Given)
*        Intensities for each pixel.
*     WEIGHTS = REAL (Given)
*        Weights for each pixel.
*     PROFILE = REAL (Given)
*        Profile at each pixel position.
*     ERRORS = REAL (Given)
*        Variances for each pixel.
*     WEIGHTS = REAL (Given and Returned)
*        Weights for each pixel.
*     MAX_SKY_PIXELS = INTEGER (Given)
*        Maximum number of pixels in dekker (max extractable).
*     REJECTS = LOGICAL (Given)
*        TRUE if rejects allowed.
*     STATUS = INTEGER (Given and Returned)
*        Input/Output status conditions.

*  Method:
*     If no more than 41 pixels per increment then
*        First load matrix and profile
*        Outer loop thru pixels
*           If not rejecting OR error is positive then
*              Inner loop thru pixels
*                 Fill space array entry
*              End loop
*           Endif
*        End loop
*        Invert matrix
*        Load modified weights
*     Endif

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
      INCLUDE 'ECH_DATA_CONSTRAINTS.INC'

*  Arguments Given:
      INTEGER MAX_SKY_PIXELS
      REAL WEIGHTS( MAX_SLICE_PIXELS )
      INTEGER NO_PIXELS
      LOGICAL REJECTS

*  Arguments Returned:
      REAL VARMAT( MAX_SKY_PIXELS, MAX_SKY_PIXELS )
      REAL PROFILE( MAX_SLICE_PIXELS )
      REAL ERRORS( MAX_SLICE_PIXELS )
      REAL VNORM

*  Workspace:
      REAL DATA( MAX_SLICE_PIXELS )

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION SPACE( MAX_COVAR_SIZE )

      INTEGER FSTATUS
      INTEGER ITASK
      INTEGER I
      INTEGER J
      INTEGER IACT
      INTEGER JACT
      INTEGER IND
      INTEGER NRANK
      INTEGER IND1
      INTEGER IND2
      INTEGER IND3
      INTEGER IND4
*.

*  If more than 41 pixels per increment do nothing.
      IF ( NO_PIXELS .LE. 41 ) THEN
         GO TO 999
      END IF

*  First load matrix and profile.
      JACT = 0
      IND = 0

*  Outer loop through pixels.
      DO J = 1, no_pixels

*     If not rejecting OR error is positive then.
         IF ( rejects .OR. ( errors( J ) .GT. 0. ) ) THEN

*        Inner loop thru pixels.
            jact = jact + 1
            iact = 0
            DO I = 1, no_pixels

*           Fill space array entry.
               IF ( rejects .OR. ( errors( i ) .GT. 0. ) ) THEN
                  ind = ind + 1
                  iact = iact + 1
                  space( ind ) = vnorm * varmat( I, J )
                  IF ( iact .EQ. jact )
     :               space( ind ) = errors( I ) ** 2 + space( ind )
               END IF
             END DO
          END IF
      END DO

      nrank = jact
      ind1 = nrank*nrank
      DO i = 1, no_pixels
         IF ( rejects .OR. ( errors( i ) .GT. 0. ) ) THEN
            space( ind1 + I ) = profile( i )
         ENDIF
      END DO


*  Invert matrix.
      IND1 = IND1 + 1
      IND2 = IND1 + NRANK
      IND3 = IND2 + NRANK
      IND4 = IND3 + NRANK
      IF ( NRANK .GT. 0 ) THEN
         FSTATUS = 0
*  NAG version.
*         CALL F04ASF( SPACE, NRANK, SPACE( IND1 ), NRANK,
*     :        SPACE( IND2 ), SPACE( IND3 ), SPACE( IND4 ), FSTATUS )
*  PDA version.
         ITASK = 1
         CALL PDA_DGEFS( SPACE, NRANK, NRANK, SPACE( IND1 ),
     :        ITASK, IND, SPACE( IND3 ), SPACE( IND4 ), FSTATUS )
      ENDIF

*  Load modified weights.
      IACT = -1
      DO I = 1, NO_PIXELS
         IF ( REJECTS .OR. ( ERRORS( I ) .GT. 0. ) ) THEN
            IACT = IACT + 1
*  NAG version.
*            WEIGHTS( I ) = REAL( SPACE( IND2 + IACT ) )
*  PDA version.
            WEIGHTS( I ) = REAL( SPACE( IND1 + IACT ) )
         END IF
      END DO

  999 CONTINUE

      END
