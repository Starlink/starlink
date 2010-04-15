      SUBROUTINE ELF1_LEVEL(THRESH,XO,YO,ELEMS,ARRAY,
     :                      PRANGE,RADIUS,STATUS)
*+
*  Name:
*     ELF1_LEVEL

*  Purpose:
*     Provides an estimate of how far out from the image centre you must
*     normally go before the average count is below the threshold level.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ELF1_LEVEL(THRESH,XO,YO,ELEMS,ARRAY,PRANGE,
*                     RADIUS,STATUS)

*  Description:
*     Looks outward from the chosen origin location along lines separated by
*     45 degrees. Determines how far along each of these lines you must
*     look to reach a pixel count value below the threshold value.
*
*  Arguments:
*     THRESH = REAL (Given)
*        Threshold pixel brightness value. Units counts.
*     XO = REAL (Given)
*        Suggested X co-ordinate for the galaxy centre.
*     YO = REAL (Given)
*        Suggested Y co-ordinate for the galaxy centre.
*     ELEMS = INTEGER (Given)
*        Number of pixels in the image.
*     ARRAY(ELEMS) = REAL (Given)
*        The image array.
*     PRANGE(2) = INTEGE (Given)
*        Size of the image axes. Units pixels.
*     RADIUS = REAL (Returned)
*        First radius of the galaxy to try. Units pixels.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     GJP: Grant Privett (STARLINK)

*  History:
*     4-AUG-1993 (GJP)
*     (Original version)

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants
      INCLUDE 'ELF_PAR'               ! ELLFOU constants

*  Status:
      INTEGER STATUS                  ! Global status

*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the image
      INTEGER PRANGE(2)               ! Size of the image
      REAL ARRAY(ELEMS)               ! Image array
      REAL THRESH                     ! Threshold count value
      REAL XO                         ! Galaxy centre co-ords
      REAL YO                         ! Galaxy centre co-ords

*  Arguments Returned:
      REAL RADIUS                     ! Radius estimate

*  Local variables:
      INTEGER FAR                     ! Distance out from the origin from
                                      ! which a search for a threshold
                                      ! should start
      INTEGER FOUND(10)               ! Was a distance value found
                                      ! for a given angle
      INTEGER I                       ! Temporary storage
      INTEGER J                       ! Temporary storage
      INTEGER XI(8)                   ! X axis increment
      INTEGER YI(8)                   ! Y axis increment
      REAL ADD                        ! Array address
      REAL DIST(10)                   ! Distance from the galaxy centre
      REAL SUM                        ! Temporary storage of a sum
      REAL VALUE                      ! Temporary storage
      REAL X                          ! Temporary X co-ordinate
      REAL Y                          ! Temporary Y co-ordinate
*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Setup arrays containing the increments in X and Y for examining lines
*   of pixels outward from the proposed galaxy origin at angles of
*   0, 45, 90, 135, 180, 225, 270, 315 respectively.

*   X increments.
      XI(1)=0
      XI(2)=1
      XI(3)=1
      XI(4)=1
      XI(5)=0
      XI(6)=-1
      XI(7)=-1
      XI(8)=-1

*   Y increments.
      YI(1)=1
      YI(2)=1
      YI(3)=0
      YI(4)=-1
      YI(5)=-1
      YI(6)=-1
      YI(7)=0
      YI(8)=-1

*   Find a distance from the galaxy which will be beyond the image
*   bounds for every direction.
      FAR=INT(SQRT(1.*PRANGE(1)*PRANGE(1)+1.*PRANGE(2)*PRANGE(2)))

*   Look along lines inward toward the centre of the galaxy to find out
*   at what distance the pixel count value drops below the threshold.
      DO 40 J=1,8

*      Clear the arrays required.
         DIST(J)=0.0
         FOUND(J)=0.0

*      Look inward along the required lines.
         DO 50 I=FAR,1,-1

*         Calculate the pixel co-ordinate.
            X=NINT(XO)+I*XI(J)
            Y=NINT(YO)+I*YI(J)

*         Check that the pixel is within the image.
            IF ((X.GE.1).AND.(X.LE.PRANGE(1)).AND.(Y.GE.1)
     :           .AND.(Y.LE.PRANGE(2))) THEN

*            Find the array address of the pixel required.
               ADD=(Y-1)*PRANGE(1)+X

*            Get its value.
               VALUE=ARRAY(INT(ADD))

*            Check that the value is not bad.
               IF (VALUE.NE.VAL__BADR) THEN

*               Only act if the value is still below the threshold
                  IF (VALUE.LT.THRESH) THEN

*                  Update the estimate of the distance away from the centre
*                  at which the threshold is crossed.
                     DIST(J)=I
                     FOUND(J)=1

                  END IF

               END IF

            END IF

 50      CONTINUE

 40   CONTINUE

*   Average estimates from the pairs of lines.
      DO 60 I=1,4

*      Average the result if a value was found for both lines
*      of a pair.
         IF ((FOUND(I).GT.0).AND.(FOUND(I+4).GT.0)) THEN

            DIST(I)=(DIST(I)+DIST(I+4))/2.

         ELSE

*         Take the only value found if one of the pair did not have a value
*         assigned.
            IF ((FOUND(I).GT.0).OR.(FOUND(I+4).GT.0)) THEN
               DIST(I)=DIST(I)+DIST(I+4)
               FOUND(I)=1
            END IF

         END IF

*      Allow for the diagonal lines being root(2) longer.
         IF ((I.EQ.2).OR.(I.EQ.4)) DIST(I)=DIST(I)*SQRT(2.)

 60   CONTINUE

*   Average the values.
      J=0
      SUM=0.0
      DO 70 I=1,4

*      Check that a value was found for the current pair of diametrically
*      opposed lines outward from the origin.
         IF (FOUND(I).GT.0) THEN

*         Add to the sum.
            J=J+1
            SUM=DIST(I)+SUM

         END IF

 70   CONTINUE

*   Assign final value.
      IF (J.GT.0) THEN
          RADIUS=SUM/REAL(J)
      ELSE
          RADIUS=-1.0
      END IF

 9999 CONTINUE

      END
