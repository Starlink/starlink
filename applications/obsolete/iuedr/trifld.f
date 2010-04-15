      SUBROUTINE TRIFLD(FWHM, NP1, X1, Y1, DQ1, NP2, X2, Y2, DQ2,
     :                  STATUS)

*+
*
*   Name:
*      SUBROUTINE TRIFLD
*
*   Description:
*      Fold input spectrum with triangle function.
*
*   History:
*      Jack Giddings      25-JUL-81     IUEDR Vn. 1.0
*      Paul Rees          13-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      For each point in the output x-grid, values from the input spectrum
*      are folded with a triangle function on a point integral basis.
*      Point in the output spectrum for which potential input points were
*      bad are undefined (values CAN be given, but they are wrong if there
*      is any contrast in the spectrum).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL*8 FWHM            ! FWHM of triangle function

      INTEGER NP1          ! number of points

      REAL*8 X1(NP1)         ! x-values
      REAL*8 Y1(NP1)         ! y-values

      BYTE DQ1(NP1)     ! data quality
      INTEGER NP2          ! number of points

      REAL*8 X2(NP2)         ! x-values

*   Export:
      REAL*8 Y2(NP2)         ! y-values

      INTEGER DQ2(NP2)     ! data quality
      INTEGER STATUS       ! status return

*   Local variables:
      LOGICAL LOST         ! switch to say when lower is updated

      INTEGER DIR1         ! direction of x1
      INTEGER DIR2         ! direction of x2
      INTEGER FIRST1       ! index of lowest x1 value
      INTEGER FIRST2       ! index of lowest x2 value
      INTEGER I1           ! loop index
      INTEGER I2           ! loop index
      INTEGER LAST1        ! index of highest x1 value
      INTEGER LAST2        ! index of highest x2 value
      INTEGER LOWER1       ! index of window start
      INTEGER LOWER        ! floating index of window start

      REAL*8 BOT             ! folding normalisation
      REAL*8 TOP             ! folding sum
      REAL*8 WEIGHT          ! folding weight
      REAL*8 XMAX            ! highest x-value in folding window
      REAL*8 XMIN            ! lowest x-value in folding window
      REAL*8 XWIND           ! local FWHM


*   Check array lengths
      IF (NP1.LT.2) THEN

         STATUS = -3
         RETURN

      ELSE IF (NP2.LT.1) THEN

         STATUS = -3
         RETURN

      END IF

*   Check profile width and define window size
      IF (FWHM.LE.0.0) THEN

         STATUS = -3
         RETURN

      END IF

      XWIND = FWHM

*   Check that x1 is monotonic and determine its direction
      CALL MSC_XMONOT(NP1, X1, DIR1, STATUS)

      IF (STATUS.NE.0) THEN

         RETURN

      ELSE IF (DIR1.EQ.1) THEN

         FIRST1 = 1
         LAST1 = NP1

      ELSE IF (DIR1.EQ. - 1) THEN

         FIRST1 = NP1
         LAST1 = 1

      ELSE

         STATUS = -3
         RETURN

      END IF

*   Check that x2 is monotinc and determine its direction
      CALL MSC_XMONOT(NP2, X2, DIR2, STATUS)

      IF (STATUS.NE.0) THEN

         RETURN

      ELSE IF (DIR2.EQ. - 1) THEN

         FIRST2 = NP2
         LAST2 = 1

      ELSE

         FIRST2 = 1
         LAST2 = NP2

      END IF

*   Loop over x2
      LOWER1 = FIRST1

      DO 200 I2 = FIRST2, LAST2, DIR2

         DQ2(I2) = 0
         TOP = 0.0
         BOT = 0.0
         XMIN = X2(I2) - XWIND
         XMAX = X2(I2) + XWIND
         LOWER = LOWER1
         LOST = .TRUE.

         DO 50 I1 = LOWER, LAST1, DIR1

            IF (X1(I1).GT.XMAX) GO TO 100

            IF (X1(I1).GE.XMIN) THEN

               IF (LOST) THEN

                  LOWER1 = I1
                  LOST = .FALSE.

               END IF

               IF (DQ1(I1).EQ.0) THEN

                  WEIGHT = 1.0 - ABS((X1(I1) - X2(I2))/FWHM)
                  TOP = TOP + Y1(I1)*WEIGHT
                  BOT = BOT + WEIGHT

               ELSE

                  DQ2(I2) = 1

               END IF

            END IF

 50      CONTINUE

 100     CONTINUE

         IF (BOT.GT.0.0) THEN

            Y2(I2) = TOP/BOT

         ELSE

            DQ2(I2) = 1

         END IF

 200  CONTINUE

      STATUS = 0

      END
