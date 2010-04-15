      SUBROUTINE MSC_XMONOT(NPOINT, X, DIR, STATUS)

*+
*
*   Name:
*      SUBROUTINE MSC_XMONOT
*
*   Description:
*      Determine whether X is monotonic and determine direction.
*
*   History:
*      Jack Giddings      25-JUL-81     IUEDR Vn. 1.0
*      Paul Rees          28-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      If the x-array has only one point, then it has no direcion.
*      Otherwise determine whether it has a single unique direction.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NPOINT     ! number of points

      REAL*8 X(NPOINT)     ! x-values

*   Export:
      INTEGER DIR        ! direction (1=increasing, -1=decreasing)
      INTEGER STATUS     ! status return

*   Local variables:
      INTEGER I          ! loop index

*   Check out the number of points
      IF (NPOINT.LT.1) THEN

         STATUS = -3
         RETURN

      ELSE IF (NPOINT.EQ.1) THEN

         DIR = 0
         STATUS = 0
         RETURN

      END IF

*   Get initial direction from first 2 points
      IF (X(2).GT.X(1)) THEN

         DIR = 1

      ELSE

         DIR = -1

      END IF

*   Go through comparing adjacent values
      DO 100 I = 2, NPOINT

         IF (DIR.LT.0 .AND. X(I).GE.X(I - 1)) THEN

            STATUS = -3
            RETURN

         ELSE IF (DIR.GT.0 .AND. X(I).LE.X(I - 1)) THEN

            STATUS = -3
            RETURN

         END IF

 100  CONTINUE

      STATUS = 0

      END
