      SUBROUTINE grf_XYLIM(XLIM, MASK, NPOINT, X, Y, DQ, YLIM)

*+
*
*   Name:
*      SUBROUTINE grf_XYLIM
*
*   Description:
*      Determine Y-axis limits subject to existing X-axis limits.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings     16-MAY-81
*         AT4 version.
*      Paul Rees         12-JAN-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         08-MAY-89     IUEDR Vn. 2.1
*         Some restructuring and final conversion to SGP/16 style.
*
*   Method:
*
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      REAL XLIM(2)         ! existing X-axis limits

      INTEGER NPOINT       ! number of points
      INTEGER MASK         ! data quality mask

      REAL X(NPOINT)       ! X-axis values
      REAL Y(NPOINT)       ! Y-axis values

      INTEGER DQ(NPOINT)   ! data quality

*   Export:
      REAL YLIM(2)         ! Y-axis limits

*   External references:
      INTEGER dq_AND       ! data quality AND

*   Local variables:
      INTEGER COUNT        ! count of valid points
      INTEGER I            ! loop index

      COUNT = 0

      DO I = 1, NPOINT

         IF (dq_AND(DQ(I), MASK).EQ.0) THEN

            IF (X(I).GE.XLIM(1) .AND. X(I).LE.XLIM(2)) THEN

               IF (COUNT.EQ.0) THEN
                  YLIM(1) = Y(I)
                  YLIM(2) = Y(I)
               ELSE
                  YLIM(1) = MIN(YLIM(1), Y(I))
                  YLIM(2) = MAX(YLIM(2), Y(I))
               END IF

               COUNT = COUNT + 1
            END IF
         END IF
      END DO

      IF (COUNT.EQ.0) THEN
         YLIM(1) = 0.0
         YLIM(2) = 0.0
      ELSE IF (COUNT.EQ.1) THEN
         YLIM(1) = YLIM(1) - 1.0
         YLIM(2) = YLIM(2) + 1.0
      END IF

      END
