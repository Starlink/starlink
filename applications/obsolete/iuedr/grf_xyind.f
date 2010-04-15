      SUBROUTINE grf_XYIND(MASK, NPOINT, X, Y, DQ, XLIM, YLIM, FIRST,
     :                     LAST)
*+
*
*   Name:
*      SUBROUTINE grf_XYIND
*
*   Description:
*      Determine first and last array indices in X-Y window.
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
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER MASK         ! data quality mask
      INTEGER NPOINT       ! number of points

      REAL X(NPOINT)       ! X-axis data
      REAL Y(NPOINT)       ! Y-axis data

      INTEGER DQ(NPOINT)   ! data quality

      REAL XLIM(2)         ! existing X-axis limits
      REAL YLIM(2)         ! existing Y-axis limits

*   Export:
      INTEGER FIRST        ! index of first point
      INTEGER LAST         ! index of last point

*   External references:
      INTEGER dq_AND       ! data quality AND

*   Local variables:
      INTEGER COUNT        ! count of valid points
      INTEGER I            ! loop index

      REAL XLS(2)          ! local copy of XLIM
      REAL YLS(2)          ! local copy of YLIM

*   Limits (increasing order)
      DO I = 1, 2
         XLS(I) = XLIM(I)
         YLS(I) = YLIM(I)
      END DO

      IF (XLS(1).GT.XLS(2)) THEN
         CALL msc_RSWAP(XLS(1), XLS(2))
      END IF

      IF (YLS(1).GT.YLS(2)) THEN
         CALL msc_RSWAP(YLS(1), YLS(2))
      END IF

      COUNT = 0

      DO I = 1, NPOINT

         IF (dq_AND(DQ(I), MASK).EQ.0) THEN

            IF (X(I).GE.XLS(1) .AND. X(I).LE.XLS(2)) THEN

               IF (Y(I).GE.YLS(1) .AND. Y(I).LE.YLS(2)) THEN

                  IF (COUNT.EQ.0) THEN
                     FIRST = I
                     LAST = I
                  ELSE
                     FIRST = MIN(FIRST, I)
                     LAST = MAX(LAST, I)
                  END IF

                  COUNT = COUNT + 1
               END IF
            END IF
         END IF
      END DO

      IF (COUNT.EQ.0) THEN
         FIRST = 1
         LAST = 0
      END IF

      END
