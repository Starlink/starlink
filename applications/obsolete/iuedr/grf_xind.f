      SUBROUTINE grf_XIND(MASK, NPOINT, X, DQ, XLIM, FIRST, LAST)

*+
*
*   Name:
*      SUBROUTINE grf_XIND
*
*   Description:
*      Determine start and end array indices in X for range defined by
*      XLIM.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      16-MAY-81
*         AT4 version.
*      Paul Rees          01-SEP-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*         Conversion to GKS 7.2 graphics.
*      Paul Rees         09-MAY-89     IUEDR Vn. 2.1
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
      REAL X(NPOINT)       ! X-axis values
      INTEGER DQ(NPOINT)   ! data quality
      REAL XLIM(2)         ! existing X-axis limits

*   Export:
      INTEGER FIRST        ! index of first point
      INTEGER LAST         ! index of last point

*   External references:
      INTEGER dq_AND       ! data quality AND

*   Local variables:
      INTEGER COUNT        ! count of valid points
      INTEGER I            ! loop index

      COUNT = 0

      DO I = 1, NPOINT

         IF (dq_AND(DQ(I), MASK).EQ.0) THEN

            IF (X(I).GE.XLIM(1) .AND. X(I).LE.XLIM(2)) THEN

               IF ( COUNT .EQ. 0 ) THEN
                  FIRST = I
                  LAST = I

               ELSE
                  FIRST = MIN(FIRST, I)
                  LAST = MAX(LAST, I)
               END IF

               COUNT = COUNT + 1
            END IF
         END IF
      END DO

      IF (COUNT.EQ.0) THEN
         FIRST = 1
         LAST = 0
      END IF

      END
