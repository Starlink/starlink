      SUBROUTINE grf_VQLIM( MASK, NPOINT, V, DQ, VLIM )

*+
*
*   Name:
*      SUBROUTINE grf_VQLIM
*
*   Description:
*      Determine the total limits for values subject to data quality.
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

      REAL V(NPOINT)       ! values

      INTEGER DQ(NPOINT)   ! data quality

*   Export:
      REAL VLIM(2)         ! value limits

*   External references:
      INTEGER dq_AND       ! data quality AND

*   Local variables:
      INTEGER COUNT        ! count of valid points
      INTEGER I            ! loop index

      COUNT = 0

      DO I = 1, NPOINT

         IF (dq_AND(DQ(I), MASK).EQ.0) THEN

            IF (COUNT.EQ.0) THEN
               VLIM(1) = V(I)
               VLIM(2) = V(I)
            ELSE
               VLIM(1) = MIN(VLIM(1), V(I))
               VLIM(2) = MAX(VLIM(2), V(I))
            END IF

            COUNT = COUNT + 1
         END IF
      END DO

      IF (COUNT.EQ.0) THEN
         VLIM(1) = 0.0
         VLIM(2) = 0.0
      ELSE IF (COUNT.EQ.1) THEN
         VLIM(1) = VLIM(1) - 1.0
         VLIM(2) = VLIM(2) + 1.0
      END IF

      END
