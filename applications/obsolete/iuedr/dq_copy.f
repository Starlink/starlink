      SUBROUTINE dq_COPY(NPOINT, DQ1, DQ2)

*+
*
*   Name:
*      SUBROUTINE dq_COPY
*
*   Description:
*      Copy a DQ array.
*
*   Authors:
*      Jack Giddings
*
*   History:
*      Jack Giddings      04-NOV-81
*         AT4 version.
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*         Conversion to FORTRAN.
*      Paul Rees          23-MAY-89     IUEDR Vn. 2.1
*         Conversion to SGP/16 style.
*
*   Method:
*      The data quality array is copied.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NPOINT       ! number of points in arrays

      BYTE DQ1(NPOINT)     ! input array

*   Export:
      BYTE DQ2(NPOINT)     ! output array

*   Local variables:
      INTEGER I            ! loop index

      DO I = 1, NPOINT
         DQ2(I) = DQ1(I)
      END DO

      END
