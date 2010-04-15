      SUBROUTINE dq_ZERO(NPOINT, DQ)

*+
*
*   Name:
*     SUBROUTINE dq_ZERO
*
*   Description:
*      Zero out a DQ array.
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
*      The data quality array is zeroed out.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NPOINT      ! number of points in array

*   Export:
      BYTE DQ(NPOINT)     ! data quality array

*   Local variables:
      INTEGER I           ! loop index

      DO I = 1, NPOINT
         DQ(I) = 0
      END DO

      END
