      SUBROUTINE MSC_RSWAP( V1, V2 )

*+
*
*   Name:
*      SUBROUTINE MSC_RSWAP
*
*   Description:
*      Swap two REAL variables.
*
*   History:
*      Jack Giddings      20-JUN-81     IUEDR Vn. 1.0
*      Paul Rees          25-OCT-88     IUEDR Vn. 2.0
*
*   Method:
*      Obvious.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import-Export:
      REAL V1     ! first variable
      REAL V2     ! second variable

*   Local variables:
      REAL V      ! temporary copy

      V = V1
      V1 = V2
      V2 = V

      END

      SUBROUTINE MSC_DSWAP( V1, V2 )

*+
*
*   Name:
*      SUBROUTINE MSC_RSWAP
*
*   Description:
*      Swap two REAL*8 variables.
*
*   History:
*      Martin Clayton     07-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      Obvious.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import-Export:
      REAL*8 V1     ! first variable
      REAL*8 V2     ! second variable

*   Local variables:
      REAL*8 V      ! temporary copy

      V = V1
      V1 = V2
      V2 = V

      END
