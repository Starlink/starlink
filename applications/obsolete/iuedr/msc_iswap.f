      SUBROUTINE MSC_ISWAP(V1, V2)
*+
*  Name:
*     SUBROUTINE MSC_ISWAP
*
*  Description:
*     Swap two integer variables.
*
*  History:
*     Jack Giddings      20-JUN-81     IUEDR Vn. 1.0
*     Paul Rees          31-OCT-88     IUEDR Vn. 2.0

*-

*  Implicit:
      IMPLICIT NONE

*  Import-Export:
      INTEGER V1     ! first variable
      INTEGER V2     ! second variable

*  Local variables:
      INTEGER V      ! temporary copy

      V = V1
      V1 = V2
      V2 = V

      END
