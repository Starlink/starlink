      SUBROUTINE BRK_KCORR( M1, M2, FCORRM1, FCORRM2, WV1, WV2,
     :                      KM, ALPHA, KCORR, OK )
*+
*  Name:
*     SUBROUTINE BRK_KCORR
*
*  Description:
*     Calculates correction to ripple parameter 'k' using
*     Barker's algorithm.
*
*  History:
*     Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*     Paul Rees          30-OCT-88     IUEDR Vn. 2.0
*     Martin Clayton     08-OCT-94     IUEDR Vn. 3.1-6
*
*  Method:
*     See Barker (1984).
*
*-

*  Implicit:
      IMPLICIT NONE

*  Import:
      INTEGER M1       ! order number 1
      INTEGER M2       ! order number 2

      REAL*8 FCORRM1   ! mean ripple-corrected flux in overlap region (order 1)
      REAL*8 FCORRM2   ! mean ripple-corrected flux in overlap region (order 2)
      REAL*8 WV1       ! mean wavelength of overlap region (order 1)
      REAL*8 WV2       ! mean wavelength of overlap region (order 2)
      REAL*8 KM        ! ripple parameter
      REAL*8 ALPHA     ! ripple parameter

*  Export:
      REAL*8 KCORR     ! correction to `k' to make FCORRM1=FCORRM2

*  Import-Export:
      LOGICAL OK       ! T on entry;  set F on error

*  External references:
      REAL*8 BRK_DRK
      REAL*8 BRK_RPC

*  Local variables:
      REAL*8 DENOM
      REAL*8 DRK1
      REAL*8 DRK2
      REAL*8 NUMER
      REAL*8 RATIO
      REAL*8 RFAC1
      REAL*8 RFAC2
*.

*  Calculate partial d(R)/d(k)
      RFAC1 = BRK_RPC( KM, ALPHA, M1, WV1 )
      RFAC2 = BRK_RPC( KM, ALPHA, M2, WV2 )
      DRK1 = BRK_DRK( KM, ALPHA, M1, WV1, RFAC1 )
      DRK2 = BRK_DRK( KM, ALPHA, M2, WV2, RFAC2 )

*  Calculate Delta(k)
      RATIO = FCORRM1 / FCORRM2
      NUMER = RATIO - 1.0
      DENOM = DRK1 / RFAC1 - RATIO * DRK2 / RFAC2

*  Trap for zero denomenator
      IF ( DENOM .EQ. 0.0 ) THEN
         OK = .FALSE.
         KCORR = 0.0

      ELSE
         KCORR = NUMER / DENOM
      END IF

      END
