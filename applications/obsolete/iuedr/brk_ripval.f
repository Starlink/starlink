      SUBROUTINE BRK_RIPVAL( MORD, IMW1, IMW2, NPTS, FLX, WV, Q,
     :                       K, ALPHA, FCORRM, WMEAN, OK )

*+
*
*   Name:
*      SUBROUTINE BRK_RIPVAL
*
*   Description:
*      Evaluates mean ripple-corrected flux and wavelength
*      in notional overlap region between orders.
*
*   History:
*      Ian Howarth        ??-AUG-84     IUEDR Vn. 1.3
*      Paul Rees          30-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      See Barker (1984).
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER MORD        ! order number
      INTEGER IMW1        ! start index of WV array
      INTEGER IMW2        ! end index of WV array
      INTEGER NPTS        ! number of FLX, WV points
      REAL*8 FLX(NPTS)    ! array of nett fluxes
      REAL*8 WV(NPTS)     ! array of wavelengths
      INTEGER Q(NPTS)     ! array of associated data qualities (0=good)
      REAL*8 K            ! ripple parameter 'k'
      REAL*8 ALPHA        ! ripple parameter

*   Export:
      REAL*8 FCORRM       ! mean ripple corrected flux
      REAL*8 WMEAN        ! mean wavelength of valid data

*   Import-Export:
      LOGICAL OK          ! imported as T; set F on failure

*   External references:
      REAL*8 BRK_RPC

*   Local variables:
      REAL*8 SUM

      INTEGER I
      INTEGER ISUM

*   Initialise accumulators
      FCORRM = 0.0
      WMEAN = 0.0
      SUM = 0.0
      ISUM = 0

*   Calculate summations
      DO I = IMW1, IMW2

*       BRK_RPC function returns ripple correction factor
         IF ( Q(I) .EQ. 0 ) THEN
            SUM = SUM + BRK_RPC( K, ALPHA, MORD, WV(I) )
            ISUM = ISUM + 1
            FCORRM = FCORRM + FLX(I)
            WMEAN = WMEAN + WV(I)
         END IF
      ENDDO

*   Trap for no data
      IF ( ISUM .LE. 0 ) THEN
         OK = .FALSE.
         FCORRM = 0.0
         WMEAN = 0.0

      ELSE
         FCORRM = FCORRM / SUM
         WMEAN = WMEAN / DBLE(ISUM)
      END IF
      END
