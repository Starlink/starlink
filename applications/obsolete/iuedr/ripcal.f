      SUBROUTINE RIPCAL(STATUS)
*+
*  Name:
*     SUBROUTINE RIPCAL
*
*  Description:
*     The contents of CMFLX are scaled by dividing by the
*     Ripple Calibration defined by CMRIP.
*
*  History:
*     Jack Giddings      25-JUN-81     IUEDR Vn. 1.0
*     Paul Rees          07-OCT-88     IUEDR Vn. 2.0
*
*  Method:
*
*-

*  Implicit:
      IMPLICIT NONE

*  Export:
      INTEGER STATUS      ! status return

*  External references:
      REAL*8 MSC_EVPOLY   ! polynomial evaluation

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMWAV'
      INCLUDE 'CMRIP'
      INCLUDE 'CMCUT'
      INCLUDE 'CMCAL'

*  Local variables:
      REAL*8 CS(6)          ! coefficients
      REAL*8 DXDW           ! scale between wavelength and ripple
      REAL*8 RIPA           ! local ripple alpha value
      REAL*8 RIPC           ! local ripple constant
      REAL*8 SCALE          ! local scaling factor (sinc function)
      REAL*8 WAVC           ! central wavelength of echelle order
      REAL*8 WLIM(2)        ! wavelength limits
      REAL*8 XX             ! argument for sinc function

      INTEGER I           ! loop index
      INTEGER IORDR       ! ripple order index
      INTEGER IORDW       ! wavelength order index
      INTEGER NC          ! number of poly coefficients

      IF (.NOT.NORIP) THEN

*     Find index in RIPO data
         CALL FNRIP(ORDER, IORDR)

*     RIPO or global
         IF (IORDR.GT.0) THEN
            RIPC = RIPKS(IORDR)
            RIPA = RIPAS(IORDR)
            NC = NRIPCS(IORDR)

            IF ( NC .GT. 0 ) THEN
               DO I = 1, NC
                  CS(I) = RIPCS(I, IORDR)
               END DO
            END IF

         ELSE
            RIPC = MSC_EVPOLY(NRIPM, RIPM, DBLE(ORDER))
            RIPA = RIPALF
            NC = 0

         END IF

*     Relationship between wavelength and X
         WAVC = RIPC/ORDER
         DXDW = 3.14159*ORDER*RIPA/WAVC

*     Wavelength limits
         WLIM(1) = WAVAIR(1)
         WLIM(2) = WAVAIR(NWAV)

         IF ( XRLIM(2) .GT. XRLIM(1) ) THEN
            DO I = 1, 2
               WLIM(I) = XRLIM(I) / DXDW + WAVC
            END DO
         END IF

*     Find index in CUT data
         CALL FNCUT(ORDER, IORDW)

*     Use specific wavelength limits
         IF (IORDW.GT.0) THEN

            IF (CUTW2(IORDW).GT.CUTW1(IORDW)) THEN

               WLIM(1) = CUTW1(IORDW)
               WLIM(2) = CUTW2(IORDW)

            END IF

         END IF

*     Divide by (sin(x)/x)**2
         DO 50 I = 1, NWAV

            IF (QCAL(I).EQ.0) THEN

               IF (WAVAIR(I).GE.WLIM(1) .AND. WAVAIR(I).LE.WLIM(2)) THEN

                  XX = (WAVAIR(I) - WAVC)*DXDW
                  XX = MIN(ABS(XX), 3.0d0)

                  IF (XX.GT.0.01) THEN

                     SCALE = (SIN(XX)/XX)**2

                  ELSE

                     SCALE = 1.0

                  END IF

                  IF (NC.GT.0) SCALE = SCALE*MSC_EVPOLY(NC, CS, XX)
                  SCAL(I) = SCAL(I)*SCALE

               ELSE

                  QCAL(I) = 1

               END IF

            END IF

 50      CONTINUE

      END IF

      STATUS = 0

      END
