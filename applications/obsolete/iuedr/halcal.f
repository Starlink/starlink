      SUBROUTINE HALCAL
*+
*  Name:
*     SUBROUTINE HALCAL

*  Description:
*     The SFLX array is replaced by a corrected version in which
*     the current and adjacent order NET spectra are used to
*     correct for order overlap effects (roughly).

*  History:
*     Jack Giddings      25-JUN-81     IUEDR Vn. 1.0
*     Paul Rees          13-OCT-88     IUEDR Vn. 2.0
*     Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6

*-

*  Implicit:
      IMPLICIT NONE

*  External references:
      INTEGER DQ_AND        ! 32 bit integer AND operation

      LOGICAL STR_SIMLR     ! caseless string equality

*  Global includes:
      INCLUDE 'CMCAL'
      INCLUDE 'CMDISP'
      INCLUDE 'CMWAV'
      INCLUDE 'CMFLX'
      INCLUDE 'CMNET'
      INCLUDE 'CMSPEC'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMDISH'
      INCLUDE 'CMHAL'

*  Local variables:
      REAL*8 BIGC             ! Bohlin et al "C" value
      REAL*8 CAD(1200)        ! correction factors
      REAL*8 COR              ! local correction factor
      REAL*8 COR2             ! local correction factor
      REAL*8 FRAC             ! denominator in SNET correction
      REAL*8 FWHM             ! fwhm for smoothing (A)
      REAL*8 S                ! summation
      REAL*8 SAD(1200)        ! smooth net in adjacent order
      REAL*8 WAD(1200)        ! wavelengths in adjacent order
      REAL*8 WFAC             ! adjacent order wavelength conversion factor

      LOGICAL HALOR         ! whether lorentz or not

      INTEGER I             ! loop index
      INTEGER IORD          ! order index
      INTEGER N             ! accumulator
      INTEGER ORD           ! order number
      INTEGER QAD(1200)     ! smooth adjacent quality
      INTEGER STATUS        ! local status

*  Exit if not sensible
      IF ( NONET .OR. NOCAL ) THEN
         RETURN

      ELSE IF ( NODISP .OR. NOHAL ) THEN
         RETURN

      ELSE IF ( HALC .LE. 0.0 ) THEN
         RETURN
      END IF

*  Set up parameters
      HALOR = STR_SIMLR( 'LORENTZ\\', HALTP )
      STATUS = 0
      CALL HITEM( 1, STATUS )
      IF ( STATUS .NE. 0 ) RETURN

      CALL ORSET( ORDER )

      IF ( HALOR ) THEN
         BIGC = 0.0

      ELSE
         BIGC = HALC * (WC - HALW0) / (HALWC - HALW0)
         BIGC = MAX(BIGC, 0.0d0)
      END IF

      IF ( BIGC .LE. 0.0 ) RETURN

*  Set correction to zero and set up coordinate grid
      DO I = 1, NWAV
         CAD(I) = 0.0
      END DO

*  Go through adjacent orders
      DO ORD = ORDER - 1, ORDER + 1
         CALL FNORD( ORD, IORD )
         IF ( IORD .GT. 0 ) THEN
            IF ( ORD .EQ. ORDER ) THEN
               COR = 1.636 * BIGC
               COR2 = 2.5 * BIGC

            ELSE
               COR = BIGC * (0.5 + 1.636 * BIGC)
               COR2 = BIGC * (0.5 + 1.25 * BIGC)
            END IF

            CALL ORSET( ORD )
            WFAC = DBLE(ORDER) / DBLE(ORD)

            DO I = 1, NWAV
               WAD(I) = WFAC * WAV(I)
            END DO

            FWHM = HALAV / DRDW
            COR = COR2

            CALL TRIFLD( FWHM, NWAVS(IORD), WAVS(1, IORD),
     :                   SNETS(1, IORD), QNETS(I, IORD),
     :                   NWAV, WAD, SAD, QAD, STATUS )

            IF ( STATUS .EQ. 0 ) THEN
               DO I = 1, NWAV
                  CAD(I) = CAD(I) + COR * SAD(I)
               END DO
            END IF
         END IF
      END DO

*  Correct the NET and put result into FLX
      N = 0
      S = 0.0
      FRAC = 1.0

      DO I = 1, NWAV
         QFLX(I) = QNET(I)
         IF ( DQ_AND(QNET(I), 1) .EQ. 0 ) THEN
            SFLX(I) = SNET(I) + CAD(I)
            SFLX(I) = SFLX(I) / FRAC
            DFLX(I) = DFLX(I) / FRAC
            N = N + 1
            S = S + CAD(I)
         END IF
      END DO

*  No need to divide if N equals 1
      IF ( N .GE. 2 ) S = S / DBLE(N)
      END
