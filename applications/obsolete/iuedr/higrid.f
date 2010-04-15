      SUBROUTINE HIGRID( STATUS )

*+
*
*   Name:
*      SUBROUTINE HIGRID
*
*   Description:
*      Define wavelength grid for HIRES order.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     04-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*     If CUTWV=TRUE then attempt to use echelle order wavelength limits
*     in CMCUT.
*     Otherwise, the wavelength grid is defined between "ripple"
*     x-coordinates (-PI,+PI).
*     The sample rate is determined from that given i ngeometric pixels
*     and from the dispersion at the central wavelength.
*     The grid is delimited at either end by the requirement to be
*     contained within the circular face-plate boundary.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Export:
      INTEGER STATUS     ! status return

*   Global variables:
      INCLUDE 'CMDISH'
      INCLUDE 'CMFACE'
      INCLUDE 'CMEXTP'
      INCLUDE 'CMCUT'
      INCLUDE 'CMWAV'

*   Local variables:
      REAL*8 DIST        ! distance from face-plate centre
      REAL*8 L           ! L-coordinate
      REAL*8 S           ! S-coordinate
      REAL*8 W           ! W-coordinate
      REAL*8 W1          ! order cutoff
      REAL*8 W2          ! order cutoff

      INTEGER I          ! loop index
      INTEGER IORDW      ! order index in cutoff table
      INTEGER IW1        ! start index
      INTEGER IW2        ! end index

*   Set EXTP undefined
      NOEXTP = .TRUE.

*   Wavelength step
      DWAV = GSAMP / DRDW

*   Start and end indices
      IORDW = 0

      IF ( CUTWV ) THEN
         CALL FNCUT( CORD, IORDW )
         IF ( IORDW .GT. 0 ) THEN
            W1 = CUTW1(IORDW)
            CALL IUE_TVAC(1, W1)
            W2 = CUTW2(IORDW)
            CALL IUE_TVAC(1, W2)
            IW1 = NINT( REAL((W1 - WC) / DWAV) )
            IW2 = NINT( REAL((W2 - WC) / DWAV) )
         END IF
      END IF

      IF ( IORDW .LT. 1 ) THEN
         IW1 = NINT( REAL( - WC / (DWAV * DBLE(CORD))) )
         IW2 = -IW1
      END IF

*   Contain start of grid
      DO WHILE ( .TRUE. )
         IF ( .NOT. ( IW1 .LE. IW2 ) ) GO TO 100
         W = DBLE(IW1) * DWAV + WC
         CALL WTOR( 0.0d0, W, S, L )
         DIST = SQRT((S - CENTRE(1))**2 + (L - CENTRE(2))**2)
         IF ( DIST .LE. RADIUS ) THEN
            GO TO 100

         ELSE
            IW1 = IW1 + 1
         END IF
      END DO
 100  CONTINUE

*   Contain end of grid
      DO WHILE ( .TRUE. )
         IF ( .NOT. ( IW2 .GE. IW1 ) ) GO TO 200
         W = DBLE(IW2) * DWAV + WC
         CALL WTOR( 0.0d0, W, S, L )
         DIST = SQRT((S - CENTRE(1))**2 + (L - CENTRE(2))**2)
         IF ( DIST .LE. RADIUS ) THEN
            GO TO 200

         ELSE
            IW2 = IW2 - 1
         END IF
      END DO
 200  CONTINUE

*   Check that there is a grid at all
      IF ( IW1 .GE. IW2 ) THEN
         CALL ERROUT( 'Error: order is not within image\\', STATUS )
         RETURN
      END IF

*   Form a proper grid
      NWAV = IW2 - IW1 + 1

      IF ( NWAV .GT. 1200 ) THEN
         CALL LINE_WCONT( 'Too many wavelengths, some lost.\\' )
         CALL PRTBUF( STATUS )
         NWAV = 1200

      ELSE IF ( NWAV .LT. 2 ) THEN
         NWAV = 2
      END IF

      WAV1 = DBLE(IW1) * DWAV + WC

      DO I = 1, NWAV
         WAV(I) = DBLE(I - 1) * DWAV + WAV1
      END DO

*   Form bin sizes
      CALL CAGRID( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: forming bin sizes\\', STATUS )
         RETURN
      END IF

*   Centroid and Background Folding Ranges
      BBASE = BKGAV / DRDW
      CBASE = CENAV / DRDW
      NOEXTP = .FALSE.

      END
