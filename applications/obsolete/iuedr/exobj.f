      SUBROUTINE EXOBJ( NSUB, DSUB, QSUB, RSUB, WSUB )
*+
*  Name:
*     SUBROUTINE EXOBJ

*  Description:
*     Extract object signal above background. The object spectrum above
*     mean background and its centroid shift data are obtained.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL EXOBJ( NSUB, DSUB, QSUB, RSUB, WSUB )

*  Method:
*     The pixels in the object channel are folded with a triangle
*     function which has the same base width as the wavelength
*     sampling rate.
*     This means that each pixel intensity is shared between two
*     wavelength points.
*     The error is propagated from the mean background, and from the
*     pixels themselves on the basis that their errors are
*     approximated from the neighbouring background.
*     The centroids of the object signal above background are determined
*     from the same pixels, but folding with a triangle function
*     of arbitrary range.
*     Centroid shifts are considered significant if the integrated
*     signal from which they are formed is at least CENSD Standard
*     Deviations above background.
*     Non-significant centroid values are marked and set zero.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     05-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     06-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global variables:
      INCLUDE 'CMEXTP'
      INCLUDE 'CMDISH'
      INCLUDE 'CMNET'
      INCLUDE 'CMBKG'
      INCLUDE 'CMDEV'
      INCLUDE 'CMCEN'
      INCLUDE 'CMWAV'
      INCLUDE 'CMBIN'
      INCLUDE 'CMSIST'

*  Arguments Given:
      INTEGER NSUB             ! number of subset pixels

      INTEGER*2 DSUB(NSUB)     ! DATA values

      BYTE QSUB(NSUB)          ! QUAL values

      REAL*8 RSUB(NSUB)        ! R-coordinates
      REAL*8 WSUB(NSUB)        ! W-coordinates

*  Local Variables:
      REAL*8 BK                ! pixel background
      REAL*8 CBOT(1200)        ! denomenator of centroid integrals
      REAL*8 CTOP(1200)        ! numerator of centroid integrals
      REAL*8 DBOT(1200)        ! denomenator of centroid errors
      REAL*8 DMOV(1200)        ! error "associated" with SMOV (not REAL*8 error)
      REAL*8 DTOP(1200)        ! numerator of centroid errors
      REAL*8 E2                ! square of F
      REAL*8 F                 ! pixel intensity above background
      REAL*8 GBOT
      REAL*8 GTOP
      REAL*8 R                 ! pixel R-coordinate
      REAL*8 RBASE
      REAL*8 RFWHM
      REAL*8 RMAX
      REAL*8 RMID              ! R sampling interval
      REAL*8 RMIN
      REAL*8 RP
      REAL*8 RPEAK
      REAL*8 RP2
      REAL*8 R0
      REAL*8 SCALE             ! NET scaling factor
      REAL*8 SMOV(1200)        ! individual centroid moves
      REAL*8 W                 ! pixel W-coordinate
      REAL*8 WBASE
      REAL*8 WFWHM
      REAL*8 WMID              ! W sampling interval
      REAL*8 WNET(1200)        ! accumulated pixel weights
      REAL*8 WPEAK
      REAL*8 WR                ! weight for R-profile
      REAL*8 WS                ! wavelength index locator temporary
      REAL*8 WT                ! total pixel folding weight
      REAL*8 WT2               ! square of WT
      REAL*8 WW                ! weight for W-profile

      INTEGER I                ! wavelength index
      INTEGER I1               ! lower wavelength index
      INTEGER I2               ! upper wavelength index
      INTEGER J                ! pixel index
      INTEGER J1
      INTEGER J2
      INTEGER NDCEN
      INTEGER NNET             ! NET pixels accumulator
      INTEGER NSCEN
      INTEGER Q                ! pixel Data Quality value
      INTEGER QMOV(1200)       ! data quality for SMOV

*.

*   Set NET and CEN undefined
      NONET = .TRUE.
      NOCEN = .TRUE.

*   Zero out arrays
      DO I = 1, NWAV

*      Object signal components
         SNET( I ) = 0.0
         WNET( I ) = 0.0
         DNET( I ) = 0.0
         QNET( I ) = 0

*      Centroid components
         CTOP( I ) = 0.0
         DTOP( I ) = 0.0
         CBOT( I ) = 0.0
         DBOT( I ) = 0.0
      END DO

*   Mediate between folding in R and W, give precedence to W
      IF ( GSAMP .GT. 0.7071 ) THEN
         WMID = GSAMP

      ELSE
         WMID = 0.7071
      END IF

      RMID = MAX( ( 1.0 / WMID ), ABS( ROBJ( 2 ) - ROBJ( 1 ) ) )

*   Define R-profile range and FWHM (NEW VERSION)
      RBASE = RMID + 0.7071
      RPEAK = 0.5 * RBASE / ( RBASE - RMID )
      RFWHM = 0.5 * RBASE
      R0 = ( ROBJ( 1 ) + ROBJ( 2 ) ) / 2.0
      RMIN = R0 - RFWHM
      RMAX = R0 + RFWHM

*   Define the W-profile range and FWHM (in pixels except for WFWFM(A))
      WBASE = WMID + 0.7071
      WPEAK = 0.5 * WBASE / ( WBASE - WMID )
      WFWHM = 0.5 * WBASE / DRDW

*   Go through pixel list
      NCUSE = 0
      NCBAD = 0
      NCUND = 0
      NGUSE = 0
      NGBAD = 0
      NGUND = 0

      DO J = 1, NSUB
         W = WSUB( J )
         WS = ( W - WAV1 ) / DWAV
         I = MAX( 1, MIN( NINT( REAL( WS ) ) + 1, NWAV ) )
         R = RSUB( J ) + SCEN( I )

         IF ( R.GE.RMIN .AND. R.LE.RMAX ) THEN
            CALL DQ_UTOI( QSUB( J ), Q )
            RP = R - R0
            RP2 = RP * RP

            WR = MAX( 0.0d0, MIN( 1.0d0,
     :                RPEAK * ( 1.0d0 - ABS( RP ) / RFWHM ) ) )

*         Net pixel intensity and its error (squared)
            IF ( Q .LT. QMAX ) THEN
               IF ( QBKG( I ) .EQ. 0 ) THEN
                  BK = R * GBKG( I ) + SBKG( I )
                  F = DSUB( J ) - BK
                  E2 = SDEV( I ) * SDEV( I ) + DBKG( I ) * DBKG( I )

               ELSE
                  Q = QMAX
                  NGUND = NGUND + 1
                  NCUND = NCUND + 1
               END IF

               IF ( Q .EQ. 0 ) THEN
                  NGUSE = NGUSE + 1
                  NCUSE = NCUSE + 1

               ELSE IF ( Q .LT. QMAX ) THEN
                  NGBAD = NGBAD + 1
                  NCBAD = NCBAD + 1
               END IF

            ELSE
               NGUND = NGUND + 1
               NCUND = NCUND + 1
            END IF

            I1 = MAX( IFIX( REAL( ( W - WFWHM - WAV1 + DWAV ) /
     :                            DWAV ) + 1 ), 1 )
            I2 = MIN( IFIX( REAL( ( W + WFWHM - WAV1 + DWAV ) /
     :                            DWAV ) ), NWAV )

            IF ( I1 .LE. I2 ) THEN
               DO I = I1, I2
                  IF ( Q .LT. QMAX ) THEN
                     WW = MAX( 0.0d0,
     :                    MIN( 1.0d0, WPEAK * ( 1.0d0 -
     :                         ABS( W - WAV( I ) ) / WFWHM ) ) )
                     WT = WW * WR
                     WT2 = WT * WT
                     SNET( I ) = SNET( I ) + WT * F
                     WNET( I ) = WNET( I ) + WT
                     DNET( I ) = DNET( I ) + WT2 * E2
                     QNET( I ) = MAX( QNET( I ), Q )
                     CTOP( I ) = CTOP( I ) + WT * F * RP
                     DTOP( I ) = DTOP( I ) + WT2 * E2 * RP2

                  ELSE
                     QNET( I ) = QMAX
                  END IF
               END DO
            END IF
         END IF
      END DO

      NNET = 0
      ASNET = 0.0
      ADNET = 0.0

      DO I = 1, NWAV

*      Net and its errors
         IF ( QNET( I ) .EQ. QMAX ) THEN
            SNET( I ) = 0.0
            DNET( I ) = 0.0
            QMOV( I ) = 1

         ELSE IF ( WNET( I ) .GT. 0.0 ) THEN
            SCALE = ( ROBJ( 2 ) - ROBJ( 1 ) ) / WNET( I )
            SNET( I ) = SNET( I ) * SCALE
            DNET( I ) = SQRT( DNET( I ) ) * SCALE
            CBOT( I ) = SNET( I )
            DBOT( I ) = DNET( I )
            SNET( I ) = SNET( I ) * SBIN( I )
            DNET( I ) = DNET( I ) * SBIN( I )
            ASNET = ASNET + SNET( I )
            ADNET = ADNET + DNET( I ) * DNET( I )
            NNET = NNET + 1

*         Centroid for individual bin
            IF ( CBOT( I ) .GT. CENSD * DBOT( I ) ) THEN
               SMOV( I ) = CTOP( I ) / CBOT( I )
               DMOV( I ) = DBOT( I )
               QMOV( I ) = 0

            ELSE
               QMOV( I ) = 1
            END IF

         ELSE
            QNET( I ) = QMAX
            QMOV( I ) = 1
         END IF

*      Transform Qnet from severity to standard form
         IF ( QNET( I ) .GT. 0 ) THEN
            Q = QNET( I )
            QNET( I ) = 0

            IF ( Q .EQ. QMAX ) THEN
               CALL DQ_WRPK( 1, 1, 1, QNET( I ) )

            ELSE
               CALL DQ_WRPK( 1, 2, 1, QNET( I ) )
               CALL DQ_WRPK( Q, 5, 4, QNET( I ) )
            END IF
         END IF
      END DO

*   Form mean net value
      IF ( NNET .GT. 0 ) THEN
         ASNET = ASNET / DBLE( NNET )
         ADNET = SQRT( ADNET / DBLE( NNET ) )
      END IF

*   Form mean corrections to prevailing centroid template
      GTOP = 0.0
      GBOT = 0.0

      DO I = 1, NWAV
         CTOP( I ) = 0.0
         CBOT( I ) = 0.0
         J1 = MAX( IFIX( REAL( ( WAV( I ) - CBASE - WAV1 + DWAV ) /
     :                         DWAV ) + 1 ), 1 )
         J2 = MIN( IFIX( REAL( ( WAV( I ) + CBASE - WAV1 + DWAV ) /
     :                         DWAV ) ), NWAV )

         IF ( J1 .LE. J2 ) THEN
            DO J = J1, J2
               IF ( QMOV(J) .EQ. 0 ) THEN
                  WT = MAX( 1.0d0 - ABS(WAV(J) - WAV(I))/ CBASE, 0.0d0 )
                  WT = WT / DMOV( J ) ** 2
                  CTOP( I ) = CTOP( I ) + SMOV( J ) * WT
                  CBOT( I ) = CBOT( I ) + WT
               END IF
            END DO

            GTOP = GTOP + CTOP( I )
            GBOT = GBOT + CBOT( I )
         END IF
      END DO

*   Create a global constant shift
      IF ( GBOT .GT. 0.0 ) THEN
         AGCEN = GTOP / GBOT

      ELSE
         AGCEN = 0.0
      END IF

*   Apply corrections to current template
      ASCEN = 0.0
      ADCEN = 0.0
      NSCEN = 0
      NDCEN = 0

      DO I = 1, NWAV
         IF ( CENSH ) THEN
            DCEN( I ) = AGCEN
            SCEN( I ) = SCEN( I ) - AGCEN
            QCEN( I ) = 0

         ELSE IF ( CBOT( I ) .GT. 0.0 ) THEN
            DCEN( I ) = CTOP( I ) / CBOT( I )
            SCEN( I ) = SCEN( I ) - DCEN( I )
            QCEN( I ) = 0
            NDCEN = NDCEN + 1
            ADCEN = ADCEN + DCEN( I ) * DCEN( I )

         ELSE
            DCEN( I ) = 0.0
            QCEN( I ) = 1
         END IF

         NSCEN = NSCEN + 1
         ASCEN = ASCEN + SCEN( I )
      END DO

*   Form mean centroid value
      IF ( NSCEN .GT. 0 ) ASCEN = ASCEN / DBLE( NSCEN )

*   Form mean rms variance
      IF ( NDCEN .GT. 0 ) ADCEN = ADCEN / DBLE( NDCEN )

*   Set NET and CEN defined
      NONET = .FALSE.
      NOCEN = .FALSE.

      END
