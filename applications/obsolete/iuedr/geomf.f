      SUBROUTINE GEOMF( NPRINT, STATUS )
*+
*
*   Name:
*      SUBROUTINE GEOMF
*
*   Description:
*      The fiducial positions are used to represent the geometry
*      using Chebyshev polynomials.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          04-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     30-SEP-94     IUEDR Vn. 3.1-6

*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NPRINT          ! print level

*   Export:
      INTEGER STATUS          ! status return

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFACE'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMGEOM'

*   Local variables:
      REAL*8 AXIR(2, 13*13)     ! (S,L) coordinates
      REAL*8 AXIS(2, 13*13)     ! (X,Y) coordinates
      REAL*8 D                  ! radius test
      REAL*8 DATL(13*13)        ! L-data
      REAL*8 DATS(13*13)        ! S-data
      REAL*8 DATX(13*13)        ! X-data
      REAL*8 DATY(13*13)        ! Y-data
      REAL*8 DEVL               ! L-deviation
      REAL*8 DEVS               ! S-deviation
      REAL*8 DEVX               ! X-deviation
      REAL*8 DEVY               ! Y-deviation
      REAL*8 FITL(13*13)        ! fitted L-positions
      REAL*8 FITS(13*13)        ! fitted S-positions
      REAL*8 FITX(13*13)        ! fitted X-positions
      REAL*8 FITY(13*13)        ! fitted Y-positions
      REAL*8 PEAKL              ! peak L-deviation
      REAL*8 PEAKS              ! peak S-deviation
      REAL*8 PEAKX              ! peak X-deviation
      REAL*8 PEAKY              ! peak Y-deviation
      REAL*8 SUML               ! L-residual sum
      REAL*8 SUMS               ! S-residual sum
      REAL*8 SUMX               ! X-residual sum
      REAL*8 SUMY               ! Y-residual sum

      INTEGER NFID            ! number of valid fiducials
      INTEGER I               ! loop index
      INTEGER IX              ! loop index
      INTEGER IY              ! loop index

      NOGEOM = .TRUE.
*.

* check a few things
      IF ( NOFIDS ) THEN
         CALL ERROUT( 'Error: fiducials not specified\\', STATUS )
         RETURN

      ELSE IF ( NOFACE ) THEN
         CALL ERROUT( 'Error: face-plate undefined\\', STATUS )
         RETURN

      ELSE IF ( GEOM ) THEN
         NGCHEB = 0
         IF ( NPRINT .GE. 1 ) THEN
            CALL LINE_WCONT(
     :        '%p Fiducials are in Geometric coordinate system\\'
     :                )
            CALL PRTBUF( STATUS )
         END IF
         RETURN
      END IF

*   Print something
      CALL LINE_WCONT( '%p Calibrating Geometry.\\' )
      CALL PRTBUF( STATUS )

*   Print degree
      IF ( NPRINT .GE. 1 ) THEN
         CALL LINE_WRITI(
     :           '%p Chebyshev function has (%i,\\', NGTERM(1) )
         CALL LINE_WRITI( '%i) terms.\\', NGTERM(2) )
         CALL PRTBUF( STATUS )
      END IF

      NGCHEB = NGTERM(1) * NGTERM(2)
      NFID = 0

      DO IY = 1, NFIDY
         DO IX = 1, NFIDX
            D = SQRT((FIDX(IX) - CENTRE(1))**2 + (FIDY(IY) - CENTRE(2))
     :          **2)

            IF ( D .GT. DBLE(RADIUS) ) FIDQ(IX, IY) = 2

            IF ( FIDQ(IX, IY) .EQ. 0 ) THEN
               NFID = NFID + 1
               AXIS(1, NFID) = FIDX(IX)
               AXIS(2, NFID) = FIDY(IY)
               DATS(NFID) = FIDS(IX, IY)
               DATL(NFID) = FIDL(IX, IY)
               AXIR(1, NFID) = FIDS(IX, IY)
               AXIR(2, NFID) = FIDL(IX, IY)
               DATX(NFID) = FIDX(IX)
               DATY(NFID) = FIDY(IY)
            END IF
         END DO
      END DO

*   Axis limits for function
      GAXMIN(1) = FIDX(1)
      GAXMAX(1) = FIDX(NFIDX)
      GAXMIN(2) = FIDY(1)
      GAXMAX(2) = FIDY(NFIDY)

      IF ( NPRINT .GE. 2 ) THEN
         CALL LINE_WRITI( '%p Geometry defined by %i fiducials.\\',
     :                    NFID )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p Chebyshev function limits (%f:\\',
     :        GAXMIN(1) )
         CALL LINE_WRITF( '%f,\\', GAXMAX(1) )
         CALL LINE_WRITF( '%f:\\', GAXMIN(2) )
         CALL LINE_WRITF( '%f).\\', GAXMAX(2) )
         CALL PRTBUF( STATUS )
      END IF

*   Fit S fiducials
      CALL MSC_CHEF( NFID, 2, DATS, AXIS, GAXMIN, GAXMAX, NGTERM, 36,
     :               GCHEBS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: fitting S-fiducials\\', STATUS )
         RETURN
      END IF

*   Fit L fiducials
      CALL MSC_CHEF( NFID, 2, DATL, AXIS, GAXMIN, GAXMAX, NGTERM, 36,
     :               GCHEBL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: fitting L-fiducials\\', STATUS )
         RETURN
      END IF

*   Fit X fiducials
      CALL MSC_CHEF( NFID, 2, DATX, AXIR, GAXMIN, GAXMAX, NGTERM, 36,
     :               GCHEBX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: fitting X-fiducials\\', STATUS )
         RETURN
      END IF

*   Fit Y fiducials
      CALL MSC_CHEF( NFID, 2, DATY, AXIR, GAXMIN, GAXMAX, NGTERM, 36,
     :               GCHEBY, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: fitting Y-fiducials\\', STATUS )
         RETURN
      END IF

*   Evaluate S-function values at fiducuials
      CALL MSC_CHEV( NFID, 2, AXIS, GAXMIN, GAXMAX, NGTERM, 36, GCHEBS,
     :               FITS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: evaluating S-geometry\\', STATUS )
         RETURN
      END IF

*   Evaluate L-function values at fiducuials
      CALL MSC_CHEV( NFID, 2, AXIS, GAXMIN, GAXMAX, NGTERM, 36, GCHEBL,
     :               FITL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: evaluating L-geometry\\', STATUS )
         RETURN
      END IF

*   Evaluate X-function values at fiducuials
      CALL MSC_CHEV( NFID, 2, AXIR, GAXMIN, GAXMAX, NGTERM, 36, GCHEBX,
     :               FITX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: evaluating X-geometry\\', STATUS )
         RETURN
      END IF

*   Evaluate Y-function values at fiducuials
      CALL MSC_CHEV( NFID, 2, AXIR, GAXMIN, GAXMAX, NGTERM, 36, GCHEBY,
     :               FITY, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT('Error: evaluating Y-geometry\\', STATUS )
         RETURN
      END IF

*   Print quality of representation
      IF ( NPRINT .GE. 3 ) THEN
         CALL LINE_WCONT( '%p Geometric representation:\\' )
         CALL LINE_WCONT( ' (X,Y) (S,L) and (dS,dL,dX,dY) values:\\' )
         CALL PRTBUF( STATUS )
      END IF

      SUMS = 0.0
      SUML = 0.0
      SUMX = 0.0
      SUMY = 0.0
      PEAKS = -1.0
      PEAKL = -1.0
      PEAKX = -1.0
      PEAKY = -1.0

      DO I = 1, NFID
         DEVS = DATS(I) - FITS(I)
         DEVL = DATL(I) - FITL(I)
         DEVX = DATX(I) - FITX(I)
         DEVY = DATY(I) - FITY(I)

         IF ( NPRINT .GE. 3 ) THEN
            CALL LINE_WRITF( '%p    (%6.2f,\\', AXIS(1, I) )
            CALL LINE_WRITF( '%6.2f)\\', AXIS(2, I) )
            CALL LINE_WRITF( ' (%6.2f,\\', DATS(I) )
            CALL LINE_WRITF( '%6.2f)\\', DATL(I) )
            CALL LINE_WRITF( ' (%6.2f,\\', DEVS )
            CALL LINE_WRITF( '%6.2f,\\', DEVL )
            CALL LINE_WRITF( '%6.2f,\\', DEVX )
            CALL LINE_WRITF( '%6.2f)\\', DEVY )
            CALL PRTBUF( STATUS )
         END IF

         SUMS = SUMS + DEVS * DEVS
         SUML = SUML + DEVL * DEVL
         SUMX = SUMX + DEVX * DEVX
         SUMY = SUMY + DEVY * DEVY
         PEAKS = MAX(PEAKS, ABS(DEVS))
         PEAKL = MAX(PEAKL, ABS(DEVL))
         PEAKX = MAX(PEAKX, ABS(DEVX))
         PEAKY = MAX(PEAKY, ABS(DEVY))
      END DO

      SUMS = SQRT(SUMS / DBLE(NFID))
      SUML = SQRT(SUML / DBLE(NFID))
      SUMX = SQRT(SUMX / DBLE(NFID))
      SUMY = SQRT(SUMY / DBLE(NFID))

      IF ( NPRINT .GE. 1 ) THEN
         CALL LINE_WRITF( '%p Peak residuals (%.2f,\\', PEAKS )
         CALL LINE_WRITF( '%.2f,\\', PEAKL )
         CALL LINE_WRITF( '%.2f,\\', PEAKX )
         CALL LINE_WRITF( '%.2f).\\', PEAKY )
         CALL PRTBUF( STATUS )
         CALL LINE_WRITF( '%p RMS deviation (%.2f,\\', SUMS )
         CALL LINE_WRITF( '%.2f,\\', SUML )
         CALL LINE_WRITF( '%.2f,\\', SUMX )
         CALL LINE_WRITF( '%.2f).\\', SUMY )
         CALL PRTBUF( STATUS )
      END IF

      CALL STR_MOVE( 'CHEBYSHEV\\', 16, GEOMTP )
      NOGEOM = .FALSE.
      END
