      SUBROUTINE HISLCU(NAXIS1, NAXIS2, DATA, QUAL, STATUS)

*+
*
*   Name:
*      SUBROUTINE HISLCU
*
*   Description:
*      Image cursor on HIRES image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     30-JUN-94     IUEDR Vn. 3.1-1
*        Changed to use MSG_ library, gives tidy table
*
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Import:
      INTEGER NAXIS1
      INTEGER NAXIS2
      INTEGER*2 DATA(NAXIS1, NAXIS2)
      BYTE QUAL(NAXIS1, NAXIS2)

*   Export:
      INTEGER STATUS      ! status return

*   External references:
      INTEGER DQ_AND      ! DQ and

*   Starlink includes:
      INCLUDE 'SAE_PAR'
      INCLUDE 'MSG_PAR'

*   Global variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMDATA'
      INCLUDE 'CMHEAD'

*   Local constants:
      LOGICAL TRUE       ! .TRUE.
      PARAMETER (TRUE = .TRUE.)

      INTEGER HIGHORD    ! highest order
      INTEGER HIRES      ! HIRES index
      INTEGER LOWORD     ! lowest order
      PARAMETER (HIGHORD = 125, HIRES = 2, LOWORD = 65)

*   Local variables:
      REAL*8 FN            ! associated flux number
      REAL*8 R             !
      REAL*8 RMIN          ! minimum acceptable R
      REAL*8 W             ! wavelength
      REAL*8 DX
      REAL*8 DY
      REAL   X             ! X cursor hit position
      REAL   Y             ! Y cursor hit position

      CHARACTER*80 AMESSAGE
      CHARACTER*40 CXLAB
      CHARACTER*40 CXUN
      CHARACTER*40 CYLAB
      CHARACTER*40 CYUN

      BYTE ASTRING(80)

      INTEGER CURMOD     ! cursor mode (0 for GKS default)
      INTEGER IAPER      ! aperture
      INTEGER IQ         ! data quality flag
      INTEGER IX         ! X pixel
      INTEGER IY         ! Y pixel
      INTEGER KEY        ! hit key
      INTEGER ORD        ! order
      INTEGER ORDMIN     ! nearest order
      INTEGER NCHAR

*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Aperture/resolution
      CALL DEFAPR( HIRES, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                STATUS )
      ELSE

*      Define template
         CALL HITEM( IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: setting up dispersion relations\\',
     :                   STATUS )

         ELSE

*         Axis labels and units
            CALL GEN_STOC( XLAB, 40, CXLAB, NCHAR )
            CALL GEN_STOC( XUN, 40, CXUN, NCHAR )
            CALL GEN_STOC( YLAB, 40, CYLAB, NCHAR )
            CALL GEN_STOC( YUN, 40, CYUN, NCHAR )
            CALL MSG_SETC( 'XLAB', CXLAB )
            CALL MSG_SETC( 'XLAB', CXUN )
            CALL MSG_SETC( 'YLAB', CYLAB)
            CALL MSG_SETC( 'YLAB', CYUN )
            CALL MSG_FMTC( 'OLINE', '(A15)', 'Inten.(FN)' )
            CALL MSG_FMTC( 'OLINE', '(A11)', 'R(PIXEL)' )
            CALL MSG_FMTC( 'OLINE', '(A12)', 'Wave(A)' )
            CALL MSG_FMTC( 'OLINE', '(A12)', 'Order' )
            CALL MSG_LOAD( ' ', ' ^XLAB    ^YLAB^OLINE', AMESSAGE,
     :                     NCHAR, STATUS )
            CALL GEN_CTOS( AMESSAGE, 80, ASTRING, NCHAR )
            CALL LINE_WRITS( '%p%s\\', ASTRING )
            CALL PRTBUF( STATUS )

*         Cursor loop
            DO WHILE (.TRUE.)
               CALL GRF_CUZONE( '12', CURMOD, KEY, X, Y, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERROUT( 'Error: reading cursor\\', STATUS )
                  GO TO 100

               ELSE IF ( KEY .LE. 0 ) THEN
                  GO TO 100
               END IF

*            Find nearest order (within reason)
               RMIN = 1.0E+06
               ORDMIN = LOWORD - 1

               DO ORD = LOWORD, HIGHORD
                  CALL ORSET( ORD )
                  CALL RTOW( DBLE(X), DBLE(Y), R, W )
                  IF ( ABS(R) .LT. ABS(RMIN) ) THEN
                     ORDMIN = ORD
                     RMIN = R
                  END IF
               END DO

               CALL ORSET( ORDMIN )
               CALL RTOW( DBLE(X), DBLE(Y), R, W )
               CALL IUE_TAIR( 1, W )
               IX = NINT(REAL(X))
               IY = NINT(REAL(Y))

               IF ( IX.GE.1 .AND. IX.LE.NAXIS1 .AND. IY.GE.1
     :              .AND. IY.LE.NAXIS2 ) THEN
                  DX = DBLE(X)
                  DY = DBLE(Y)
                  CALL MSG_FMTD( 'DLINE', 'F7.3', DX )
                  CALL MSG_FMTD( 'DLINE', 'F12.3', DY )

                  IF ( DATA(IX, IY) .NE. DBLANK ) THEN
                     CALL DQ_UTOI( QUAL(IX, IY), IQ )
                     IF ( DQ_AND(IQ, 1) .EQ. 0 ) THEN
                        FN = DBLE(DATA(IX, IY)) * DSCALE + DZERO
                        CALL MSG_FMTD( 'DLINE', 'G15.4', FN )

                     ELSE
                        CALL MSG_FMTC( 'DLINE', 'A11', '-' )
                     END IF

                  ELSE
                     CALL MSG_FMTC( 'DLINE', 'A11', '-' )
                  END IF

                  CALL MSG_FMTD( 'DLINE', 'F15.5', R )
                  CALL MSG_FMTD( 'DLINE', 'F13.2', W )
                  CALL MSG_FMTI( 'DLINE', 'I10', ORDMIN )
                  CALL MSG_LOAD( ' ', ' ^DLINE', AMESSAGE, NCHAR,
     :                           STATUS )
                  CALL GEN_CTOS( AMESSAGE, 80, ASTRING, NCHAR )
                  CALL LINE_WRITS( '%p%s\\', ASTRING )
                  CALL PRTBUF( STATUS )
               END IF
            END DO
 100        CONTINUE
         END IF
      END IF
      END
