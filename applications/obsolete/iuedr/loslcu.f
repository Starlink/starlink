      SUBROUTINE LOSLCU( NAXIS1, NAXIS2, DATA, QUAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE LOSLCU
*
*   Description:
*      Image cursor on LORES image.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-SEP-88     IUEDR Vn. 2.0
*      Martin Clayton     05-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NAXIS1                   ! X-axis size of image
      INTEGER NAXIS2                   ! Y-axis size of image
      INTEGER*2 DATA(NAXIS1, NAXIS2)   ! image data array
      BYTE QUAL(NAXIS1, NAXIS2)        ! image quality array

*   Export:
      INTEGER STATUS      ! status return

*   External references:
      INTEGER DQ_AND      ! DQ and

*   Gloabl variables:
      INCLUDE 'CMGRAF'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'

*   Local constants:
      INTEGER CURMOD     ! GKS cursor mode (0 default)
      PARAMETER (CURMOD = 0)

*   Local variables:
      REAL*8 FN          ! associated flux number
      REAL*8 R           !
      REAL*8 RMIN        ! minimum acceptable R
      REAL*8 W           ! wavelength
      REAL*8 DX
      REAL*8 DY
      REAL   X           ! X cursor hit position
      REAL   Y           ! Y cursor hit position

      INTEGER I          ! do loop counter
      INTEGER IAPER      ! aperture
      INTEGER IQ         ! data quality flag
      INTEGER IX         ! X pixel
      INTEGER IY         ! Y pixel
      INTEGER KEY        ! hit key

*.

*   Axis labels and units
      CALL LINE_WRITS( '%p %s\\', XLAB )
      CALL LINE_WRITS( '%s\\', XUN )
      CALL LINE_WRITS( '%13p %s\\', YLAB )
      CALL LINE_WRITS( '%s\\', YUN )
      CALL LINE_WRITS( '%26p %s\\', 'Inten.\\' )
      CALL LINE_WRITS( '%s\\', '(FN)\\' )
      CALL LINE_WRITS( '%39p %s\\', 'R\\' )
      CALL LINE_WRITS( '%s\\', '(PIXEL)\\' )
      CALL LINE_WRITS( '%52p %s\\', 'Wave' )
      CALL LINE_WRITS( '%s\\', '(A)\\' )
      CALL LINE_WRITS( '%65p %s\\', 'Aperture\\' )
      CALL PRTBUF( STATUS )

*   Cursor loop
      DO WHILE ( .TRUE. )
         CALL GRF_CUZONE( '12', CURMOD, KEY, X, Y, STATUS )
         IF ( STATUS .NE. SAI__OK) THEN
            CALL ERROUT( 'Error: reading cursor\\', STATUS )
            GO TO 100

         ELSE IF ( KEY .LE. 0 ) THEN
            GO TO 100

         ELSE

*         Find nearest aperture (within reason)
            RMIN = 1.0E+06
            IAPER = 0

            DO I = 1, NAPER
               CALL LOTEM( I, STATUS )
               CALL RTOW( DBLE(X), DBLE(Y), R, W )
               IF ( ABS(R) .LT. ABS(RMIN) ) THEN
                  IAPER = I
                  RMIN = R
               END IF
            END DO

            CALL LOTEM( IAPER, STATUS )
            CALL RTOW( DBLE(X), DBLE(Y), R, W )
            CALL IUE_TAIR( 1, W )
            IX = NINT(REAL(X))
            IY = NINT(REAL(Y))
            DX = DBLE(X)
            DY = DBLE(Y)
            CALL LINE_WRITF( '%p %g\\', DX )
            CALL LINE_WRITF( '%13p %g\\', DY )

            IF ( IX.GE.1 .AND. IX.LE.NAXIS1 .AND.
     :           IY.GE.1 .AND. IY.LE.NAXIS2 ) THEN
               IF ( DATA(IX, IY) .NE. DBLANK ) THEN
                  CALL DQ_UTOI( QUAL(IX, IY), IQ )
                  IF ( DQ_AND(IQ, 1) .EQ. 0 ) THEN
                     FN = DBLE(DATA(IX, IY)) * DSCALE + DZERO
                     CALL LINE_WRITF( '%26p %g\\', FN )
                  END IF
               END IF
            END IF

            CALL LINE_WRITF( '%39p %g\\', R )
            CALL LINE_WRITF( '%52p %g\\', W )
            CALL LINE_WRITS( '%65p %s\\', APERS(1, IAPER) )
            CALL PRTBUF( STATUS )
         END IF
      END DO

 100  CONTINUE
      END
