      SUBROUTINE EXSUBS( NS, NL, DATA, QUAL, LMIN, LMAX, SMIN, SMAX,
     :                   SL_RW, MSUB, DSUB, QSUB, RSUB, WSUB, NSUB,
     :                   DBLANK, STATUS )

*+
*
*   Name:
*      SUBROUTINE EXSUBS
*
*   Description:
*      Extract subset from image. Extract a list of pixels forming an
*      image subset. The Data Quality is converted to a Data Severity
*      in the context of IUE.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          08-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     08-OCT-94     IUEDR Vn. 3.1-6
*
*   Method:
*      The transformation of data quality is done in a complex way which
*      may speed things up (e.g. by doing dblank and zero as special cases).
*      The assumption is made that the severity code never overflows the
*      BYTE type (-128 to 127) range.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER NS                 ! size of axis 1 (sample)
      INTEGER NL                 ! size of axis 2 (line)

      INTEGER*2 DATA(NS, NL)     ! image

      BYTE QUAL(NS, NL)          ! quality

      INTEGER LMIN               ! lowest LINE
      INTEGER LMAX               ! highest LINE
      INTEGER SMIN(NL)           ! start SAMPLE for each LINE
      INTEGER SMAX(NL)           ! end SAMPLE for each LINE
      INTEGER DBLANK             ! blank data value
      INTEGER MSUB               ! maximum number of pixels

      external sl_rw ! (S,L) to (R,W) transform

*   Export:
      INTEGER*2 DSUB(MSUB)       ! DATA values

      BYTE QSUB(MSUB)            ! QUAL values

      REAL*8 RSUB(MSUB)          ! R-coordinates
      REAL*8 WSUB(MSUB)          ! W-coordinates

      INTEGER NSUB               ! actual size of list
      INTEGER STATUS             ! status return

*   Global variables:
      INCLUDE 'CMBUG'
      INCLUDE 'CMEXTP'

*   Local variables:
      INTEGER IL                 ! loop index
      INTEGER IS                 ! loop index
      INTEGER Q                  ! temporary data quality value
      INTEGER QBITS(8)           ! full data quality bit array
      INTEGER QSEV               ! temporary data severity value

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Severity values for SPK and MAX
      QSPK = 3
      QMAX = 8

*   Go through image
      NSUB = 0

      DO IL = LMIN, LMAX
         DO IS = SMIN(IL), SMAX(IL)
            IF ( NSUB .LT. MSUB ) THEN
               NSUB = NSUB + 1
               DSUB(NSUB) = DATA(IS, IL)

               IF ( DSUB(NSUB) .EQ. DBLANK ) THEN
                  QSEV = QMAX

               ELSE IF ( QUAL(IS, IL) .EQ. 0 ) THEN
                  QSEV = 0

               ELSE
                  CALL DQ_UTOI( QUAL(IS, IL), Q )
                  CALL DQ_UNPK( Q, 8, QBITS )

                  IF ( QBITS(1) .EQ. 1 ) THEN
                     QSEV = QMAX

                  ELSE IF ( QBITS(7) .EQ. 1 ) THEN
                     QSEV = 7

                  ELSE IF ( QBITS(6) .EQ. 1 ) THEN
                     QSEV = 6

                  ELSE IF ( QBITS(8) .EQ. 1 ) THEN
                     QSEV = 5

                  ELSE IF ( QBITS(2) .EQ. 1 ) THEN
                     QSEV = 4

                  ELSE
                     QSEV = 0
                  END IF
               END IF

               QSUB(NSUB) = QSEV

               CALL SL_RW( DBLE(IS), DBLE(IL), RSUB(NSUB), WSUB(NSUB) )

            ELSE
               CALL ERROUT( 'Error: image subset exceeds memory\\',
     :                      STATUS )
               RETURN
            END IF
         END DO
      END DO

      END
