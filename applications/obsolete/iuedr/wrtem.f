      SUBROUTINE WRTEM( FD, FILE, STATUS )
*+
*   Name:
*      SUBROUTINE WRTEM
*
*   Description:
*      Write template data to calibration file.
*
*   History:
*      Jack Giddings      07-MAY-83     IUEDR Vn. 1.0
*      Paul Rees          05-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     17-AUG-94     IUEDR Vn. 3.1-2
*      Martin Clayton     30-SEP-94     IUEDR Vn. 3.1-6
*      Mark Taylor        09-JUL-99     Standardized OPEN statement.
*
*   Method:
*      A text file is written containing the template data present in
*      CMTEM.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import:
      INTEGER FD               ! file unit

      BYTE FILE(*)             ! file name

*   Export:
      INTEGER STATUS           ! status return

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMTEM'

*   Local variables:
      BYTE LINE(79)            ! SWT line string

      CHARACTER*81 CFILE       ! Fortran 77 filename
      CHARACTER*(79) CLINE     ! CHR line string

      INTEGER CLEN             ! character string length
      INTEGER I                ! loop index
      INTEGER IRES             ! resolution index
      INTEGER ISTAT            ! status
      INTEGER J                ! loop index
      INTEGER POS              ! character position
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Convert file name and labels to CHARACTER strings.
      CFILE = ' '
      CALL GEN_STOC( FILE, 81, CFILE, CLEN )

*   Output to file.
      OPEN ( UNIT = FD, NAME = CFILE( :CLEN ), ACCESS = 'SEQUENTIAL',
     :       FORM = 'FORMATTED', STATUS = 'UNKNOWN', IOSTAT = STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file open error\\', STATUS )
         GO TO 999
      END IF

*   Print information.
      CALL LINE_WRITS(
     :       '%p Writing Formatted Template Data File (%s).\\', FILE )
      CALL PRTBUF( STATUS )

*   Find resolution index.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         GO TO 999
      END IF

*   Type (HIRES/LORES).
      POS = 0
      CALL STR_WRITS( '''%s''\\', RESOL, 79, LINE, POS )
      CALL GEN_STOC( LINE, 79, CLINE, CLEN )
      WRITE ( FD, '( A )', IOSTAT = STATUS ) CLINE( : CLEN )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: dataset type\\', STATUS )
         GO TO 100
      END IF

*   Number of orders/apertures.
      WRITE ( FD, '( I3 )', IOSTAT = STATUS) NTEMO
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: number of templates\\', STATUS )

      ELSE IF ( NTEMO .GT. 0 ) THEN
         DO I = 1, NTEMO
            IF ( IRES .EQ. 1 ) THEN
               POS = 0
               CALL STR_WRITS(
     :              '''%s''\\', APERS( 1, TEMORD( I ) ), 79, LINE, POS )
               CALL GEN_STOC( LINE, 79, CLINE, CLEN )
               WRITE ( FD, '( 1X, A, 1X, I3, 2( 2X, F8.3 ) )',
     :                 IOSTAT = STATUS )
     :                 CLINE( :CLEN), NTEMS( I ), TEMW0( I ), TEMDW( I )

            ELSE IF ( IRES .EQ. 2 ) THEN
               WRITE ( FD, '( 1X, I3, 1X, I3, 2( 2X, F8.3 ) )',
     :                 IOSTAT = STATUS )
     :                 TEMORD( I ), NTEMS( I ), TEMW0( I ), TEMDW( I )

            END IF
            IF ( STATUS .NE. SAI__OK ) GO TO 100
            WRITE ( FD, '( 10( 1X, F6.2 ) )', IOSTAT = STATUS )
     :              ( TEMCEN( J, I ), J = 1, NTEMS( I ) )
            IF ( STATUS .NE. SAI__OK ) GO TO 100
         END DO
      END IF

 100  CONTINUE

*   Close file (if opened).
      CLOSE ( UNIT = FD, IOSTAT = ISTAT )
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file close error\\', STATUS )
      END IF

 999  CONTINUE

      END
