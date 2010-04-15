      SUBROUTINE OUTSPC( FD, FILE, TYPE, NPOINT, X, Y, LABEL1, LABEL2,
     :                   STATUS )
*+
*  Name:
*     SUBROUTINE OUTSPC

*  Purpose:
*     Create a SPECTRUM format file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OUTSPC( FD, FILE, TYPE, NPOINT, X, Y, LABEL1, LABEL2,
*    :             STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        I/O unit number to open file on.
*     FILE = BYTE( * ) (Given)
*        Name of the file to be written.
*     TYPE = INTEGER (Given)
*        Which SPECTRUM format is to be used.
*     NPOINT = INTEGER (Given)
*        Number of points to be written.
*     X = REAL*8( * ) (Given)
*        X-value array.
*     Y = REAL*8( * ) (Given)
*        Y-value array.
*     LABEL1 = BYTE( * ) (Given)
*        First line labelling text.
*     LABEL2 = BYTE( * ) (Given)
*        Second line labelling text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     A "SPECTRUM" file is created and filled with the supplied spectrum
*     and associated labels.
*     Files with SPECTRUM types 0 (Binary), 1 (fixed format text) and
*     2 (free format text) can be written.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     DMILLS: Dave Mills (UCL)
*     MJC: Martin Clayton (UCL)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     07-MAY-83 (JRG):
*       IUEDR Vn. 1.0
*     04-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     ??-???-?? (DMILLS):
*       IUEDR Vn. 3.0
*     03-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     26-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Modified NDF title string to include CLAB1 data as well as CLAB2.
*     09-JUL-99 (MBT):
*       Removed CARRIAGECONTROL keywords in OPEN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'CHR_ERR'

*  External References:
      INTEGER CHR_LEN

*  Arguments Given:
      INTEGER FD           ! File unit.

      BYTE FILE( * )       ! File name.

      INTEGER TYPE         ! SPECTRUM type.
      INTEGER NPOINT       ! Number of points.

      REAL*8 X( * )        ! X-values.
      REAL*8 Y( * )        ! Y-values (zero => blank).

      BYTE LABEL1( * )     ! Text label 1.
      BYTE LABEL2( * )     ! Text label 2.

*  Status:
      INTEGER STATUS       ! Global status.

*  Local Variables:
      CHARACTER*81 CFILE   ! Fortran 77 filename.
      CHARACTER*79 CLAB1   ! Label 1.
      CHARACTER*79 CLAB2   ! Label 2.
      CHARACTER*80 CTITLE  ! Title for NDF dataset.

      INTEGER CLEN         ! Character string length.
      INTEGER IPOSN
      INTEGER I            ! Loop index.
      INTEGER IOSTAT       ! Internal status flag.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert file name and labels to CHARACTER strings.
      CLAB1 = ' '
      CALL GEN_STOC( LABEL1, 79, CLAB1, CLEN )
      CLAB2 = ' '
      CALL GEN_STOC( LABEL2, 79, CLAB2, CLEN )
      CFILE = ' '
      CALL GEN_STOC( FILE, 81, CFILE, CLEN )

*  Branch on TYPE.
      IF ( TYPE .EQ. 0 ) THEN

*     Print information.
         CALL LINE_WRITS( '%p Writing NDF SPECTRUM File (%s).\\',
     :                    FILE )
         CALL PRTBUF( STATUS )

*     Generate dataset title.
         CTITLE = 'IUEDR output spectrum: '
         CALL CHR_COPY( CLAB1, .TRUE., CTITLE, STATUS )
         IPOSN = CHR_LEN( CTITLE )
         CALL CHR_APPND( CLAB2, CTITLE, IPOSN )

*     Write out to NDF.
         CALL WRITE_NDF( 'SP0WR', CFILE( : CLEN ), NPOINT,
     :                   X, Y, 'Wavelength', 'Flux',
     :                   CTITLE, 1, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( FILE )
            CALL ERROUT( ': NDF write error\\', STATUS )
            GO TO 999
         END IF

      ELSE IF ( TYPE .EQ. 1 ) THEN

*     Open file.
         OPEN ( UNIT = FD, NAME = CFILE( :CLEN ), ACCESS = 'SEQUENTIAL',
     :          FORM = 'FORMATTED', STATUS = 'UNKNOWN',
     :          IOSTAT = IOSTAT )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERRSTR( FILE )
            CALL ERROUT( ': file open error\\', STATUS )
            GO TO 999
         END IF

*     Print information.
         CALL LINE_WRITS(
     :        '%p Writing Fixed Format SPECTRUM File (%s).\\',
     :        FILE )
         CALL PRTBUF( STATUS )

*     Write 1st line.
         WRITE ( FD, '( A )', IOSTAT = IOSTAT ) CLAB1
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing 1st text line\\', STATUS )
            GO TO 100
         END IF

*     Write 2nd character line.
         WRITE ( FD, '( A )', IOSTAT = IOSTAT ) CLAB2
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing 2nd text line\\', STATUS )
            GO TO 100
         END IF

*     Write number of points.
         WRITE ( FD, '( 20X, I6 )', IOSTAT = IOSTAT ) NPOINT
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing number of points\\', STATUS )
            GO TO 100
         END IF

*     Write spectrum itself.
         WRITE ( FD, '( 4( 0PF8.3, 1PE10.3 ) )', IOSTAT = IOSTAT )
     :           ( X( I ), Y( I ), I = 1, NPOINT )
         IF ( IOSTAT .NE. 0 )
     :      CALL ERROUT( 'Error: writing spectrum arrays\\', STATUS )

      ELSE IF ( TYPE .EQ. 2 ) THEN

*     Open file.
         OPEN ( UNIT = FD, NAME = CFILE( :CLEN ), ACCESS = 'SEQUENTIAL',
     :          FORM = 'FORMATTED', STATUS = 'UNKNOWN',
     :          IOSTAT = IOSTAT )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERRSTR( FILE )
            CALL ERROUT( ': file open error\\', STATUS )
            GO TO 999
         END IF

*     Print information.
         CALL LINE_WRITS(
     :          '%p Writing Free Format SPECTRUM File (%s).\\', FILE )
         CALL PRTBUF( STATUS )

*     Write 1st line.
         WRITE ( FD, '( A )', IOSTAT = IOSTAT ) CLAB1
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing 1st text line\\', STATUS )
            GO TO 100
         END IF

*     Write 2nd character line.
         WRITE ( FD, '( A )', IOSTAT = IOSTAT ) CLAB2
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing 2nd text line\\', STATUS )
            GO TO 100
         END IF

*     Write number of points.
         WRITE ( FD, *, IOSTAT = IOSTAT ) NPOINT
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing number of points\\', STATUS )
            GO TO 100
         END IF

*     Write spectrum itself.
         DO I = 1, NPOINT
            WRITE ( FD, *, IOSTAT = IOSTAT ) X( I ), Y( I )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing spectrum arrays\\',
     :                      STATUS )
               GO TO 100
            END IF
         END DO
      END IF

*  Close file (if opened).
 100  CONTINUE
      CLOSE ( UNIT = FD, IOSTAT = IOSTAT )
      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file close error\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
