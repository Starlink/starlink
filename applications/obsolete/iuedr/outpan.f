      SUBROUTINE OUTPAN( FD, FILE, TYPE, MAXU, MAXV, FPAN, QPAN, LABEL1,
     :                   LABEL2, STATUS )
*+
*  Name:
*     SUBROUTINE OUTPAN

*  Purpose:
*     Output LBLS in SPECTRUM-like file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL OUTPAN( FD, FILE, TYPE, MAXU, MAXV, FPAN, QPAN, LABEL1,
*    :             LABEL2, STATUS )

*  Arguments:
*     FD = INTEGER (Given)
*        Unit number of file to be written to.
*     FILE = BYTE( * ) (Given)
*        Name of file to be written.
*     TYPE = INTEGER (Given)
*        SPECTRUM file type number.
*     MAXU = INTGER (Given)
*        U size of LBLS array.
*     MAXV = INTGER (Given)
*        V size of LBLS array.
*     FPAN = REAL*8( MAXU, MAXV ) (Given)
*        Array of flux values.
*     QPAN = BYTE( MAXU, MAXV ) (Given)
*        Array of quality flag values.
*     LABEL1 = BYTE( * ) (Given)
*        First label for file.
*     LABEL2 = BYTE( * ) (Given)
*        Second label for file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     A SPECTRUM-like file is created containing the LBLS array.
*     This is only "like" since SPECTRUM does not entertain 2-D data.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     07-MAY-83 (JRG):
*       IUEDR Vn. 1.0
*     05-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     20-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Problems:
*     At present, TYPE=0 is the only allowed mode.

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMLBLS'

*  Local Constants:
      INTEGER MAXLABEL       ! Maximum length of label.
      PARAMETER ( MAXLABEL = 40 )

*  Arguments Given:
      INTEGER FD                ! File unit.

      BYTE FILE( * )            ! File name.

      INTEGER TYPE              ! SPECTRUM type.
      INTEGER MAXU              ! U-size.
      INTEGER MAXV              ! V-size.

      REAL*8 FPAN( MAXU, MAXV ) ! Fluxes.

      BYTE QPAN( MAXU, MAXV )   ! Data quality.
      BYTE LABEL1( * )          ! Text label 1.
      BYTE LABEL2( * )          ! Text label 2.

*  Status:
      INTEGER STATUS            ! Global status.

*  Local Variables:
      CHARACTER*81 CFILE        ! Fortran 77 filename.
      CHARACTER*79 CLAB         ! Label 1.

      INTEGER CLEN              ! Character string length.
      INTEGER IOSTAT            ! Local status.
      INTEGER IU                ! loop index.
      INTEGER IV                ! Loop index.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert file name and labels to CHARACTER strings
*  Branch on TYPE.
      IF ( TYPE .EQ. 0 ) THEN

*     Open file.
         CFILE = ' '
         CALL GEN_STOC( FILE, 81, CFILE, CLEN )
         OPEN( UNIT = FD, NAME = CFILE( :CLEN ), ACCESS = 'SEQUENTIAL',
     :         FORM = 'UNFORMATTED', STATUS = 'UNKNOWN',
     :         IOSTAT = IOSTAT )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERRSTR( FILE )
            CALL ERROUT( ': file open error\\', STATUS )
            GO TO 999
         END IF

*     Print information.
         CALL LINE_WRITS( '%p Writing Binary LBLS File (%s).\\', FILE )
         CALL PRTBUF( STATUS )

*     Write 1st line (TITLE).
         CLAB = ' '
         CALL GEN_STOC( LABEL1, 79, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing 1st text line\\', STATUS )
            GO TO 100
         END IF

*     Write 2nd character line (OBJECT).
         CLAB = ' '
         CALL GEN_STOC( LABEL2, 79, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing 2nd text line\\', STATUS )
            GO TO 100
         END IF

*     Write U-label.
         CALL GEN_STOC( ULAB, MAXLABEL, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing u-axis label\\', STATUS )
            GO TO 100
         END IF

*     Write U-units.
         CALL GEN_STOC( UUNT, MAXLABEL, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing u-axis units\\', STATUS )
            GO TO 100
         END IF

*     Write U-axis data.
         WRITE( FD, IOSTAT = IOSTAT ) NU, ( US( IU ), IU = 1, NU )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing u-axis data\\', STATUS )
            GO TO 100
         END IF

*     Write V-label.
         CALL GEN_STOC( VLAB, MAXLABEL, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing v-axis label\\', STATUS )
            GO TO 100
         END IF

*     Write V-units.
         CALL GEN_STOC( VUNT, MAXLABEL, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing v-axis units\\', STATUS )
            GO TO 100
         END IF

*     Write V-axis data.
         WRITE( FD, IOSTAT = IOSTAT ) NV, ( VS( IV ), IV = 1, NV )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing v-axis data data\\', STATUS )
            GO TO 100
         END IF

*     Write D-label.
         CALL GEN_STOC( DLAB, MAXLABEL, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing data label\\', STATUS )
            GO TO 100
         END IF

*     Write D-units.
         CALL GEN_STOC( DUNT, MAXLABEL, CLAB, CLEN )
         WRITE( FD, IOSTAT = IOSTAT ) CLAB
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERROUT( 'Error: writing data units\\', STATUS )
            GO TO 100
         END IF

*     Write F and Q arrays, one line per record.
         DO IV = 1, NV
            WRITE( FD, IOSTAT = IOSTAT )
     :             ( FPAN( IU, IV ), IU = 1, NU ),
     :             ( QPAN( IU, IV ), IU = 1, NU )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERROUT( 'Error: writing LBLS array\\', STATUS )
               GO TO 100
            END IF
         END DO

      ELSE
         CALL ERROUT( 'Error: unknown file type\\', STATUS )
         GO TO 999
      END IF

 100  CONTINUE

*  Close file (if opened).
      CLOSE( UNIT = FD, IOSTAT = IOSTAT )
      IF ( IOSTAT .NE. 0 ) THEN
         CALL ERRSTR( FILE )
         CALL ERROUT( ': file close error\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
