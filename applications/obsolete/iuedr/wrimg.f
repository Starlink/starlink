      SUBROUTINE WRIMG( FILE, STATUS )
*+
*  Name:
*     SUBROUTINE WRIMG

*  Purpose:
*     Read the calibration part of the dataset from the given logical
*     unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRIMG( FILE, STATUS )

*  Description:
*     The dataset is stored in 4 coupled files:
*        FILE.UEC      Contains the calibration data.
*        FILE_UED.sdf  Contains the Data Array & Data Quality
*        FILE_UES.sdf  Contains any spectrum.
*        FILE_UEM.sdf  Contains any mean spectrum.
*
*     Use sequential Fortran I/O for all files.

*  Arguments:
*     FILE = BYTE( 81 ) (Given)
*        The name of the dataset.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     10-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     26-APR-95 (MJC):
*       Modified to support DQCHAN as well as DACHAN.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMPSX'

*  Arguments Given:
      BYTE FILE( 81 )       ! Dataset name

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      REAL*8  WAVDMY         ! Dummy for write_ndf calls

      BYTE VFILE( 81 )       ! file for Calibration

      CHARACTER*( 81 ) CFILE ! Fortran 77 file name

      INTEGER FD             ! I/O unit for calibration file
      INTEGER IOSTAT         ! Fortran I/O status
      INTEGER ISTAT          ! Status from file close
      INTEGER NCHAR          ! Number of characters for GEN_STOC
      INTEGER FIOSTAT

      CHARACTER*80  DDTITLE  ! Tile of ndf file
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that there is an image.
      IF ( NOHEAD ) THEN
         CALL ERROUT( 'Error: no dataset\\', STATUS )
         GO TO 999
      END IF

*  Calibration file.
      IF ( CACHAN ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '.UEC\\', 81, VFILE )
         CALL LINE_WRITS( '%p Writing %s (Calibration File).\\', VFILE )
         CALL PRTBUF( STATUS )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

*     Fetch an I/O unit.
         CALL FIO_GUNIT( FD, STATUS )

         IF ( SYSNAME .EQ. 'VMS' ) THEN
            OPEN( UNIT=FD, NAME=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :            FORM='FORMATTED', STATUS='UNKNOWN',
     :            RECL=8192, IOSTAT=IOSTAT )

         ELSE
            OPEN( UNIT=FD, NAME=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :            FORM='FORMATTED', STATUS='UNKNOWN',
     :            IOSTAT=IOSTAT )
         END IF
         IF ( IOSTAT .NE. 0 ) THEN
            FIOSTAT = SAI__OK
            CALL FIO_PUNIT( FD, FIOSTAT )
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file create error\\', STATUS )
            GO TO 999
         END IF

         CALL WRCAL2( FD, STATUS )
         CLOSE( UNIT=FD, IOSTAT=ISTAT )
         FIOSTAT = SAI__OK
         CALL FIO_PUNIT( FD, FIOSTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', STATUS )
            GO TO 999
         END IF

         CACHAN = .FALSE.
      END IF

*  Image & Quality file (if data present and has changed).
      IF ( ( .NOT.NODATA ) .AND. ( .NOT.NOIMA ) .AND.
     :     ( DACHAN .OR. DQCHAN ) ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UED\\', 81, VFILE )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
         CALL LINE_WRITS( '%p Writing %s (Image & Quality File).\\',
     :                    VFILE )
         CALL PRTBUF( STATUS )

         DDTITLE = 'IUE image'
         CALL WRITE_NDF ('IMAGE', CFILE( :NCHAR ), 1,
     :                     WAVDMY, WAVDMY,
     :                    'NS', 'NL', DDTITLE,
     :                    1, STATUS)
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', STATUS )
            GO TO 999
         END IF

         DACHAN = .FALSE.
         DQCHAN = .FALSE.
      END IF

*  Spectrum file (if spectrum present and if changed).
      IF ( ( .NOT.NOSPEC ) .AND. ( NORDER.GT.0 ) .AND. SPCHAN ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UES\\', 81, VFILE )
         CALL LINE_WRITS( '%p Writing %s (Spectrum File).\\', VFILE )
         CALL PRTBUF( STATUS )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

         CALL WRSPC( CFILE( :NCHAR ), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', STATUS )
            GO TO 999
         END IF

         SPCHAN = .FALSE.
      END IF

*  Combined spectrum file (if present and changed).
      IF ( ( .NOT.NOCOMB ) .AND. ( NCOMB.GT.0 ) .AND. MECHAN ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UEM\\', 81, VFILE )
         CALL LINE_WRITS( '%p Writing %s (Mapped Spectrum File).\\',
     :                    VFILE )
         CALL PRTBUF( STATUS )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

         CALL WRMAP( CFILE( :NCHAR ), STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', STATUS )
            GO TO 999
         END IF

         MECHAN = .FALSE.
      END IF

*  Abort.
 999  CONTINUE

      END
