      SUBROUTINE RDIMG( FILE, STATUS )
*+
*  Name:
*     SUBROUTINE RDIMG

*  Purpose:
*     Read the specified IUE dataset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDIMG( FILE, STATUS )

*  Description:
*     The Dataset is stored in 4 coupled files:
*        FILE.UEC      Contains the calibration data.
*        FILE_UED.sdf  Contains the Data Array and Data Quality.
*        FILE_UES.sdf  Contains any spectrum.
*        FILE_UEM.sdf  Contains any mean spectrum.
*
*     Use Fortran I/O for all files.
*     Whether an attempt is made to read each file is determined by the
*     switches in CMNEED.
*     Whether a failed read is fatal is determined by the switches in
*     CMMUST.

*  Arguments:
*     FILE = BYTE( 81 ) (Given)
*        Dataset name.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     MBT: Mark Taylor (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     22-SEP-88 (PCTR):
*       IUEDR Vn. 2.0
*     10-OCT-94 (MJC):
*       IUEDR Vn. 3.1-6
*     09-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     09-JUL-99 (MBT):
*       Removed nonstandard OPEN keywords.
*     {enter_further_changes_here}

*  Bugs:
*     The calls to DIOINT were needed since any DAREAD invocations in
*     some weird way I don't unterstand ... the over-write problem may
*     remain after DIO removal!
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMNEED'
      INCLUDE 'CMMUST'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMPSX'

*  Arguments Given:
      BYTE FILE(81)         ! Dataset name

*  Status:
      INTEGER STATUS        ! Global status.

*  Local Variables:
      BYTE VFILE(81)         ! File for Calibration.

      CHARACTER*(81) CFILE   ! Fortran 77 file name.
      CHARACTER*(11) FFORM   ! Fortran I/O FORM.
      CHARACTER*(10) FACC    ! Fortran I/O access mode.

      INTEGER FD             ! I/O unit number.
      INTEGER IOSTAT         ! Fortran I/O status.
      INTEGER ISTAT          ! Status from file close.
      INTEGER NCHAR          ! Number of characters for GEN_STOC.
      INTEGER FIOSTAT        ! status for FIO_PUNIT calls.
      INTEGER NNN

      INTEGER*2 WDUMMY       ! Dummy for non-IMAGE read_ndfs.

      BYTE BDUMMY            ! Dummy for non-IMAGE read_ndfs.

      LOGICAL GOTQUAL

      LOGICAL OLDDATA
      COMMON / CMVERSION / OLDDATA
*.

*  Don't pass error status to FIO_PUNIT as this makes ems go awry.
      FIOSTAT = SAI__OK

*  Calibration file.
      IF ( CANEED ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '.UEC\\', 81, VFILE )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
         CALL LINE_WRITS( '%p Reading %s (Calibration File).\\',
     :                    VFILE )
         CALL PRTBUF( STATUS )

*     Fetch an I/O unit number.
         CALL FIO_GUNIT( FD, STATUS )

*     Inquire the ACCESS mode and FORM of the file.
C         IF ( SYSNAME .EQ. 'VMS' ) THEN
C            OPEN( UNIT=FD, FILE=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
C     :            READONLY, FORM='FORMATTED', STATUS='OLD',
C     :            CARRIAGECONTROL='NONE' , IOSTAT=IOSTAT )
C
C         ELSE
            OPEN( UNIT=FD, FILE=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :            FORM='FORMATTED', STATUS='OLD', IOSTAT=IOSTAT )
C         END IF
         IF ( IOSTAT .NE. SAI__OK ) THEN
            FACC = 'UNKNOWN'
            FFORM = 'UNKNOWN'
         END IF

*     Act on returned FORM and ACCESS values.
         IF ( IOSTAT .EQ. SAI__OK ) THEN

*        Assume the file is in IUEDR Vn. 3.* format.
            OLDDATA = .FALSE.
            CALL RDCAL2( FD, STATUS )
            CLOSE( UNIT=FD, IOSTAT=ISTAT )

*        Check if RDCAL2 was successful: if not, then the calibration file
*        may be in IUEDR Vn. 1.*/2.* format. Try this.
            IF ( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__OK
               OPEN( UNIT=FD, FILE=CFILE( :NCHAR ),
     :               ACCESS='SEQUENTIAL', FORM='UNFORMATTED',
     :               STATUS='OLD', IOSTAT=IOSTAT )
               IF ( IOSTAT .NE. SAI__OK ) THEN
                  CALL ERRSTR( VFILE )
                  CALL ERROUT( ': file open error\\', STATUS )
                  GO TO 998
               END IF

               CALL LINE_WRITS(
     :               '%p Re-reading %s (Calibration File).\\', VFILE )
               CALL PRTBUF( STATUS )
               OLDDATA = .TRUE.
               CALL RDCAL( FD, STATUS )
               CLOSE( UNIT=FD, IOSTAT=ISTAT )
            END IF

         ELSE

*        Assume the file is in IUEDR Vn. 1.*/2.* format.
            OPEN( UNIT=FD, FILE=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :            FORM='UNFORMATTED', STATUS='OLD', IOSTAT=IOSTAT )
            IF ( IOSTAT .NE. SAI__OK ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': file open error\\', STATUS )
               GO TO 998
            END IF

            OLDDATA = .TRUE.
            CALL RDCAL( FD, STATUS )
            CLOSE( UNIT=FD, IOSTAT=ISTAT )
         END IF

         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', STATUS )
            GO TO 998

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file read error\\', STATUS )
            GO TO 998
         END IF
         CALL FIO_PUNIT( FD, FIOSTAT )
         IF ( FIOSTAT .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': I/O unit release error\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Image & Quality data file(s) (only read if NODATA=FALSE and needed).
      IF ( DANEED .AND. DAMUST .AND. NODATA ) THEN
         CALL ERROUT( 'Error: no image available\\', STATUS )
         GO TO 999

      ELSE IF ( DANEED .AND. ( .NOT.NODATA ) ) THEN
         NOIMA = .TRUE.
         GOTQUAL = .FALSE.
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND('_UED\\', 81, VFILE)
         CALL LINE_WRITS( '%p Reading %s (Image & Quality File).\\',
     :                    VFILE )
         CALL PRTBUF( STATUS )

*     Get virtual memory for data array and read from file.
         CALL ALADR( 'short\\', NS * NL, DATA_VM, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: obtaining memory for DATA\\', STATUS )
         END IF

*     Get new virtual memory.
         CALL ALADR( 'byte\\', NS * NL, QUAL_VM, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: obtaining VM for Data Quality\\',
     :                   STATUS )
         ELSE
            CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
            NNN = NS * NL
            CALL READ_NDF( 'IMAGE', CFILE( :NCHAR ), NNN,
     :                     %VAL( DATA_VM ), %VAL( QUAL_VM ), STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               GOTQUAL = .TRUE.
               NOIMA = .FALSE.
            ELSE
               STATUS = SAI__OK

*     Fetch an I/O unit number.
               CALL FIO_GUNIT( FD, STATUS )

               CALL STR_MOVE( FILE, 81, VFILE )
               CALL STR_APPND( '.UED\\', 81, VFILE )
               CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
               CALL LINE_WRITS(
     :                      '%p Reading %s (Old Style Image File).\\',
     :                      VFILE )
               CALL PRTBUF( STATUS )

               OPEN( UNIT=FD, NAME=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :               FORM='UNFORMATTED', STATUS='OLD', IOSTAT=IOSTAT )
               IF ( DAMUST .AND. ( IOSTAT .NE. SAI__OK ) ) THEN
                  CALL ERRSTR( VFILE )
                  CALL ERROUT( ': file open error\\', STATUS )
                  GO TO 998

               ELSE IF ( IOSTAT .EQ. SAI__OK ) THEN
                  CALL RDDAT( FD, NS, NL, %VAL(DATA_VM), STATUS )
                  IF ( STATUS .EQ. SAI__OK ) NOIMA = .FALSE.
                  CLOSE( UNIT=FD, IOSTAT=ISTAT )
                  CALL FIO_PUNIT( FD, FIOSTAT )
                  IF ( FIOSTAT .NE. SAI__OK ) THEN
                     CALL ERRSTR( VFILE )
                     CALL ERROUT( ': I/O unit release error\\',
     :                            STATUS )
                     GO TO 999
                  END IF
               END IF
            END IF
         END IF

         IF ( ISTAT .NE.  SAI__OK) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file read error\\', STATUS )
            GO TO 999
         END IF
      END IF

*   Data quality file (only read for NODATA=FALSE, and then optional).
      IF ( DANEED .AND. (.NOT.NODATA) .AND. (.NOT. GOTQUAL) ) THEN

*     Fetch an I/O unit number.
         CALL FIO_GUNIT( FD, STATUS )

*     Get new virtual memory.
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '.UEQ\\', 81, VFILE )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
         OPEN( UNIT=FD, NAME=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :         FORM='UNFORMATTED', STATUS='OLD', IOSTAT=IOSTAT )
         CALL RDQUAL( FD, NS, NL, %VAL(QUAL_VM), STATUS )
         CLOSE( UNIT=FD, IOSTAT=ISTAT )
         CALL FIO_PUNIT( FD, FIOSTAT )
         IF ( FIOSTAT .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': I/O unit release error\\', STATUS )
            GO TO 999
         END IF
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', STATUS )
            GO TO 999
         END IF

         IF ( IOSTAT .NE. SAI__OK ) THEN
            STATUS = SAI__OK
            CALL DQ_ZERO( NS * NL, %VAL(QUAL_VM) )

         ELSE
            CALL LINE_WRITS(
     :             '%p Reading %s (Old Style Image Quality File).\\',
     :             VFILE )
            CALL PRTBUF( STATUS )

            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': file read error\\', STATUS )
               NOIMA = .TRUE.
               GO TO 999
            END IF
         END IF
      END IF

*   Spectrum file (optional).
      IF ( SPNEED ) THEN
         IOSTAT = 0
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UES\\', 81, VFILE )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

*      Fetch an I/O unit number.
         CALL FIO_GUNIT( FD, STATUS )

         OPEN( UNIT=FD, NAME=CFILE( :NCHAR )//'.sdf',
     :         ACCESS='SEQUENTIAL', FORM='UNFORMATTED', STATUS='OLD',
     :         IOSTAT=IOSTAT )
         IF ( IOSTAT .EQ. SAI__OK ) THEN
            CALL LINE_WRITS( '%p Reading %s (Spectrum File).\\',
     :                       VFILE )
            CALL PRTBUF( STATUS )
            CLOSE ( FD, IOSTAT=ISTAT )
            IF ( ISTAT .NE. SAI__OK ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': file close error\\', STATUS )
               GO TO 998
            END IF
            NNN = 1
            CALL READ_NDF( 'SPECTRUM', CFILE( :NCHAR ), NNN,
     :                     WDUMMY, BDUMMY, IOSTAT )
         END IF
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL LINE_WRITS( '%p NDF not available %s.\\', VFILE )
            CALL PRTBUF( STATUS )
            CALL STR_MOVE( FILE, 81, VFILE )
            CALL STR_APPND( '.UES\\', 81, VFILE )
            CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

            OPEN( UNIT=FD, NAME=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :            FORM='UNFORMATTED', STATUS='OLD', IOSTAT=IOSTAT )
            IF ( ( IOSTAT .NE. SAI__OK ) .AND. SPMUST ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': open error\\', STATUS )
               GO TO 998

            ELSE IF ( IOSTAT .NE. SAI__OK ) THEN
               NOSPEC = .TRUE.
               NORDER = 0

            ELSE
               STATUS = SAI__OK
               CALL LINE_WRITS(
     :              '%p Reading %s (Version 2 Spectrum File).\\',
     :              VFILE )
               CALL PRTBUF( STATUS )
               CALL RDSPC( FD, STATUS )

               CLOSE( UNIT=FD, IOSTAT=ISTAT )
               IF ( ISTAT .NE. SAI__OK ) THEN
                  CALL ERRSTR( VFILE )
                  CALL ERROUT( ': file close error\\', STATUS )
                  GO TO 998

               ELSE IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERRSTR( VFILE )
                  CALL ERROUT( ': file read error\\', STATUS )
                  GO TO 998
               END IF
               NOSPEC = .FALSE.
            END IF

         ELSE
            STATUS = SAI__OK
            NOSPEC = .FALSE.
         END IF
         CALL FIO_PUNIT( FD, FIOSTAT )
         IF ( FIOSTAT .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': I/O unit release error\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Combined spectrum file (optional).
      IF ( MENEED ) THEN
         IOSTAT = 0
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UEM\\', 81, VFILE )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

*        Fetch an I/O unit number.
         CALL FIO_GUNIT( FD, STATUS )

         OPEN( UNIT=FD, NAME=CFILE( :NCHAR )//'.sdf',
     :         ACCESS='SEQUENTIAL', FORM='UNFORMATTED', STATUS='OLD',
     :         IOSTAT=IOSTAT )
         IF ( IOSTAT .EQ. SAI__OK ) THEN
            CALL LINE_WRITS( '%p Reading %s (Mapped Spectrum File).\\',
     :                       VFILE )
            CALL PRTBUF( STATUS )
            CLOSE ( FD, IOSTAT=ISTAT )
            IF ( ISTAT .NE. SAI__OK ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': file close error\\', STATUS )
               GO TO 998
            END IF
            NNN = 1
            CALL READ_NDF( 'MSPECTRUM', CFILE( :NCHAR ), NNN,
     :                     WDUMMY, BDUMMY, IOSTAT )
         END IF
         IF ( IOSTAT .NE. SAI__OK ) THEN
            CALL LINE_WRITS( '%p NDF not available %s.\\', VFILE )
            CALL PRTBUF( STATUS )
            CALL STR_MOVE( FILE, 81, VFILE )
            CALL STR_APPND( '.UEM\\', 81, VFILE )
            CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

            OPEN( UNIT=FD, NAME=CFILE( :NCHAR ), ACCESS='SEQUENTIAL',
     :         FORM='UNFORMATTED', STATUS='OLD', IOSTAT=IOSTAT )
            IF ( ( IOSTAT .NE. SAI__OK ) .AND. MEMUST ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': open error\\', STATUS )
               GO TO 998

            ELSE IF ( IOSTAT .NE. SAI__OK ) THEN
               NOCOMB = .TRUE.
               NCOMB = 0

            ELSE
               STATUS = SAI__OK
               CALL LINE_WRITS(
     :           '%p Reading %s (Version 2 Mapped Spectrum File).\\',
     :           VFILE )
               CALL PRTBUF( STATUS )
               CALL RDMAP( FD, STATUS )

               CLOSE( UNIT=FD, IOSTAT=ISTAT )
               IF ( ISTAT .NE. SAI__OK ) THEN
                  CALL ERRSTR( VFILE )
                  CALL ERROUT( ': file close error\\', STATUS )
                  GO TO 998

               ELSE IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERRSTR( VFILE )
                  CALL ERROUT( ': file read error\\', STATUS )
                  GO TO 998
               END IF
               NOCOMB = .FALSE.
            END IF

         ELSE
            STATUS = SAI__OK
            NOCOMB = .FALSE.
         END IF

         CALL FIO_PUNIT( FD, FIOSTAT )
         IF ( FIOSTAT .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': I/O unit release error\\', STATUS )
            GO TO 999
         END IF
      END IF

*  Abort.
      GO TO 999

 998  CALL FIO_PUNIT( FD, FIOSTAT )
      IF ( FIOSTAT .NE. SAI__OK ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': I/O unit release error\\', STATUS )
      END IF

 999  CONTINUE

      END
