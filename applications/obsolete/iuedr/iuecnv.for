       SUBROUTINE IUECNV( STATUS )

*+
*  Name:
*     IUECNV

*  Purpose:
*      Convert the specified IUE dataset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRDIMG( FILE, STATUS )

*  Description:
*     The Dataset is stored in 5 coupled files:
*        FILE.UEC    Contains the calibration data.

*-

*  Type Declarations:
      IMPLICIT NONE         ! No implicit typing

*  Status:
      INTEGER STATUS        ! Returned status

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMNEED'
      INCLUDE 'CMMUST'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMGRAF'
      INCLUDE 'CMDYN'

*  Local Variables:
      BYTE FILE(81)         ! Dataset name
      CHARACTER*(81) CFILE  ! Fortran 77 file name
      EQUIVALENCE ( CFILE, FILE )

      LOGICAL OLDDATA
      COMMON / CMVERSION / OLDDATA

      DATA DFREE / 32*.TRUE. /

      CALL PSX_GETENV( 'IUE_DATASET', CFILE, STATUS )

      CALL IRDIMG( FILE, STATUS )
      CALL IWRIMG( FILE, STATUS )

*  Abort.
 999  CONTINUE
      END


      SUBROUTINE IRDIMG( FILE, STATUS )
*+
*  Name:
*     RDIMG

*  Purpose:
*      Read the specified IUE dataset.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDIMG( FILE, STATUS )

*  Description:
*     The Dataset is stored in 5 coupled files:
*        FILE.UEC    Contains the calibration data.
*        FILE.UED    Contains the Data Array.
*        FILE.UEQ    Contains the Data Quality for affected pixels
*                    (only).
*        FILE.UES    Contains any spectrum.
*        FILE.UEM    Contains any mean spectrum.
*
*     Use Fortran I/O for all files.
*     Whether an attempt is made to read each file is determined by the
*     switches in CMNEED. Whether a failed read is fatal is determined
*     by the switches in CMMUST.

*  Arguments:
*     FILE = BYTE( 81 ) (Given)
*        Dataset name.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     JEG: Jack Giddings (STARLINK)
*     PCTR: Paul Rees (STARLINK)

*  History:
*     1-MAY-1982 (JEG):
*        Original version (IUEDR Vn. 1.0).
*     22-SEP-1988 (PCTR):
*        IUEDR Vn. 2.0.

*  Bugs:
*     The calls to DIOINT were needed since any DAREAD invocations in
*     some weird way I don't unterstand ... the over-write problem may
*     remain after DIO removal!

*-

*  Type Declarations:
      IMPLICIT NONE         ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE constants

*  Arguments Given:
      BYTE FILE( 81 )       ! Dataset name

*  Status:
      INTEGER STATUS        ! Returned status

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMNEED'
      INCLUDE 'CMMUST'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMGRAF'

*  Local Variables:
      BYTE VFILE( 81 )      ! VMS file for Calibration
      INTEGER  ndum
      REAL     dummy(100)
      CHARACTER * ( 81 ) CFILE ! Fortran 77 file name
      CHARACTER * ( 10 ) FACC  ! Fortran I/O access mode
      CHARACTER * ( 11 ) FFORM ! Fortran I/O FORM

      CHARACTER*80  LTITLE,LXLAB,LYLAB

      LOGICAL GOTQUAL
      INTEGER IOSTAT        ! Fortran I/O status
      INTEGER ISTAT         ! Status from file close
      INTEGER NCHAR         ! Number of characters for GEN_STOC

      INTEGER NNN,ipd,ipq
      CHARACTER*32  sysname
      CHARACTER*32  nodename
      CHARACTER*32  release
      CHARACTER*32  version
      CHARACTER*32  machine

      LOGICAL OLDDATA
      COMMON / CMVERSION / OLDDATA
*.

*  Calibration file.
      CALL STR_MOVE( FILE, 81, VFILE )
      CALL STR_APPND( '.UEC\\', 81, VFILE )
      CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
      CALL LINE_WRITS(
     :      '%p Check for new style %s (Calibration File).\\',
     :                    VFILE )
      CALL PRTBUF( 8, STATUS )

*  Inquire the ACCESS mode and FORM of the file.
      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION,
     :                MACHINE, STATUS )
      IF ( sysname .EQ. 'VMS' ) THEN
         OPEN( UNIT=40, FILE=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :         READONLY, FORM='FORMATTED', STATUS='OLD',
     :         RECL=8192 , IOSTAT=IOSTAT )
      ELSE
         OPEN( UNIT=40, FILE=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :         READONLY, FORM='FORMATTED', STATUS='OLD',
     :         IOSTAT=IOSTAT )
      ENDIF

      IF ( IOSTAT .NE. 0 ) THEN
        FACC = 'UNKNOWN'
        FFORM = 'UNKNOWN'
      END IF

*  Act on returned FORM and ACCESS values.
      IF ( IOSTAT .EQ. 0 ) THEN

*     Assume the file is in IUEDR Vn. 3.* format.
         OLDDATA = .FALSE.
         CALL RDCAL2( 40, STATUS )
         CLOSE( UNIT=40, IOSTAT=ISTAT )

*     Check if RDCAL2 was successful: if not, then the calibration file
*     may be in IUEDR Vn. 1.*/2.* format. Try this.
         IF ( STATUS.NE.SAI__OK ) THEN
            STATUS = SAI__OK
            OPEN( UNIT=40, FILE=CFILE( : NCHAR ),
     :            ACCESS='SEQUENTIAL', READONLY, FORM='UNFORMATTED',
     :            STATUS='OLD', IOSTAT=IOSTAT )
            IF ( IOSTAT .NE. 0 ) THEN
               CALL ERRSTR( VFILE )
               CALL ERROUT( ': file open error\\', 4, STATUS )
               GO TO 999
            END IF

            CALL LINE_WRITS(
     :        '%p Reading old style %s (Calibration File).\\',
     :                       VFILE )
            CALL PRTBUF( 8, STATUS )
            OLDDATA = .TRUE.
            CALL RDCAL( 40, STATUS )
            CLOSE( UNIT=40, IOSTAT=ISTAT )
         END IF
      ELSE

*     Assume the file is in IUEDR Vn. 1.*/2.* format.
         OPEN( UNIT=40, FILE=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :         READONLY, FORM='UNFORMATTED', STATUS='OLD',
     :         IOSTAT=IOSTAT )
         IF ( IOSTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file open error\\', 4, STATUS )
            GO TO 999
         END IF

         OLDDATA = .TRUE.
         CALL RDCAL( 40, STATUS )
         CLOSE( UNIT=40, IOSTAT=ISTAT )
      END IF

      IF ( ISTAT .NE. 0 ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file close error\\', 4, STATUS )
         GO TO 999
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file read error\\', 4, STATUS )
         GO TO 999
      END IF

*  Image data file (only read if NODATA=FALSE and needed).
      NOIMA = .TRUE.
      GOTQUAL = .FALSE.
      NNN = NS * NL
      CALL STR_MOVE( FILE, 81, VFILE )
      CALL STR_APPND( '.UED\\', 81, VFILE )
      CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
      CALL LINE_WRITS( '%p Reading %s (Image File).\\', VFILE )
      CALL PRTBUF( 8, STATUS )

      OPEN( UNIT=40, NAME=CFILE( : NCHAR ), ACCESS='sequential',
     :      READONLY, FORM='unformatted', STATUS='old',
     :      IOSTAT=IOSTAT )
      IF ( IOSTAT .NE. 0 ) THEN
         TYPE *, '! No Image information to convert.'
         NOIMA = .TRUE.
         GOTO 200

*   Get virtual memory for data array and read from file.
      ELSE IF ( IOSTAT .EQ. 0 ) THEN
         CALL ALADR( 'short\\', NS*NL, DATA_VM, STATUS )
         CALL RDDAT( 40, NS, NL, %VAL( DATA_VM ), STATUS )
         IF ( STATUS.EQ.SAI__OK ) NOIMA = .FALSE.
         CLOSE( UNIT=40, IOSTAT=ISTAT )
      END IF

      IF ( ISTAT .NE. 0 ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file close error\\', 4, STATUS )
         GO TO 200
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file read error\\', 4, STATUS )
         GO TO 200
      END IF

*  Get new virtual memory.
      CALL STR_MOVE( FILE, 81, VFILE )
      CALL STR_APPND( '.UEQ\\', 81, VFILE )
      CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
      OPEN( UNIT=40, NAME=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :      READONLY, FORM='UNFORMATTED', STATUS='OLD',
     :      IOSTAT=IOSTAT )
      IF ( IOSTAT .EQ. 0 ) THEN
*  Get new virtual memory.
         CALL ALADR( 'byte\\', NS*NL, QUAL_VM, STATUS )
         CALL RDQUAL( 40, NS, NL, %VAL(QUAL_VM), STATUS )
         CLOSE( UNIT=40, IOSTAT=ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', 4, STATUS )
            GO TO 200
         ENDIF
      ELSE
         TYPE *, '! No quality information to convert.'
         GOTO 200
      ENDIF
      IF ( IOSTAT .NE. 0 ) THEN
         STATUS = SAI__OK
         CALL DQ_ZERO( NS * NL, %VAL(QUAL_VM) )
      ELSE
         CALL LINE_WRITS( '%p Reading %s (Image Quality File).\\',
     :                       VFILE )
         CALL PRTBUF( 8, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file read error\\', 4, STATUS )
            NOIMA = .TRUE.
            GO TO 200
         END IF
      END IF

 200  CONTINUE

*  Spectrum file (optional).
      OLDDATA=.TRUE.
      IOSTAT = 0
      CALL STR_MOVE( FILE, 81, VFILE )
      CALL STR_APPND( '.UES\\', 81, VFILE )
      CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

      OPEN( UNIT=40, NAME=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :      READONLY, FORM='UNFORMATTED', STATUS='OLD',
     :      IOSTAT=IOSTAT )
      IF (  IOSTAT .NE. 0 ) THEN
         TYPE *, '! No Spectrum information to convert.'
         NOSPEC = .TRUE.
         NORDER = 0
         GOTO 300
      ELSE
         STATUS = SAI__OK
         CALL LINE_WRITS( '%p Reading %s (Spectrum File).\\', VFILE )
         CALL PRTBUF( 8, STATUS )
         CALL RDSPC( 40, STATUS )
         CLOSE( UNIT=40, IOSTAT=ISTAT )

         IF ( ISTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', 4, STATUS )
            GO TO 300
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file read error\\', 4, STATUS )
            GO TO 300
         END IF

         NOSPEC = .FALSE.
      END IF

 300  CONTINUE

*  Combined spectrum file (optional).
      OLDDATA=.TRUE.
      IOSTAT = 0
      CALL STR_MOVE( FILE, 81, VFILE )
      CALL STR_APPND( '.UEM\\', 81, VFILE )
      CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

      OPEN( UNIT=40, NAME=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :      READONLY, FORM='UNFORMATTED', STATUS='OLD',
     :      IOSTAT=IOSTAT )
      IF ( IOSTAT .NE. 0 ) THEN
         TYPE *, '! No Merged spectrum information to convert.'
         NOCOMB = .TRUE.
         NCOMB = 0
      ELSE
         CALL LINE_WRITS( '%p Reading %s (Mapped Spectrum File).\\',
     :                    VFILE )
         CALL PRTBUF( 8, STATUS )
         CALL RDMAP( 40, STATUS )
         CLOSE( UNIT=40, IOSTAT=ISTAT )

         IF ( ISTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', 4, STATUS )
            GO TO 999
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file read error\\', 4, STATUS )
            GO TO 999
         END IF

         NOCOMB = .FALSE.
      END IF

*  Abort.
 999  CONTINUE
      END


      SUBROUTINE IWRIMG( FILE, STATUS )
*+
*  Name:
*     WRIMG

*  Purpose:
*      Read the calibration part of the dataset from the given logical
*      unit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL WRIMG( FD, STATUS )

*  Description:
*      The dataset is stored in 5 coupled files:
*         FILE.UEC    Contains the calibration data.
*         FILE.UED    Contains the Data Array.
*         FILE.UEQ    Contains the Data Quality for affected pixels
*                     (only).
*         FILE.UES    Contains any spectrum.
*         FILE.UEM    Contains any mean spectrum.
*
*
*      Use sequential Fortran I/O for all files.

*  Arguments:
*     FILE = INTEGER (Given)
*        The name of the dataset.
*     STATUS = INTEGER (Returned)
*        The global status.

*  Authors:
*     JEG: Jack Giddings (STARLINK)
*     PCTR: Paul Rees (STARLINK)

*  History:
*     1-MAY-1982 (JEG):
*        Original version (IUEDR Vn. 1.0).
*     22-SEP-1988 (PCTR):
*        IUEDR Vn. 2.0.

*  Bugs:

*-

*  Type Declarations:
      IMPLICIT NONE         ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE constants

*  Arguments Given:
      BYTE FILE( 81 )       ! Dataset name

*  Status:
      INTEGER STATUS        ! Returned status

*  Global Variables:
      INCLUDE 'CMFILE'
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDATA'
      INCLUDE 'CMSAVE'
      INCLUDE 'CMCOMB'
      INCLUDE 'CMPSX'

*  Local Variables:
      REAL*8 WAVDMY          ! Dummy for write_ndf calls

      BYTE VFILE( 81 )       ! VMS file for Calibration

      INTEGER    ipd , ipq
      CHARACTER * ( 81 ) CFILE ! Fortran 77 file name

      INTEGER IOSTAT         ! Fortran I/O status
      INTEGER ISTAT          ! Status from file close
      INTEGER NCHAR          ! Number of characters for GEN_STOC

      INTEGER NNN
      LOGICAL NOT            ! Whether no input defined

      CHARACTER*80  IMTITLE

*  Check that there is an image.
      IF ( NOHEAD ) THEN
         CALL ERROUT( ' Error : no dataset\\', 4, STATUS )
         GO TO 999
      END IF

*  Calibration file.
      CALL STR_MOVE( FILE, 81, VFILE )
      CALL STR_APPND( '.UEC\\', 81, VFILE )
      CALL LINE_WRITS( '%p Writing %s (Calibration File).\\', VFILE )
      CALL PRTBUF( 8, STATUS )
      CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )
      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION,
     :                  MACHINE, STATUS )
      IF ( SYSNAME .EQ. 'VMS' ) THEN
         OPEN( UNIT=40, NAME=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :         FORM='FORMATTED', STATUS='NEW',
     :         RECL=8192 , IOSTAT=IOSTAT )
      ELSE
         OPEN( UNIT=40, NAME=CFILE( : NCHAR ), ACCESS='SEQUENTIAL',
     :         FORM='FORMATTED', STATUS='NEW',
     :         IOSTAT=IOSTAT )
      ENDIF
      IF ( IOSTAT.NE.0 ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file create error\\', 4, STATUS )
         GO TO 999
      END IF

      CALL WRCAL2( 40, STATUS )
      CLOSE( UNIT=40, IOSTAT=ISTAT )
      IF ( ISTAT.NE.0 ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file close error\\', 4, STATUS )
         GO TO 999
      ELSE IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERRSTR( VFILE )
         CALL ERROUT( ': file write error\\', 4, STATUS )
         GO TO 999
      END IF

*  Image file (if data present and has changed).
      IF (  .NOT.NOIMA  ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UED\\', 81, VFILE )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

         CALL LINE_WRITS( '%p Writing %s (Image File).\\', VFILE )
         CALL PRTBUF( 8, STATUS )

         NNN = NS * NL
         IMTITLE = 'IUE image'
         CALL WRITE_NDF( 'IMAGE', CFILE(1:NCHAR), NNN,
     :                    WAVDMY, WAVDMY,
     :                    'NS', 'NL', IMTITLE,
     :                    1, STATUS )
         IF ( ISTAT .NE. 0 ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file close error\\', 4, STATUS )
            GO TO 999
         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', 4, STATUS )
            GO TO 999
         END IF

         DACHAN = .FALSE.
      END IF

*  Spectrum file (if spectrum present and if changed).
      IF ( ( .NOT.NOSPEC ) .AND. ( NORDER.GT.0 ) ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UES\\', 81, VFILE )
         CALL LINE_WRITS( '%p Writing %s (Spectrum File).\\', VFILE )
         CALL PRTBUF( 8, STATUS )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

         CALL WRSPC( CFILE(1:NCHAR), STATUS )
         IF ( STATUS.NE.SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', 4, STATUS )
            GO TO 999
         END IF

         SPCHAN = .FALSE.
      END IF

*  Combined spectrum file (if present and changed).
      IF ( ( .NOT.NOCOMB ) .AND. ( NCOMB.GT.0 ) ) THEN
         CALL STR_MOVE( FILE, 81, VFILE )
         CALL STR_APPND( '_UEM\\', 81, VFILE )
         CALL LINE_WRITS( '%p Writing %s (Mapped Spectrum File).\\',
     :                    VFILE )
         CALL PRTBUF( 8, STATUS )
         CALL GEN_STOC( VFILE, 81, CFILE, NCHAR )

         CALL WRMAP( CFILE(1:NCHAR), STATUS )
         IF ( STATUS.NE.SAI__OK ) THEN
            CALL ERRSTR( VFILE )
            CALL ERROUT( ': file write error\\', 4, STATUS )
            GO TO 999
         END IF

         MECHAN = .FALSE.
      END IF

*  Abort.
 999  CONTINUE
      END
