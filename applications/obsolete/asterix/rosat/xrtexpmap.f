*+  XRTEXPMAP - Create XRT exposure map
      SUBROUTINE XRTEXPMAP( STATUS )
*
*    Description :
*
*     Program XRTEXPMAP is used to cast the band-correct exposure map for
*     pointed observations.  It uses detector maps created from survey
*     data.
*
*     This program was written to work from ASCII files create by MIDAS
*     from tables from the observation data set.  Subsets of data are
*     extracted from the ATTITUDE and EVENTRATES files.  An accepted time
*     file is also used.  This file can be created from the accepted times
*     given in the observation data set but I suggest verifying the times
*     by hand.
*
*     The program follows the suggestions of Snowden et al. (1992, ApJ,
*     393 819) and Plucinsky et al. (1993, ApJ, in press) to exclude
*     regions of the PSPC near the edges of the PSPC which are strongly
*     affected by the particle background, the "bright line" regions.
*     The program also assumes that a selection has been done on the
*     data to exclude all events which follow within 0.5 ms of a
*     "precursor" event.  This excludes some of the low pulse-height
*     crud which affects data collected after May 1992.
*
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     Steve Snowden (MPE::)
*     David J. Allan (ROSAT,University of Birmingham)
*     author (institution::username)
*
*    History :
*
*     18 Dec 92 :
*     15 Apr 94 : V1.7-0  ADAMised and made portable (DJA)
*      9 Jun 94 : V1.7-1  Handles RDF data (DJA)
*      5 Sep 95 : V1.8-0  Bug calculating exposure time when first attitude
*                         record was ok (DJA)
*     11 Dec 1995 : V2.0-0 ADI port (DJA)
*     21 Feb 1996 : V2.0-1 Removed SIND,COSD for Linux port (DJA)
*     17 Mar 1999 : v2.3-0 FITS file input (DGED)
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'MATH_PAR'
      INCLUDE 'PAR_ERR'
*
*    Global variables :
*
      INCLUDE 'XRTSRT_CMN'
      INCLUDE 'XRTHEAD_CMN'
*
*    Status :
*
      INTEGER STATUS
*
*    Function declarations :
*
      INTEGER			CHR_LEN
      LOGICAL			CHR_SIMLR
*
*    Local constants :
*
      REAL			MAXGTIME           	! Maximum no. good times
	PARAMETER		( MAXGTIME = 1000 )
      REAL			PIXSIZE 		! Pix size in arcsec
        PARAMETER		( PIXSIZE = 14.94733 )
      DOUBLE PRECISION          JAN26_1991		! Date of PSPC change
        PARAMETER               ( JAN26_1991 = 48282.0 )
*
*    Local variables :
*
      CHARACTER*60		DETDIR			! DET map directory
      CHARACTER*79		LINE			! Output text
      CHARACTER 		TIMEFILE*80		! Good times list

      DOUBLE PRECISION 		DSCS			! Spacecraft clock

      DOUBLE PRECISION		BASESC			! S/c clock at start
      DOUBLE PRECISION		GSTART(MAXGTIME)	! Good time starts
      DOUBLE PRECISION		GEND(MAXGTIME)		! Good time ends

      INTEGER			ATT_R_PTR		! Attitude ROLL
      INTEGER			ATT_T_PTR		! Attitude TIME
      INTEGER			ATT_X_PTR		! Attitude XOFFSET
      INTEGER			ATT_Y_PTR		! Attitude YOFFSET
      INTEGER			BLK			! FITS block size
      INTEGER			DIMS(DAT__MXDIM)	! O/p dimensions
      INTEGER			EVR_A1LL_PTR		! Event rate A1LL
      INTEGER			EVR_AXE_PTR		! Event rate AXE
      INTEGER			EVR_AEXE_PTR		! Event rate AEXE
      INTEGER			EVR_MV_PTR		! Event rate MV
      INTEGER			EVR_T_PTR		! Event rate TIME
      INTEGER			IATT			! Attitude loop
      INTEGER			IEVR			! Event rate loop
      INTEGER			IFILE			! Detector map no.
      INTEGER			IMV			! A master veto value
      INTEGER			ITIME			! Loop over time slots
      INTEGER			LUN			! Logical unit number
      INTEGER			MFID			! O/p check map
      INTEGER			NACTGTIME		! Num good time slots
      INTEGER			NATTREC			! No. attitude records
      INTEGER			NEVRREC			! No. ev rate records
      INTEGER			OFID			! Output dataset id
      INTEGER			TEMP_PTR		! Temp data for RDF
*
      LOGICAL			BAD			! Any bad points?
      LOGICAL 			MAPCHK			! Create map check?
*
      INTEGER                   IUNIT                   ! Logical I/O unit
      INTEGER                   MAXRAW                  ! Max value
        PARAMETER (MAXRAW = 500)
      INTEGER                   NFILES                  ! Number of files
*
      CHARACTER*100             FILES(MAXRAW)           ! File name aray
      CHARACTER*132             FITSDIR                 ! Directory for FITS
      CHARACTER*132             FROOT                   ! Root of FITS filename
      CHARACTER*5               ORIGIN                  ! Origin of FITS file
      CHARACTER*132             FILENAME
*
      INTEGER ANYF                                 ! Notes undefined array elements
      INTEGER COLNO                                ! Fits table, column no
      INTEGER FEOF                                 ! Marks end of FITS file
      INTEGER ENTIM
      INTEGER EVNHDU                               ! Position of eventrate in FITS file
      INTEGER FBEG                                 ! Fits table, start
      INTEGER HTYPE                                ! Fits header, style
      INTEGER LP
      INTEGER MXCOL                                ! Max number of columns
        PARAMETER (MXCOL = 512)
      INTEGER NROWS                                ! Fits table, no of rows
      INTEGER NHDU                                 ! Fits header, unit
      INTEGER VARIDAT                              ! Fitsio variable
      INTEGER TFIELDS                              ! Fits header, no fields per rows
      INTEGER BLOCK

      CHARACTER*20  EXTNAME                         ! File extension name
      CHARACTER*12  TTYPE(MXCOL)                    ! Fits header, col name
      CHARACTER*40  TFORM(MXCOL)                    ! Fits header, var type
      CHARACTER*40  TUNIT(MXCOL)                    ! Fits header, units of measurement

*
        INTEGER IA, IA1LL, IAEXE, IAXE, I,
     +      IASP(3,10000), IDET, IERR, II, III, ILIVEN, IR,
     +      IROLL, ISCS, ISCSO, IX, IXX, IY,
     +      IYY, NB, NB1, NB2, NB3, NG, NUM
*
        REAL A, ANGLE, ASP(10000),
     +      COSROL, DEADTP, DELT, EXP, EXPARR(512,512), FLIVE,
     +      FLIVE1, FLIVE2, NMV, RMAP(512,512), ROLL,
     +      SCALE1(12), SCALE2(12), SINROL, TEMPAR(512,512),
     +      TMV, TOTEXP, TOTIME, X, XX, Y, YY
*
	CHARACTER INSMAP*80
*	From INTEGER to improve exposure time evaluation
	DOUBLE PRECISION IACTBE, IACTEN
*
*    Local data :
*
*    Primary PSPC (PSPC C) detector maps
      CHARACTER*40 INSTMP1(12), INSTMP2(12)
      DATA INSTMP1 /
     :      'det_8_19_c',	'det_20_41_c',
     :      'det_42_51_c',	'det_52_69_c',
     :      'det_70_90_c',	'det_91_131_c',
     :      'det_132_201_c',	'det_8_41_c',
     :      'det_52_90_c',	'det_91_201_c',
     :      'det_42_131_c',	'det_42_201_c'/

*    Secondary PSPC (PSPC B) detector maps
      DATA INSTMP2 /
     :      'det_8_19_b',	'det_20_41_b',
     :      'det_42_51_b',	'det_52_69_b',
     :      'det_70_90_b',	'det_91_131_b',
     :      'det_132_201_b',	'det_8_41_b',
     :      'det_52_90_b',	'det_91_201_b',
     :      'det_42_131_b',	'det_42_201_b'/

*    The following data statement is for the scale factors of the
*    detector maps
      DATA SCALE1  /12.9761E-5, 4.6281E-5, 6.0585E-5, 5.0370E-5,
     :      5.6676E-5, 5.4558E-5, 5.9294E-5, 7.4916E-5, 4.6767E-5,
     :      4.8066E-5, 4.6940E-5, 4.6642E-5/
      DATA SCALE2  /15.8361E-5, 8.1814E-5, 6.3375E-5, 5.0491E-5,
     :      5.0294E-5, 4.9386E-5, 5.3749E-5, 9.2081E-5, 4.6737E-5,
     :      4.7305E-5, 4.7219E-5, 4.4383E-5/

*    The following data statement is for the deadtime scale factor
      DATA			DEADTP/234./
*
*    Version :
*
      CHARACTER*30		VERSION
        PARAMETER 		( VERSION = 'XRTEXPMAP Version 2.3-0')
*-

*    Initialise Asterix
      CALL AST_INIT()

*    Version id
      CALL MSG_PRNT( VERSION )
*
*  Get input file details
*  Get the current working directory
      CALL UTIL_GETCWD( FITSDIR, STATUS )
      CALL USI_DEF0C( 'RAWDIR', FITSDIR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

      CALL USI_GET0C( 'RAWDIR', FITSDIR, STATUS )
*  Any FITS files?
      CALL UTIL_FINDFILE(FITSDIR, '*.fits', MAXRAW, FILES, NFILES,
     :                                                       STATUS)
*  If no files - exit
      IF (NFILES .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_PRNT ('XRTEXPMAP : Error - No FITS file found')
         CALL MSG_PRNT ('XRTEXPMAP : Uses RDF FITS files only.')
         CALL MSG_PRNT ('XRTEXPMAP : Please use VERSION V2.2-1 for SDF'
     :                                                //'file input')
         GOTO 99
      END IF
*
*  Get root name of FITS file
      CALL USI_GET0C ('ROOTNAME', FROOT, STATUS )
*  Append extension of FITS extension containing header
      SRT_ROOTNAME = FROOT(1:CHR_LEN(FROOT)) // '_bas.fits'
*  Does file exist?
      CALL UTIL_FINDFILE(FITSDIR, SRT_ROOTNAME, MAXRAW, FILES, NFILES,
     :                                                       STATUS)
      IF (NFILES .LE. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_PRNT ('XRTEXPMAP : Error - Header file not found')
         GOTO 99
      END IF

      SRT_ROOTNAME = FITSDIR(1:CHR_LEN(FITSDIR))//'/' //SRT_ROOTNAME
      CALL MSG_PRNT('XRTEXPMAP : Using FITS file : '// SRT_ROOTNAME)
*
*  Open the FITS file
      CALL FIO_GUNIT(IUNIT, STATUS)
      CALL FTOPEN(IUNIT, SRT_ROOTNAME, 0, BLOCK, STATUS)
      IF ( STATUS .NE. SAI__OK ) THEN
	 CALL MSG_SETC('FNAM',SRT_ROOTNAME)
         CALL MSG_PRNT('XRTEXPMAP : Error - opening file ^FNAM **')
         GOTO 99
      ENDIF
*
      ORIGIN = 'RDF'
*  Read in FITS header
      CALL RAT_RDHEAD(IUNIT, ORIGIN, STATUS)
*
      IF ( STATUS .NE. SAI__OK ) GOTO 99
*
*  Close FITS files
      CALL FTCLOS(IUNIT, STATUS)
      CALL FIO_PUNIT(IUNIT, STATUS)
*
      CALL USI_CREAT( 'OUT', ADI__NULLID, OFID, STATUS)
*
*    Get observation start time
      BASESC = HEAD_BASE_SCTIME

*    Choose detector number
      IF ( CHR_SIMLR(HEAD_DETECTOR,'PSPCB') ) THEN
        IDET = 2
      ELSE IF ( CHR_SIMLR(HEAD_DETECTOR,'PSPCC') ) THEN
        IDET = 1
      ELSE
        IF ( HEAD_BASE_MJD .LT. JAN26_1991 ) THEN
          IDET = 1
          CALL MSG_SETC( 'D', 'C' )
        ELSE
          IDET = 2
          CALL MSG_SETC( 'D', 'B' )
        END IF
        CALL MSG_PRNT( 'Invalid detector id in dataset, using PSPC ^D'/
     :                              /' derived from observation time' )
      END IF

*    Open good times file
      CALL USI_GET0C( 'TIMRANGE', TIMEFILE, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN

*      No good time slots
        CALL ERR_ANNUL( STATUS )
        NACTGTIME = 0

      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99

      ELSE

*      Decode good times file
        CALL UTIL_TDECODE( TIMEFILE, HEAD_BASE_MJD, MAXGTIME,
     :                      NACTGTIME, GSTART, GEND, STATUS )

*      Convert to spacecraft clock
        DO ITIME = 1, NACTGTIME
          GSTART(ITIME) = GSTART(ITIME) + BASESC
          GEND(ITIME) = GEND(ITIME) + BASESC
        END DO

*      Inform user of good slots
        CALL MSG_SETI( 'NOK', NACTGTIME )
        CALL MSG_SETC( 'FILE', TIMEFILE )
        CALL MSG_PRNT( 'Read ^NOK good time slots from ^FILE' )

      END IF

*    Present list of available maps
      IF ( IDET .EQ. 2 ) THEN
	DO I=1,12
	  WRITE(LINE,'(5X,I2,2X,A)') I,INSTMP2(I)
          CALL MSG_PRNT( LINE )
	END DO
      ELSE IF ( IDET .EQ. 1 ) THEN
	DO I=1,12
	  WRITE(LINE,'(5X,I2,2X,A)') I,INSTMP1(I)
          CALL MSG_PRNT( LINE )
	END DO
      END IF

*    Select map
      CALL USI_GET0I( 'IFILE', IFILE, STATUS )

*    Get directory containing detector map files
      CALL PSX_GETENV( 'AST_ETC', DETDIR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Switch on detector
      IF ( IDET .EQ. 1 ) THEN
	INSMAP = DETDIR(1:CHR_LEN(DETDIR))//'/'//INSTMP1(IFILE)
     :                  (:CHR_LEN(INSTMP1(IFILE)))//'.fits'
        CALL MSG_SETC( 'MAP', INSTMP2(IFILE) )
      ELSE IF ( IDET .EQ. 2 ) THEN
	INSMAP = DETDIR(1:CHR_LEN(DETDIR))//'/'//INSTMP2(IFILE)
     :                  (:CHR_LEN(INSTMP2(IFILE)))//'.fits'
        CALL MSG_SETC( 'MAP', INSTMP2(IFILE) )
      END IF
      CALL MSG_PRNT( 'Opened detector map ^MAP' )

*    Read in the map file
      CALL FIO_GUNIT( LUN, STATUS )
      CALL FTOPEN( LUN, INSMAP, 0, BLK, STATUS )
      IF ( STATUS .NE. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Unable to open detector map file', STATUS )
        GOTO 99
      END IF
      CALL FTGPVE( LUN, 0, 1, 512*512, 0, RMAP, BAD, STATUS )
      CALL FTCLOS( LUN, STATUS )
      CALL FIO_PUNIT( LUN, STATUS )

*  Output detector map?
      CALL USI_CREAT( 'DETMAP', ADI__NULLID, MFID, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
        CALL ERR_ANNUL( STATUS )
        MAPCHK = .FALSE.
      ELSE IF ( STATUS .NE. SAI__OK ) THEN
        GOTO 99
      ELSE
        MAPCHK = .TRUE.
      END IF

*    Set up map dimensions
      DIMS(1) = 512
      DIMS(2) = 512

*    Write detector map if needed
      IF ( MAPCHK ) THEN
        CALL XRTEXPMAP_PUTIM( DIMS, PIXSIZE, RMAP,
     :        'Detector map', ' ', ' ', MFID, STATUS )
        CALL USI_ANNUL( 'DETMAP', STATUS )
      END IF
*
*    Center the instrument map, invert the Y-axis, and turn it real.
*    Also, exclude the "bright line" regions of enhanced particle
*    background as suggested by Plucinsky et al. (1993)
      CALL ARR_COP1R( 512*512, RMAP, TEMPAR, STATUS )
      DO I = 25, 488
        DO II = 35, 456
          IF ( II .EQ. 35 ) THEN
            RMAP(I,514-II-12) = 0.25*TEMPAR(I,II)
          ELSEIF(II .EQ. 456) THEN
            RMAP(I,514-II-12) = 0.44*TEMPAR(I,II)
          ELSE
            RMAP(I,514-II-12) = TEMPAR(I,II)
          END IF
        END DO
      END DO
*
*  The ASPECT/EVRATE tables are in _ANC.FITS
      FILENAME = FROOT(1:CHR_LEN(FROOT))//'_anc.fits'
      FILENAME =  FITSDIR(1:CHR_LEN(FITSDIR))//'/'//FILENAME
*
*  Open the FITS file
      CALL FIO_GUNIT(IUNIT,STATUS)
      CALL FTOPEN(IUNIT,FILENAME,0,BLOCK,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
	 CALL MSG_SETC('FNAM',FILENAME)
         CALL MSG_PRNT('XRTEXPMAP: ERROR - opening file ^FNAM **')
         GOTO 99
      ENDIF
*
*  Locate ATTITUDE data
*  Move to FITS header.
      NHDU = 1
      FEOF = 0
      CALL FTMAHD(IUNIT, 1, HTYPE, STATUS)
*     Locate EVRATE table in FITS file.
      DO WHILE (EXTNAME(1:6) .NE. 'ASPECT' .AND. FEOF .NE. NHDU)
         FEOF = NHDU
*        Move to the next data unit.
         CALL FTMRHD(IUNIT,1,HTYPE,STATUS)
*         Get the current hdu values
         CALL FTGHDN(IUNIT,NHDU)
*        If type is binary table get table details
         IF (HTYPE .EQ. 2) THEN
            CALL FTGBNH(IUNIT, NROWS, TFIELDS, TTYPE, TFORM,
     :      TUNIT, EXTNAME, VARIDAT, STATUS)
         END IF
      ENDDO
      EVNHDU = NHDU
*
* Quit if file coundn't be opened
      IF (NHDU .EQ. FEOF) THEN
         CALL MSG_PRNT('XRTEXPMAP : ERROR - failed to find '//
     :   'ASPECT extension in FITS file')
         GOTO 99
      END IF
*
*  Find the 'TIME' column position in the ASPECT extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:4) .EQ. 'TIME') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPD(1,NROWS,ATT_T_PTR,STATUS)
            CALL FTGCVD(IUNIT, COLNO, FBEG, 1, NROWS, 0.D0,
     :      %VAL(ATT_T_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column TIME not found'
     :      // ' in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
*
*  Find the 'XOFFSET' column position in the ASPECT extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:6) .EQ. 'RA_CAS') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,ATT_X_PTR,STATUS)
            CALL DYN_MAPR(1,NROWS,TEMP_PTR,STATUS)
            CALL FTGCVE(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(TEMP_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column XOFFSET not'
     :      // ' found in EVENTRATE column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
         CALL XRTEXPMAP_RESCALE( NROWS, %VAL(TEMP_PTR),
     :                           %VAL(ATT_X_PTR), STATUS )
*
*  Find the 'YOFFSET' column position in the ASPECT extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:7) .EQ. 'DEC_CAS') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,ATT_Y_PTR,STATUS)
            CALL FTGCVE(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(TEMP_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column YOFFSET not'
     :      // ' found in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
         CALL XRTEXPMAP_RESCALE( NROWS, %VAL(TEMP_PTR),
     :                           %VAL(ATT_Y_PTR), STATUS )
*
*  Find the 'ROLL' column position in the ASPECT extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:8) .EQ. 'ROAN_CAS') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,ATT_R_PTR,STATUS)
            CALL FTGCVE(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(TEMP_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column YOFFSET not'
     :      // ' found in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
         CALL XRTEXPMAP_RESCALE( NROWS, %VAL(TEMP_PTR),
     :                           %VAL(ATT_R_PTR), STATUS )
         NATTREC = NROWS !Number of attribute records
*
*  Locate EVENTRATE data
*  Move to FITS header.
      NHDU = 1
      FEOF = 0
      CALL FTMAHD(IUNIT, 1, HTYPE, STATUS)
*     Locate EVRATE table in FITS file.
      DO WHILE (EXTNAME(1:6) .NE. 'EVRATE' .AND. FEOF .NE. NHDU)
         FEOF = NHDU
*        Move to the next data unit.
         CALL FTMRHD(IUNIT,1,HTYPE,STATUS)
*         Get the current hdu values
         CALL FTGHDN(IUNIT,NHDU)
*        If type is binary table get table details
         IF (HTYPE .EQ. 2) THEN
            CALL FTGBNH(IUNIT, NROWS, TFIELDS, TTYPE, TFORM,
     :      TUNIT, EXTNAME, VARIDAT, STATUS)
         END IF
      ENDDO
*
* Quit if file coundn't be opened
      IF (NHDU .EQ. FEOF) THEN
         CALL MSG_PRNT('XRTEXPMAP : ERROR - failed to find '//
     :   'EVENTRATE extension in FITS file')
         GOTO 99
      END IF
*
*  Find the 'TIME' column position in the EVRATE extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:4) .EQ. 'TIME') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,EVR_T_PTR,STATUS)
            CALL FTGCVJ(IUNIT, COLNO, FBEG, 1, NROWS, 0.D0,
     :      %VAL(EVR_T_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column TIME not found'
     :      // ' in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
*
*  Find the 'XTRANSM' column position in the EVRATE extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:7) .EQ. 'XTRANSM') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,EVR_AEXE_PTR,STATUS)
            CALL FTGCVJ(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(EVR_AEXE_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column XTRANSM not'
     :      // ' found in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
*
*  Find the 'A1_AL' column position in the EVRATE extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
           IF ( TTYPE(LP)(1:5) .EQ. 'A1_AL') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,EVR_A1LL_PTR,STATUS)
            CALL FTGCVJ(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(EVR_A1LL_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column A1_AL not'
     :      // ' found in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
*
*  Find the 'XACC ' column position in the EVRATE extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
*        Note the blank
           IF ( TTYPE(LP)(1:5) .EQ. 'XACC ') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,EVR_AXE_PTR,STATUS)
            CALL FTGCVJ(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(EVR_AXE_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column XACC not'
     :      // ' found in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
*
*  Find the 'MVRATE' column position in the EVRATE extension.
         COLNO = 0
         LP = 1
         DO WHILE (LP .NE. TFIELDS+1)
*        Note the blank
           IF ( TTYPE(LP)(1:6) .EQ. 'MV_ACO') THEN
               COLNO = LP
               LP = TFIELDS
            END IF
            LP = LP + 1
         END DO
*
*  Create array and read in selected column details
       IF (COLNO .NE. 0) THEN
         FBEG = 1
            CALL DYN_MAPI(1,NROWS,EVR_MV_PTR,STATUS)
            CALL FTGCVJ(IUNIT, COLNO, FBEG, 1, NROWS, 0,
     :      %VAL(EVR_MV_PTR),ANYF,STATUS)
            ENTIM = NROWS
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_PRNT('XRTEXPMAP : ERROR - array creation error')
               GOTO 99
            ENDIF
         ELSE
            CALL MSG_PRNT( 'XRTEXPMAP : ERROR - column MVRATE not'
     :      // ' found in ASPECT column')
            STATUS = SAI__ERROR
            GOTO 99
         ENDIF
*
         NEVRREC = NROWS
*
      IF ( STATUS .NE. SAI__OK ) GOTO 99
*
*    Start the loop over the attitude file
      NUM = 0
      IEVR = 1
      IACTEN = -100000
      DO IATT = 1, NATTREC

*      Extract,
*       DSCS   double precision space craft clock seconds
*       IX     pointing offset from nominal position, X direction
*       IY     pointing offset from nominal position, Y direction
*       IROLL  detector roll angle
        CALL ARR_ELEM1D( ATT_T_PTR, NATTREC, IATT, DSCS, STATUS )
        CALL ARR_ELEM1I( ATT_X_PTR, NATTREC, IATT, IX, STATUS )
        CALL ARR_ELEM1I( ATT_Y_PTR, NATTREC, IATT, IY, STATUS )
        CALL ARR_ELEM1I( ATT_R_PTR, NATTREC, IATT, IROLL, STATUS )

*  Determine the delta time from the last entry
*
*	CRB:
*		DIAGNOSTICS AND TESTS ON CODE SUGGEST THAT IF THE ATTITUDE TIME
*		FALLS WITHIN A SELECTION PERIOD THEN WHEN THE LAST ATTITUDE
*		TIME WAS A LONG TIME AGO THE DELT VALUE IS PUSHED VERY HIGH
*		HAVE PUT IN SAFEGUARDS IN CODE LATER ON TO AVOID ANY
*		PROBELMS WITH THIS SO THAT IF DELT >1 THEN DELT = 1.
*		THIS IS A CHEAT BUT UNSURE WHY
*		A/	STEVE HAS EVER USED THE DELT VALUE
*		B/	STEVE HAS NOT NOTICED THE PROBLEM BEFORE
        ISCS = DSCS
        IF ( IATT .EQ. 1 ) THEN
          DELT = 1.0
        ELSE
          DELT = ISCS - ISCSO
        END IF
        ISCSO = ISCS

*      Flip the Y value for consistancy.  Increasing Y in SASS is downward
*      in declination.  The instrument map has already been flipped
        IY = -IY

*      Process the attitude step, first check the accepted time file
*      to see if the attitude step is in an accepted time period
        ITIME = 1
        DO WHILE ( (DBLE(ISCS) .GT. IACTEN) .AND. (ITIME.LE.NACTGTIME) )
          IACTBE = GSTART(ITIME)
          IACTEN = GEND(ITIME)

*        Next good time slot
          ITIME = ITIME + 1

        END DO

*      If a good time, or no time slots supplied...
        IF ( (NACTGTIME.EQ.0) .OR.
     :       ((DBLE(ISCS).GE.IACTBE) .AND. (ITIME.LE.NACTGTIME)) ) THEN

*        Accepted time, now find the live time.
          NUM = NUM + 1
C          IF ( MOD(NUM,1000) .EQ. 0) THEN
C            CALL MSG_SETI( 'NUM', NUM )
C            CALL MSG_PRNT( '^NUM' )
C          END IF

*        Diagnostic routine
	  IF ( (DELT.GT.1) .AND. (IATT.GT.1) ) THEN
c	    WRITE(*,*) 'DELT ERROR'
c	    WRITE(*,*) DELT
c		WRITE(*,*) ISCS
c		WRITE(*,*) ISCSO
c		WRITE(*,*) IACTBE, IACTEN
c		WRITE(*,*) ITEMPBE, ITEMPEN
c		WRITE(*,*) 'HIT'
            DELT=1
	  END IF

*        First sort through the event rate file to find a close time
          DO WHILE ( (ISCS .GT. ILIVEN) .AND. (IEVR.LE.NEVRREC) )

C      ILIVEN  integer space craft clock seconds
C      IA1LL   A1LL scaler count rate
C      IAEXE   AEXE scaler count rate
C      IAXE    AXE scaler count rate
C      MV      MV (master veto) count rate

            CALL ARR_ELEM1I( EVR_T_PTR, NEVRREC, IEVR, ILIVEN, STATUS )
            CALL ARR_ELEM1I( EVR_A1LL_PTR, NEVRREC, IEVR, IA1LL,
     :                       STATUS )
            CALL ARR_ELEM1I( EVR_AEXE_PTR, NEVRREC, IEVR, IAEXE,
     :                       STATUS )
            CALL ARR_ELEM1I( EVR_AXE_PTR, NEVRREC, IEVR, IAXE, STATUS )
            CALL ARR_ELEM1I( EVR_MV_PTR, NEVRREC, IEVR, IMV, STATUS )

*          Next event rate record file
            IEVR = IEVR + 1

          END DO

*        Check to see if the detector was actually on
          IF ( IA1LL .GT. 10 ) THEN

*          Calculate the live time fraction.  This is a GRH routine
            CALL XRTEXPMAP_LIVTIM( REAL(IA1LL), DEADTP, REAL(IAXE),
     :                             REAL(IAEXE), FLIVE1, FLIVE2,
     :                             FLIVE, IERR )

*          Calculate the live time fraction with the additional 0.00005 sec
*          deadtime per acepted event
            FLIVE1 = 1.0 - 0.00005*REAL(IAEXE)

*          The attitude steps should be on 1-second intervals, calculate the
*          exposure
            EXP = DELT*FLIVE*FLIVE1
            TOTIME = TOTIME + DELT
            TOTEXP = TOTEXP + EXP

*          Sum the MV count rate to find an average value
            TMV = TMV + REAL(IMV)
            NMV = NMV + 1.0

*          Set the X, Y, and ROLL values for the aspect array.  X,Y Steps are
*          in units of 14.94733 arc seconds (historical reasons).  ROLL steps
*          are in units of 0.2076 degrees.
            X = 1000. + FLOAT(IX)/29.894656
            IX = INT(X) - 1000
            Y = 1000. + FLOAT(IY)/29.894656
            IY = INT(Y) - 1000
            ROLL = 20000.5 + FLOAT(IROLL)/1494.733
            IR = INT(ROLL) - 20000

*          Add to the aspect list
            NB1 = 1
            IF ( IA .GT. 0 ) THEN
              DO I=1,IA
                IF ( (IX .EQ. IASP(1,I)) .AND.
     :               (IY .EQ. IASP(2,I)) .AND.
     :               (IR .EQ. IASP(3,I))) THEN
                  ASP(I) = ASP(I) + EXP
                  NB1 = 0
                  GOTO 50
                END IF
              END DO
            END IF
 50         IF ( NB1 .EQ. 1 ) THEN
              IA = IA + 1
              IASP(1,IA) = IX
              IASP(2,IA) = IY
              IASP(3,IA) = IR
              ASP(IA) = EXP
            END IF
            NG = NG + 1
          ELSE
            NB = NB + 1
          END IF
        ENDIF

      END DO
*
*    Print out diagnostic information
      CALL MSG_SETI( 'LEN', IA )
      CALL MSG_SETI( 'NG', NG )
      CALL MSG_SETI( 'NB', NB )
      CALL MSG_PRNT( '^LEN distinct aspect bins, with ^NG good '/
     :               /'and ^NB bad attitude records' )
     :
      CALL MSG_SETR( 'TTIME', TOTIME )
      CALL MSG_SETR( 'TEXP', TOTEXP )
      CALL MSG_PRNT( 'Total on time ^TTIME s, total exposure ^TEXP s' )
      TMV = TMV/NMV
      CALL MSG_SETR( 'MV', TMV )
      CALL MSG_PRNT( 'Average master veto rate = ^MV ct/sec' )

*  Sort on roll angle
      DO I=IA-1,2,-1
        DO II=1,I
          IF(IASP(3,II) .GT. IASP(3,II+1)) THEN
            NB1 = IASP(1,II)
            NB2 = IASP(2,II)
            NB3 = IASP(3,II)
            A = ASP(II)
            IASP(1,II) = IASP(1,II+1)
            IASP(2,II) = IASP(2,II+1)
            IASP(3,II) = IASP(3,II+1)
            ASP(II) = ASP(II+1)
            IASP(1,II+1) = NB1
            IASP(2,II+1) = NB2
            IASP(3,II+1) = NB3
            ASP(II+1) = A
          END IF
        END DO
      END DO

*  Start the progress message
      CALL AIO_REWRITE( 0, 'NOCR', 'Progress : ', STATUS )

*  Now cast the exposure
      NB = IA/20
      NG = 0
      NB1 = -100000
      DO I=1,IA

*    Progress update
        IF(MOD(I,NB) .EQ. 0) THEN
          NG = NG + 1
          WRITE( LINE(1:5), '(I3,A)' ) NG*5, ' %'
          CALL AIO_REWRITE( 0, 'BACKSPACE', LINE(1:5), STATUS )
        END IF

*      Extract values from attitude bin
        IX = IASP(1,I)
        IY = IASP(2,I)
        IR = IASP(3,I)

            IF(IR .NE. NB1) THEN
C
C  First nonzero aspect point with this roll angle, make a rotated map
C
                ANGLE = IR*0.2076017
                COSROL = COS(ANGLE*MATH__DTOR)
                SINROL = SIN(ANGLE*MATH__DTOR)
C
C  Zero the temp array
C
                DO II=25,488
                    DO III=25,488
                        TEMPAR(II,III) = 0.0
                    ENDDO
                ENDDO
C
C  Calculate the rotated array only once for each roll angle
C
                DO II=25,488
                    DO III=25,488
                        IF(RMAP(II,III) .NE. 0.) THEN
                            X = (II - 257.)
                            Y = (III - 257.)
                            XX = COSROL*X + SINROL*Y
                            YY = COSROL*Y - SINROL*X
                            IXX = INT(XX + 257.5)
                            IYY = INT(YY + 257.5)
                            TEMPAR(IXX,IYY) =
     +                              TEMPAR(IXX,IYY) + RMAP(II,III)
                        ENDIF
                    ENDDO
                ENDDO
            ENDIF
            NB1 = IR
C
C  Nonzero element, cast it
C
            DO II=25,488
                IXX = II + IX
                DO III=25,488
                    IF(TEMPAR(II,III) .NE. 0.) THEN
                        IYY = III + IY
                        EXPARR(IXX,IYY) = EXPARR(IXX,IYY) +
     +                                  ASP(I)*TEMPAR(II,III)
                    ENDIF
                ENDDO
            ENDDO
        ENDDO

*  Terminate the rewrite
      CALL AIO_REWRITE( 0, 'TERMINATE', 'DONE ', STATUS )

*  Write exposure map
      CALL XRTEXPMAP_PUTIM( DIMS, PIXSIZE, EXPARR,
     :  'Exposure image', 'Exposure', 'seconds',
     :  OFID, STATUS )


*  Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END


*+  XRTEXPMAP_LIVTIM
        SUBROUTINE XRTEXPMAP_LIVTIM(A1LL,DEADTP,AXE,AEXE,
     +      FLIVE1,FLIVE2,FLIVE,IERR)
C
CC  Calculates PSPC livetime factor from Count Rates and Deadtime Param.
C
C************************ FFORM VERSION 1.2 ************ DD-MMM-YY HH:MM
C
CA  author : GRH        date: 13-MAR-1990 09:02
CU  update : GRH        date: 13-MAR-1990 10:02
C
CT  status: not tested
C
C   general description:
CG  The PSPC livetime factor, a value between 0 and 1) which has to be
CG  multiplied to the exposure time to obtain the effective live
CG  exposure time, is calculated from a product of two values:
CG
CG  FLIVE1 using the input A1-lower-level-discriminator count rate
CG  (A1LL) [cts/s] and the deadtime factor (DEADTP) [musec] according
CG  to the recipe in the TN-ROS-ME-ZA00-025. The deadtime parameter
CG  DEADTP, which is actually a function of mean energy and PSPC,
CG  should be specified from outside as a parameter.
CG
CG  FLIVE2 from the ratio between the accepted and evaluated X-ray
CG  event rate (AEXE) and the accepted X-ray event rate (AXE). A
CG  difference between those two indicates loss of events in the
CG  telemetry stream because of a busy readout.
C
C   call_var.          type I/O description
CP  A1LL                R4  I   EE-A1LL count rate from HK-data [cts/s]
CP  AXE                 R4  I   EE-AXE  count rate from HK-data [cts/s]
CP  AEXE                R4  I   EE-AEXE  count rate from HK-data [cts/s]
CP  DEADTP              R4  I   Deadtime Parameter (ca. 190-260 [musec])
CP  FLIVE1              R4    O PSPC Livetime Factor (between 0 and 1)
CP  FLIVE2              R4    O ER Livetime Factor (between 0 and 1)
CP  FLIVE               R4    O Ttotal Livetime Factor (between 0 and 1)
CP  IERR                I     O = 0 no error
CP                              = 1 negative square root ARG (FLIVE1=1)
CP                              = 1 denominator = 0 (FLIVE2=1)
CP                              = 3
C
C   include_block_name          description
CI  R$COMMON:CGENL.CMN          general parameter common block
C
C   routines_called    type     description
CR  HFLAG               R       output flag handling routine
CR  WRFLAG              SR      writes to FLAG output stream
C
C   extensions/system calls     description
CX
C
C***********************************************************************
C
C   variables   meaning
C   ARG    argument under square root
C
        IMPLICIT NONE
C
        REAL        A1LL,AXE,AEXE,ARG,DEADTP,FLIVE1,FLIVE2,FLIVE
        INTEGER     IERR
C
        IERR = 0

*    First: calculate PSPC livetime FLIVE1
      ARG = 2.0E-6 * A1LL * DEADTP

*    Check for error condition
      IF((ARG .LT. 0) .OR. (ARG .GT.1)) THEN
        IERR = 1
        FLIVE1 = 1.
      ELSE
        FLIVE1=SQRT(1.0-ARG)
      END IF

*    Second: calculate ER livetime FLIVE2
*    check for error condition
      IF ( AXE .EQ. 0 ) THEN
        IERR = 1
        FLIVE2 = 1.0
      ELSE
        FLIVE2=AEXE/AXE
      END IF

*    Third: multiply the two values
      FLIVE = FLIVE1 * FLIVE2

      END

      SUBROUTINE XRTEXPMAP_RESCALE( N, SCALED, ORIGINAL, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER N,ORIGINAL(*),STATUS,I
      REAL SCALED(*)
      IF ( STATUS .EQ. SAI__OK ) THEN
        DO I = 1,N
          ORIGINAL(I) = NINT(SCALED(I)*7200.0)
        END DO
      END IF

      END

      SUBROUTINE XRTEXPMAP_PUTIM( DIMS, PIXSIZ, DATA, TITLE, LABEL,
     :                            UNITS, OFID, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      CHARACTER*(*) TITLE,LABEL,UNITS
      INTEGER       DIMS(2), OFID
      REAL          PIXSIZ, DATA(*)
      INTEGER       STATUS
      REAL SPARR(2)

*  Create interface object
      CALL BDI_LINK( 'XYimage', 2, DIMS, 'REAL', OFID, STATUS )

*  Write regular axes
      SPARR(1) = (REAL(DIMS(1)/2)-0.5)*PIXSIZ/3600.0
      SPARR(2) = -PIXSIZ/3600.0
      CALL BDI_AXPUT1R( OFID, 1, 'SpacedData', 2, SPARR, STATUS )
      SPARR(1) = -(REAL(DIMS(2)/2)-0.5)*PIXSIZ/3600.0
      SPARR(2) = PIXSIZ/3600.0
      CALL BDI_AXPUT1R( OFID, 2, 'SpacedData', 2, SPARR, STATUS )
      CALL BDI_AXPUT0C( OFID, 1, 'Label', 'X position', STATUS )
      CALL BDI_AXPUT0C( OFID, 1, 'Units', 'degrees', STATUS )
      CALL BDI_AXPUT0C( OFID, 2, 'Label', 'Y position', STATUS )
      CALL BDI_AXPUT0C( OFID, 2, 'Units', 'degrees', STATUS )

      CALL BDI_PUT0C( OFID, 'Label', LABEL, STATUS )
      CALL BDI_PUT0C( OFID, 'Units', UNITS, STATUS )
      CALL BDI_PUT0C( OFID, 'Title', TITLE, STATUS )
      CALL BDI_AXPUT0L( OFID, 1, 'Normalised', .TRUE., STATUS )
      CALL BDI_AXPUT0L( OFID, 2, 'Normalised', .TRUE., STATUS )

*  Write data
      CALL BDI_PUT( OFID, 'Data', 'REAL', 2, DIMS, DATA, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'XRTEXPMAP_PUTIM', STATUS )
      END IF

      END
