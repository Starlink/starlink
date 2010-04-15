      SUBROUTINE IRCAM3LIST
C
C     03-May-1994: Original release (jach::kevin)
C     23-Sep-1994: port to unix
C
C     I R C A M 3 L I S T
C
C     Figaro command to list information from IRCAM data files
C     in a directory
C
C     Command Parameters -
C
C     FILENAME  A file specification (which can include wildcards) for
C               the files to be listed. The .DST or .SDF extension
C               must be specified
C
C     LISTING   The name of the text file in which the information
C               will be listed
C
C     Command Keywords -
C
C
C     FULL      By default 80 character listing records are produced.
C               If FULL is specified 132 character records containing
C               additional information are output.
C
C+
      IMPLICIT NONE
C
C     Dynamic memory include file - defines DYNAMIC_MEM
C
      INCLUDE 'DYNAMIC_MEMORY'
C
C     Local variables
C
      CHARACTER*1       ACCESS     ! ACCESS argument for SEEK_FITS
      REAL              AIRMASS    ! Airmass at start of observation
      CHARACTER*64      COMMENT    ! COMMENT argument for GET_FITS routines
      INTEGER           ELEMENTS   ! ELEMENTS argument for GET_FITS routines
      CHARACTER*22      DATE       ! Date of observation
      INTEGER           DCOLUMNS   ! Number of detector in readout column
      INTEGER           DROWS      ! Number of detector in readout row
      CHARACTER*8       DET        ! Detector name
      REAL              DIFF
      CHARACTER         EQ	   ! # for 1950.0, @ for 2000.0
      LOGICAL           EXISTS     ! TRUE if item exists
      REAL              EXP        ! Exposure time
      INTEGER           FD         ! field descriptor for fio calls
      CHARACTER*6       FIL        ! Filter name
      CHARACTER*132     FILENAME   ! Wild Card Filename
      LOGICAL           FULL       ! Produce full (132 character) listing
      INTEGER           GROUP      ! Group number
      CHARACTER*132     HEADER     ! Header string for output file
      CHARACTER*80      HEAD1      ! First part of column heads
      INTEGER           IDEC(4)    ! Declination in DMSF
      INTEGER           IGNORE     ! Ignoreable status
      CHARACTER*8       INST       ! Instrument name (IRCAM)
      INTEGER           IRA(4)     ! Right ascension in HMSF
      CHARACTER*80      LISTFILE   ! Name of listing file
      CHARACTER*3       MAGNIFIE   ! Magnifier
      REAL              MDEC       ! Mean Dec
      REAL              MEQNX      ! Equinox of mean coordinates
      REAL              MRA        ! Mean RA
      CHARACTER*8       MODE       ! IRACS mode
      INTEGER           NEXP       ! Number of exposures
      CHARACTER*32      OBJECT     ! Object name
      INTEGER           OBS        ! observation number
      CHARACTER*132     OBSREC     ! record to output
      CHARACTER*8       OBSTYPE    ! Type of observation
      INTEGER           OPEN_STATUS  ! open status for fio_open
      LOGICAL           OTHER	   ! if equinox other than 1950 or 2000
      CHARACTER*6       PATT       ! PATT reference number
      REAL              PIXELSIZ   ! pixel size (microns)
      REAL              RUT        ! UT of start of observation
      CHARACTER*1       SIGN       ! Sign of declination
      INTEGER           STATUS     ! Status value
      INTEGER           STRLEN     ! Length of string
      INTEGER           UTH        ! UT hours
      INTEGER           UTM        ! UT minutes
      INTEGER           UTS        ! UT seconds
C
C     Initialize DSA system
C
      STATUS=0
      IGNORE=0
      CALL DSA_OPEN(STATUS)
C
C     Get File name
C
      CALL PAR_RDCHAR('FILENAME',' ',FILENAME)
C
C     Get output file name
C
      CALL PAR_RDCHAR('LISTING',' ',LISTFILE)
C
C     Get FULL keyword
C
      CALL PAR_RDKEY('FULL',' ',FULL)
C
C     Open the listing file
C
      CALL FIO_OPEN (LISTFILE, 'APPEND', 'LIST', 0, FD, OPEN_STATUS )
C
C     Open the file
C
              CALL DSA_NAMED_INPUT('INPUT',FILENAME,STATUS)
              IF (STATUS .NE. 0) THEN
                  CALL PAR_WRUSER('Error opening '//FILENAME,IGNORE)
                  STATUS = 0
              ELSE
C
C     Check that the OBSNUM item exists - If not assume the
C     file is a group and use GRPNUM instead
C
                  CALL DSA_SEEK_FITS('INPUT','OBSNUM',EXISTS,ACCESS,
     :                ELEMENTS,STRLEN,STATUS)
                  CALL DSA_GET_FITS_I('INPUT','GRPNUM',1,GROUP,
     :                COMMENT,STATUS)
                  IF (EXISTS) THEN
                     CALL DSA_GET_FITS_I('INPUT','OBSNUM',1,OBS,
     :                COMMENT,STATUS)
                  ELSE
                      OBS = GROUP
                  ENDIF

                  TYPE *, obs

                  IF (OBS .EQ. 1) THEN
C
C     If it is the first file get the information required for
C     the header of the listing file. It is assumed that the date
C     etc. obtained from the first file applies to all of them, though
C     this is not checked
C
                     CALL DSA_GET_FITS_C('INPUT','UTDATE',1,DATE,
     :                COMMENT,STATUS)
                     CALL DSA_GET_FITS_C('INPUT','INSTRUME',1,INST,
     :                COMMENT,STATUS)
                     CALL DSA_GET_FITS_C('INPUT','DETECTOR',1,DET,
     :                COMMENT,STATUS)
                     CALL DSA_GET_FITS_C('INPUT','OBSREF',1,PATT,
     :                COMMENT,STATUS)
                     HEADER = ' UT Date: '//DATE//
     :                 ' Instr: '//INST//' Array: '//DET//
     :                '  PATT ref: '//PATT
                     CALL FIO_WRITE ( FD, HEADER, STATUS )
C
C     Output column headings
C
                     HEADER = ' All images are co-added in ALICE'//
     :                        ' to Nexp=1'
                     CALL FIO_WRITE ( FD, HEADER, STATUS )

                         HEAD1 = '  Obs Object      Obstype  UT start'//
     :                  '     Exp.    Nexp       Mode   Filter'//
     :                  '   Armss'
                     IF (FULL) THEN
                        HEADER = HEAD1//' Cols Rows  Mag Psize'//
     :	                  '    R.A.          Dec     Eqnx'
                        CALL FIO_WRITE ( FD, HEADER, STATUS )
                     ELSE
                        CALL FIO_WRITE ( FD, HEAD1, STATUS )
                     ENDIF
                  ENDIF
C
C     Get remaining items from FITS structure
C
                  CALL DSA_GET_FITS_C('INPUT','OBJECT',1,OBJECT,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','OBSTYPE',1,OBSTYPE,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_F('INPUT','RUTSTART',1,RUT,
     :                COMMENT,STATUS)
                  UTH=INT(RUT)
                  UTM=INT((RUT-REAL(UTH))*60.0)
                  UTS=INT((RUT-REAL(UTH)-REAL(UTM)/60.0)*3600.0)
                  CALL DSA_GET_FITS_F('INPUT','DEXPTIME',1,EXP,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_I('INPUT','NEXP',1,NEXP,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','MODE',1,MODE,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','FILTER',1,FIL,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_F('INPUT','AMSTART',1,AIRMASS,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_I('INPUT','DCOLUMNS',1,DCOLUMNS,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_I('INPUT','DROWS',1,DROWS,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','MAGNIFIE',1,MAGNIFIE,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_F('INPUT','PIXELSIZ',1,PIXELSIZ,
     :                COMMENT,STATUS)
C
C     These items are needed for the FULL mode
C
                  IF (FULL) THEN
                     CALL DSA_GET_FITS_F('INPUT','MEANRA',1,MRA,
     :                COMMENT,STATUS)
                     MRA=MRA*3.1415926/12.0
                     CALL DSA_GET_FITS_F('INPUT','MEANDEC',1,MDEC,
     :                COMMENT,STATUS)
                     MDEC=MDEC*3.1415926/180.0
                     CALL DSA_GET_FITS_F('INPUT','EQUINOX',1,MEQNX,
     :                COMMENT,STATUS)
                  ENDIF
C
C     Get additional items needed for FULL listing
C
                  IF (FULL) THEN
                     CALL SLA_CR2TF(1,MRA,SIGN,IRA)
                     CALL SLA_CR2AF(1,MDEC,SIGN,IDEC)
                  ENDIF
C
C    Write results into observation record
C
                   WRITE(OBSREC,'('' '',I4,'' '',A12,
     :              A8,'' '',I2.2,
     :            '':'',I2.2,'':'',I2.2,''  '',F7.3,''   '',I5,''   '',
     :              A8,''   '',A6,''   '',F5.3,''   '',I3,''x'',I3,
     :              ''  '',A3,'' '',F5.3)')
     :              OBS,OBJECT,OBSTYPE,UTH,UTM,UTS,EXP,NEXP,
     :              MODE,FIL,AIRMASS,DCOLUMNS,DROWS,MAGNIFIE,PIXELSIZ

                  OTHER = .TRUE.
                  DIFF = ABS (MEQNX - 1950.0)
                  IF ( DIFF .LT. 0.1 ) THEN
                    EQ = '#'
                    OTHER = .FALSE.
                  ENDIF
                  DIFF = ABS (MEQNX - 2000.0)
                  IF ( DIFF .LT. 0.1 ) THEN
                    EQ = '@'
                    OTHER = .FALSE.
                  ENDIF

                  IF (FULL) THEN
                     IF ( .NOT. OTHER ) THEN
                       WRITE(OBSREC(102:132),'(
     :                  '' '',I2.2,'':'',I2.2,'':'',I2.2,''.'',I2.2,
     :                  ''  '',A1,I2.2,'':'',I2.2,'':'',I2.2,''.'',I1,
     :                  ''    '',A)')
     :                    IRA(1),IRA(2),IRA(3),IRA(4),
     :                    SIGN,IDEC(1),IDEC(2),IDEC(3),IDEC(4),EQ
                     ENDIF

                     IF ( OTHER ) THEN
                       WRITE(OBSREC(102:132),'(
     :                  '' '',I2.2,'':'',I2.2,'':'',I2.2,''.'',I2.2,
     :                  ''  '',A1,I2.2,'':'',I2.2,'':'',I2.2,''.'',I1,
     :                  '' '',I4.4)')
     :                    IRA(1),IRA(2),IRA(3),IRA(4),
     :                    SIGN,IDEC(1),IDEC(2),IDEC(3),IDEC(4),
     :                    NINT(MEQNX)
                     ENDIF
                    IF ( OBS .EQ. 1 ) THEN
                      HEADER = ' # = 1950.0   @ = 2000.0'
                      CALL FIO_WRITE ( FD, HEADER, STATUS )
                    ENDIF
                  ENDIF
                  STATUS=0
                  CALL DSA_CLOSE_STRUCTURE('INPUT',STATUS)

              ENDIF
C
C     Output the listing record
C
       IF (FULL) THEN
          CALL FIO_WRITE ( FD, OBSREC, STATUS )
       ELSE
          CALL FIO_WRITE ( FD, OBSREC(1:80), STATUS )
       ENDIF
C
C     Close down DSA - this will close the listing file
C
500   CONTINUE
      CALL FIO_CLOSE ( FD, STATUS )
      CALL DSA_CLOSE(STATUS)
C
      END


