      SUBROUTINE CGS4LIST
C+
C    03-May-1991 : original working version
C    22-Sep-1994 : port to unix (open only one CGS4 data file per call)
C
C    Note: the records must be sorted by a separate program
C
C     C G S 4 L I S T
C
C     Figaro command to extract information from CGS4 data files
C     in a directory
C
C     Command Parameters -
C
C     FILENAME  A file specification (which can include wildcards) for
C               the files to be listed. The .DST or .SDF extension
C               must be specified.  This is set by the environmental
C               variable FIGARO_FORMATS.
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
C     Constants
C
C     Local variables
C
      CHARACTER*1       ACCESS     ! ACCESS argument for SEEK_FITS
      REAL              AIRMASS    ! Airmass at start of observation
      CHARACTER*64      COMMENT    ! COMMENT argument for GET_FITS routines
      INTEGER           ELEMENTS   ! ELEMENTS argument for GET_FITS routines
      CHARACTER*25      DATE       ! Date of observation
      CHARACTER*8       DET        ! Detector name
      LOGICAL           EXISTS     ! TRUE if item exists
      REAL              EXP        ! Exposure time
      INTEGER           FD         ! field descriptor for fio calls
      CHARACTER*6       FIL        ! Filter name
      CHARACTER*132     FILENAME   ! Wild Card Filename
      LOGICAL           FULL       ! Produce full (132 character) listing
      CHARACTER*8       GRAT       ! Grating name
      INTEGER           GROUP      ! Group number
      CHARACTER*132     HEADER     ! Header string for output file
      CHARACTER*80      HEAD1      ! First part of column heads
      INTEGER           IDEC(4)    ! Declination in DMSF
      INTEGER           IGNORE     ! Ignorable status
      INTEGER           IRA(4)     ! Right ascension in HMSF
      REAL              LAMBDA     ! Grating wavelength
      CHARACTER*80      LISTFILE   ! Name of listing file
      REAL              MDEC       ! Mean Dec
      REAL              MEQNX      ! Equinox of mean coordinates
      REAL              MRA        ! Mean RA
      CHARACTER*7       MODE       ! IRACS mode
      CHARACTER*32      OBJECT     ! Object name
      INTEGER           OBS        ! observation number
      CHARACTER*132     OBSREC     ! record to output
      INTEGER           OPEN_STATUS  ! FIO_OPEN status
      INTEGER           ORDER      ! Grating order
      CHARACTER*8       PATT       ! PATT reference number
      REAL              RUT        ! UT of start of observation
      CHARACTER*1       SIGN       ! Sign of declination
      CHARACTER*10      SLIT       ! Slit name
      INTEGER           STATUS     ! Status value
      INTEGER           STRLEN     ! Length of string
      INTEGER           UTH        ! UT hours
      INTEGER           UTM        ! UT minutes
      INTEGER           UTS        ! UT seconds
C
C     Integer functions
C
      INTEGER ICH_LEN
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
C     Extract data from one file.  If it's the first time calling this
C       routine, put out the header information
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
                      OBS=GROUP
                  ENDIF

                  IF (OBS .EQ. 1) THEN
C
C     If it is the first file get the information required for
C     the header of the listing file. It is assumed that the date
C     etc. obtained from the first file applies to all of them, though
C     this is not checked
C
                     CALL DSA_GET_FITS_C('INPUT','UTDATE',1,DATE,
     :                COMMENT,STATUS)
                     CALL DSA_GET_FITS_C('INPUT','OBSREF',1,PATT,
     :                COMMENT,STATUS)
                     CALL DSA_GET_FITS_C('INPUT','DETECTOR',1,DET,
     :                COMMENT,STATUS)
                     HEADER = '  UT Date: '//DATE(:ICH_LEN(DATE))/
     :                /' Array: '//DET//
     :                '  PATT ref: '//PATT
                     CALL FIO_WRITE ( FD, HEADER, STATUS )
C
C     Output column headings
C
                     CALL FIO_WRITE ( FD, ' ', STATUS )
                         HEAD1 =
     :                  ' Obs  Object      UT start'//
     :                  ' Exptime Grating  Lambda Filter Mode '//
     :                  ' Airmass Group   '
                     IF (FULL) THEN
                        HEADER = HEAD1//' Ord Slit   '//
     :                   '     R.A.          Dec        Eqnx      '
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
                  CALL DSA_GET_FITS_F('INPUT','RUTSTART',1,RUT,
     :                COMMENT,STATUS)
                  UTH=INT(RUT)
                  UTM=INT((RUT-REAL(UTH))*60.0)
                  UTS=INT((RUT-REAL(UTH)-REAL(UTM)/60.0)*3600.0)
                  CALL DSA_GET_FITS_F('INPUT','DEXPTIME',1,EXP,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','GRATING',1,GRAT,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_F('INPUT','GLAMBDA',1,LAMBDA,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','FILTERS',1,FIL,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_C('INPUT','INTTYPE',1,MODE,
     :                COMMENT,STATUS)
                  CALL DSA_GET_FITS_F('INPUT','AMSTART',1,AIRMASS,
     :                COMMENT,STATUS)
C
C     These items are needed for FULL modes
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
                     CALL DSA_GET_FITS_I('INPUT','GORDER',1,ORDER,
     :                COMMENT,STATUS)
                     CALL DSA_GET_FITS_C('INPUT','SLIT',1,SLIT,
     :                COMMENT,STATUS)
                     CALL SLA_CR2TF(1,MRA,SIGN,IRA)
                     CALL SLA_CR2AF(1,MDEC,SIGN,IDEC)
                  ENDIF
C
C    Write results into observation record
C
                     WRITE(OBSREC,'(I4,'' '',A12,'' '',I2.2,
     :                '':'',I2.2,'':'',I2.2,'' '',F7.3,'' '',A8,'' '',
     :                F6.4,'' '',A6,A7,'' '',F5.3'' '',I5)')
     :                OBS,OBJECT,UTH,UTM,UTS,EXP,GRAT,LAMBDA,
     :                FIL,MODE,AIRMASS,GROUP

                  IF (FULL) THEN
                     WRITE(OBSREC(81:132),'(I3,'' '',A10,
     :                '' '',I2.2,'':'',I2.2,'':'',I2.2,''.'',I2.2,
     :                ''  '',A1,I2.2,'':'',I2.2,'':'',I2.2,''.'',I1,
     :                ''  '',F7.1)')
     :                  ORDER,SLIT,IRA(1),IRA(2),IRA(3),IRA(4),
     :                  SIGN,IDEC(1),IDEC(2),IDEC(3),IDEC(4),MEQNX
                  ENDIF
                  STATUS=0
                  CALL DSA_CLOSE_STRUCTURE('INPUT',STATUS)
              ENDIF
C
C     Output the listing file
C
      IF (FULL) THEN
         CALL FIO_WRITE (FD, OBSREC, STATUS )
      ELSE
         CALL FIO_WRITE (FD, OBSREC(1:80), STATUS )
      ENDIF
C
500   CONTINUE
      CALL FIO_CLOSE ( FD, STATUS )
      CALL DSA_CLOSE(STATUS)
C
      END
