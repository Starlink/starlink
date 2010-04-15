      SUBROUTINE CGS3LIST
C+
C
C     C G S 3 L I S T
C
C     Figaro command to list information from CGS3 data files
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
C     Author: Phil Daly, JAC, but unashamedly ripped off from CGS4LIST
C     26-Sep-1994: port to unix (KLK)
C+
      IMPLICIT NONE
C
C     Constants
      REAL PI
      PARAMETER(PI=3.1415926)
C
C     Local variables
      CHARACTER*1       CEQNX      ! Character equinox string
      CHARACTER*1       SIGN       ! Sign of declination
      CHARACTER*6       FIL        ! Filter name
      CHARACTER*6       DET        ! Detector name
      CHARACTER*6       TELE       ! The telescope
      CHARACTER*8       GRAT       ! Grating name
      CHARACTER*8       RUT        ! UT of start of observation
      CHARACTER*25      DATE       ! Date of observation
      CHARACTER*16      OBJECT     ! Object name
      CHARACTER*64      COMMENT    ! COMMENT argument for GET_FITS routines
      CHARACTER*80      HEADER     ! Header string for output file
      CHARACTER*80      HEAD1      ! First part of column heads
      CHARACTER*80      LISTFILE   ! Name of listing file
      CHARACTER*132     FILENAME   ! Wild Card Filename
      CHARACTER*132     OBSREC     ! record to output
      INTEGER           C00        ! Counter for J2000 epoch
      INTEGER           C25        ! Counter for unknown epoch
      INTEGER           C50        ! Counter for J1950 epoch
      INTEGER           C75        ! Counter for J1975 epoch
      INTEGER           FD         ! field descriptor for fio_ calls
      INTEGER           IDEC(4)    ! Mean Dec (HMSF)
      INTEGER           IGNORE     ! Ignoreable status
      INTEGER           IRA(4)     ! Mean RA  (HMSF)
      INTEGER           OBS        ! observation number
      INTEGER           OFFSET     ! offset within character string
      INTEGER           STATUS     ! Status value
      REAL              AIRMASS    ! Airmass at start of observation
      REAL              EXP        ! Exposure time
      REAL              FEQNX      ! Equinox of first mean coordinate
      REAL              MDEC       ! Mean Dec
      REAL              MEQNX      ! Equinox of mean coordinates
      REAL              MRA        ! Mean RA
C
C     Initialize DSA system
      STATUS=0
      IGNORE=0
      CALL DSA_OPEN(STATUS)
      C00=0
      C25=0
      C50=0
      C75=0
C
C     Get input and output file name(s)
      CALL PAR_RDCHAR('FILENAME',' ',FILENAME)
      CALL PAR_RDCHAR('LISTING',' ',LISTFILE)
C
C     Open the listing file
      CALL FIO_OPEN(LISTFILE, 'APPEND', 'LIST', 0, FD, STATUS)
C
C     Open the file
      CALL DSA_NAMED_INPUT('INPUT',FILENAME,STATUS)
      IF (STATUS .NE. 0) THEN
         CALL PAR_WRUSER('Error opening '//FILENAME,IGNORE)
         STATUS = 0
      ELSE
C
         CALL DSA_GET_FITS_I('INPUT','RUN',1,OBS,COMMENT,STATUS)

         IF ( OBS .LE. 0 ) THEN
            STATUS = 0
            GOTO 500
         ENDIF

         IF (OBS .EQ. 1) THEN
C
            HEAD1 = ' Equinoxes detected: '/
     :         /'(#)=1950.0, (+)=1975.0, (*)=2000.0, (?)=Check'
            CALL FIO_WRITE ( FD, HEAD1, STATUS )
            CALL FIO_WRITE ( FD, ' ', STATUS )

C     Get some header information (assume equinox is same for all obs)
            CALL DSA_GET_FITS_C('INPUT','UTDATE',1,DATE,COMMENT,STATUS)
            CALL DSA_GET_FITS_C('INPUT','TELESCOP',1,TELE,COMMENT,STATUS)
            CALL DSA_GET_FITS_C('INPUT','INSTRUME',1,DET,COMMENT,STATUS)
            CALL DSA_GET_FITS_F('INPUT','EQUINOX',1,FEQNX,COMMENT,STATUS)
            HEADER = '  UT Date: '//DATE//'   Instrument: '//DET/
     :         /'  Telescope: '//TELE//'  Equinox: '
            WRITE (OBSREC,'(A72,F6.1)') HEADER,FEQNX
            CALL FIO_WRITE(FD, OBSREC, STATUS)
            CALL FIO_WRITE(FD, ' ', STATUS)
C
C     Output a heading
            HEAD1 = ' Obs  Object       UTstart    Exp    '/
     :         /'Grating  Filter Airmass    RA       Dec    '
            CALL FIO_WRITE ( FD, HEAD1, STATUS )
         ENDIF
C
C     Get THE FITS items
         CALL DSA_GET_FITS_C('INPUT','OBJECT',1,OBJECT,COMMENT,STATUS)
         CALL DSA_GET_FITS_C('INPUT','UTSTART',1,RUT,COMMENT,STATUS)
         OFFSET = INDEX( OBJECT, 'SKY' )
         IF ( OFFSET .GT. 0 ) THEN
            CALL DSA_GET_FITS_F('INPUT','C3SKYDWL',1,EXP,COMMENT,STATUS)
         ELSE
            CALL DSA_GET_FITS_F('INPUT','C3STRDWL',1,EXP,COMMENT,STATUS)
         ENDIF
         CALL DSA_GET_FITS_C('INPUT','C3GRAT',1,GRAT,COMMENT,STATUS)
         CALL DSA_GET_FITS_C('INPUT','C3FILT',1,FIL,COMMENT,STATUS)
         CALL DSA_GET_FITS_F('INPUT','AMSTART',1,AIRMASS,COMMENT,STATUS)
         CALL DSA_GET_FITS_F('INPUT','MEANRA',1,MRA,COMMENT,STATUS)
         MRA=MRA*PI/180.0
         CALL SLA_CR2TF(1,MRA,SIGN,IRA)
         CALL DSA_GET_FITS_F('INPUT','MEANDEC',1,MDEC,COMMENT,STATUS)
         MDEC=MDEC*PI/180.0
         CALL SLA_CR2AF(1,MDEC,SIGN,IDEC)
         CALL DSA_GET_FITS_F('INPUT','EQUINOX',1,MEQNX,COMMENT,STATUS)
         IF ( MEQNX .EQ. FEQNX  ) THEN
            CEQNX = ' '
         ELSE
            IF ( MEQNX .EQ. 1950.0 ) THEN
               CEQNX = '#'
               C50 = C50 + 1
            ELSE IF ( MEQNX .EQ. 1975.0 ) THEN
               CEQNX = '+'
               C75 = C75 + 1
            ELSE IF ( MEQNX .EQ. 2000.0 ) THEN
               CEQNX = '*'
               C00 = C00 + 1
            ELSE
               CEQNX = '?'
               C25 = C25 + 1
            ENDIF
         ENDIF

C
C    Write results into observation record
         WRITE(OBSREC,'(T1,I4,T7,A12,T20,A8,T28,F7.3,T38,A8,
     :      T47,A6,T55,F5.3,T62,I2.2,T64,A1,T65,I2.2,T67,A1,
     :      T68,I2.2,T72,A1,T73,I2.2,T75,A1,T76,I2.2,T79,A1)')
     :      OBS,OBJECT,RUT,EXP,GRAT,FIL,AIRMASS,
     :      IRA(1),':',IRA(2),':',IRA(3),SIGN,IDEC(1),
     :      ' ',IDEC(2),CEQNX
         STATUS=0
         CALL DSA_CLOSE_STRUCTURE('INPUT',STATUS)
      ENDIF
C
C     Output to the listing file
C
      CALL FIO_WRITE ( FD, OBSREC(1:80), STATUS )
C
C     Close down DSA - this will close the listing file
C
500   CONTINUE
      CALL FIO_CLOSE(FD,STATUS)
      CALL DSA_CLOSE(STATUS)
C
      END
