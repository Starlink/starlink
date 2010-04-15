C+
      SUBROUTINE RHDSPLOT(STATUS)
C
C            R H D S P L O T
C
C     Command name:
C        RHDSPLOT
C
C     Function:
C        Read ASCII files of Hatfield Polarimeter Data.
C
C     Description:
C        RHDSPLOT reads ASCII files created by Tim Peacock's HDSPLOT program
C        from raw Hatfield Polarimeter Data. It outputs time series datasets
C        with either 3 or 6 wavelengths channels, depending on which version
C        of the polarimeter the data came from.
C
C        This command is superseded by RHATPOL which can reduce Hatfield
C        polarimetry data directly from the raw data files.
C
C     Parameters:
C        FILENAME   (Char)    - The name of the HDSPLOT file to be read.
C        NCHANS     (Integer) - The number of wavelength channels.
C        OUTPUT     (TSP, 2D) - The output dataset to be created.
C        CHANNEL    (Char)    - Name of channel.
C        WAVELENGTH (Real)    - Wavelength of channel.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         27/2/1988
C
C-
C
C  History:
C    Nov/1987   Original Version.   JAB/AAO
C    26/2/1988   TSP Monolith version.  JAB/AAO
C


      IMPLICIT NONE

*  Status argument
      INTEGER STATUS
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Maximum number of data points
      INTEGER MAXLEN
      PARAMETER (MAXLEN = 1000)

*  Input filename
      CHARACTER*80 FILENAME
      CHARACTER*80 NAME

*  Number of channels
      INTEGER NCHANS
      INTEGER LEN

*  IO status
      INTEGER IOS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,SLOC,DLOC,VLOC

*  data arrays to receive polarization data from input file
      REAL I(6,MAXLEN), Q(6,MAXLEN), U(6,MAXLEN), V(6,MAXLEN)
      REAL IE(6,MAXLEN), QE(6,MAXLEN), UE(6,MAXLEN), VE(6,MAXLEN)

*  Wavelength of each channel and its fluz zero point
      REAL WAVES(6),ZEROPT(6)

*  Time (MJD) array
      DOUBLE PRECISION TIMES(MAXLEN)
      INTEGER NS,NE
      INTEGER BIN

*  Object name and UT date
      CHARACTER*80 OBJECT,UTDATE
      CHARACTER*132 BUFFER
      DOUBLE PRECISION DJD
      INTEGER CHAN
      INTEGER START,FINISH
      INTEGER DPTR,VPTR
      INTEGER DIMS(2)
      INTEGER J,K
      LOGICAL FIRST
      REAL D(6),E(6)
      REAL QP,UP

*  ICH functions
      INTEGER ICH_VERIF,ICH_DELIM,ICH_LEN

*  Degree to radians
      REAL DEGRAD
      DEGRAD = 45.0/ATAN(1.0)

*  Get the name of the HDSPLOT file

      CALL PAR_GET0C('FILENAME',FILENAME,STATUS)
      LEN = ICH_LEN(FILENAME)
      IF (STATUS .EQ. SAI__OK) THEN

*  Open the file

         OPEN(UNIT=10,FILE=FILENAME(1:LEN),STATUS='OLD',
     :      IOSTAT=IOS)
         IF (IOS .NE. 0) THEN
            CALL MSG_OUT('MSG','Cannot open file',STATUS)
            STATUS = USER__001
         ENDIF

*  Get number of channels

         CALL PAR_GET0I('NCHANS',NCHANS,STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            IF ((NCHANS .GT. 6) .OR. (NCHANS .LT. 1)) THEN
               CALL MSG_OUT('MSG','Invalid number of channels',STATUS)
               STATUS = USER__001
            ENDIF
         ENDIF

*  Get Output HDS File

         CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
         CALL DAT_ASSOC('OUTPUT','WRITE',LOC,STATUS)

         DO J=1,6
            WAVES(J)=0.0
         ENDDO

         BIN = 1
         IOS = 0
         FIRST = .TRUE.
         DO WHILE (IOS .EQ. 0)

            READ(10,'(A)',IOSTAT=IOS) BUFFER
            IF (IOS .EQ. 0) THEN
               IF (FIRST) THEN
                  IF (BUFFER(1:8) .EQ. 'OBJECT  ') THEN
                     START = ICH_VERIF(BUFFER,10,' ''')
                     FINISH = ICH_DELIM(BUFFER,START,'''')
                     IF (FINISH .NE. 0) THEN
                         OBJECT = BUFFER(START:FINISH-1)
                     ELSE
                         OBJECT = BUFFER(12:29)
                     ENDIF
                  ELSE IF (BUFFER(1:8) .EQ. 'IRPS_FIL') THEN
                     IF (BUFFER(11:11) .EQ. 'K') THEN
                        WAVES(1)=22000
                        ZEROPT(1)=650
                     ELSE IF (BUFFER(11:11) .EQ. 'H') THEN
                        WAVES(1)=16400
                        ZEROPT(1)=1030
                     ELSE IF (BUFFER(11:11) .EQ. 'J') THEN
                        WAVES(1)=12000
                        ZEROPT(1)=1640
                     ELSE IF (BUFFER(11:11) .EQ. 'L') THEN
                        WAVES(1)=38000
                        ZEROPT(1)=250
                     ELSE IF (BUFFER(11:11) .EQ. 'M') THEN
                        WAVES(1)=48000
                        ZEROPT(1)=2000
                     ENDIF
                  ELSE IF (BUFFER(1:8) .EQ. 'RED_FIL') THEN
                     IF (BUFFER(11:11) .EQ. 'I') THEN
                        WAVES(2)=7900
                        ZEROPT(2)=2550
                     ELSE IF (BUFFER(11:11) .EQ. 'R') THEN
                        WAVES(2)=6400
                        ZEROPT(2)=3080
                     ELSE IF (BUFFER(11:11) .EQ. 'V') THEN
                        WAVES(2)=5500
                        ZEROPT(2)=3640
                     ELSE IF (BUFFER(11:11) .EQ. 'W') THEN
                        WAVES(2)=6500
                        ZEROPT(2)=2000
                     ENDIF
                  ELSE IF (BUFFER(1:8) .EQ. 'BLUE_FIL') THEN
                     IF (BUFFER(11:11) .EQ. 'B') THEN
                        WAVES(3)=4400
                        ZEROPT(3)=4260
                     ELSE IF (BUFFER(11:11) .EQ. 'U') THEN
                        WAVES(3)=3600
                        ZEROPT(3)=1810
                     ELSE IF (BUFFER(11:11) .EQ. 'C') THEN
                        WAVES(3)=4000
                        ZEROPT(3)=2000
                     ENDIF
                  ELSE IF (BUFFER(1:8) .EQ. 'UTDATE') THEN
                     UTDATE = BUFFER(12:21)
                  ENDIF
               ENDIF

               IF (BUFFER(1:8) .EQ. 'JUL_DATE') THEN
                  CALL ICH_NUMBD(BUFFER,10,' ',DJD,NS,NE)
                  TIMES(BIN) = DJD + 39999.5              ! Convert to MJD
               ELSE IF ((BUFFER(9:9) .NE. '=') .AND.
     :               (BUFFER(1:8) .NE. '        ')) THEN

*  Data lines reached

                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  CALL TSP_EXTRACT(NCHANS,BUFFER,D,E)
                  DO CHAN=1,NCHANS
                     I(CHAN,BIN)=D(CHAN)
                     IE(CHAN,BIN)=E(CHAN)
                  ENDDO
                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  CALL TSP_EXTRACT(NCHANS,BUFFER,D,E)
                  DO CHAN=1,NCHANS
                     Q(CHAN,BIN)=D(CHAN)
                     QE(CHAN,BIN)=E(CHAN)
                  ENDDO
                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  CALL TSP_EXTRACT(NCHANS,BUFFER,D,E)
                  DO CHAN=1,NCHANS
                     U(CHAN,BIN)=D(CHAN)
                     UE(CHAN,BIN)=E(CHAN)
                  ENDDO
                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  CALL TSP_EXTRACT(NCHANS,BUFFER,D,E)
                  DO CHAN=1,NCHANS
                     V(CHAN,BIN)=D(CHAN)
                     VE(CHAN,BIN)=E(CHAN)
                  ENDDO

                  READ(10,'(A)',IOSTAT=IOS) BUFFER
                  BIN = BIN+1
                  IF (BIN/100*100 .EQ. BIN) THEN
                     CALL MSG_SETI('BIN',BIN)
                     CALL MSG_OUT('MSG','Processing bin number ^BIN',
     :                        STATUS)
                  ENDIF
                  IF (BIN .GT. MAXLEN) THEN
                     CALL MSG_OUT('MSG','Too many data bins in file',
     :                        STATUS)
                     IOS = 1
                  ENDIF
                  FIRST = .FALSE.
               ENDIF
            ENDIF
         ENDDO

         BIN = BIN-1
         DO J=1,NCHANS
            IF (WAVES(J) .LT. 1) THEN
               CALL MSG_SETI('CHN',J)
               CALL MSG_OUT('MSG','Enter Name of channel ^CHN',STATUS)
               CALL PAR_GET0C('CHANNEL',NAME,STATUS)
               CALL PAR_CANCL('CHANNEL',STATUS)
               IF (NAME .EQ. 'K') THEN
                  WAVES(J) = 22000
                  ZEROPT(J) = 650
               ELSE IF (NAME .EQ. 'H') THEN
                  WAVES(J) = 16400
                  ZEROPT(J) = 1030
               ELSE IF (NAME .EQ. 'J') THEN
                  WAVES(J) = 12000
                  ZEROPT(J) = 1640
               ELSE IF (NAME .EQ. 'I') THEN
                  WAVES(J) = 7900
                  ZEROPT(J) = 2250
               ELSE IF (NAME .EQ. 'R') THEN
                  WAVES(J) = 6400
                  ZEROPT(J) = 3080
               ELSE IF (NAME .EQ. 'V') THEN
                  WAVES(J) = 5500
                  ZEROPT(J) = 3640
               ELSE IF (NAME .EQ. 'B') THEN
                  WAVES(J) = 4400
                  ZEROPT(J) = 4260
               ELSE IF (NAME .EQ. 'U') THEN
                  WAVES(J) = 3600
                  ZEROPT(J) = 1810
               ELSE
                  ZEROPT(J) = 3000
                  CALL PAR_GET0R('WAVELENGTH',WAVES(J),STATUS)
                  CALL PAR_CANCL('WAVELENGTH',STATUS)
               ENDIF
            ENDIF

*  Convert magnitudes to flux (Jy)

            IF (STATUS .EQ. SAI__OK) THEN
                DO K=1,BIN
                   IF (I(J,K) .GT. 50) THEN
                      I(J,K)=0.0
                      IE(J,K)=0.0
                   ELSE
                       I(J,K) = ZEROPT(J) / (10**(I(J,K)/2.5))
                       IE(J,K) = I(J,K) * (2.5*10**(IE(J,K)) - 1)
                       IE(J,K) = IE(J,K)*IE(J,K)
                   ENDIF

*  Convert P and Theta to Q and U

                   QP = Q(J,K) * COS (2*U(J,K)/DEGRAD)
                   UP = Q(J,K) * SIN (2*U(J,K)/DEGRAD)
                   Q(J,K) = QP/100.0 * I(J,K)
                   U(J,K) = UP/100.0 * I(J,K)
                   UE(J,K) = QE(J,K)/100.0 * I(J,K)
                   UE(J,K) = UE(J,K)*UE(J,K)
                   QE(J,K) = UE(J,K)

*  Convert V to Flux units

                   V(J,K) = V(J,K)/100.0 * I(J,K)
                   VE(J,K) = VE(J,K)/100.0 * I(J,K)
                   VE(J,K) = VE(J,K)*VE(J,K)
                ENDDO
             ENDIF
         ENDDO

         DIMS(1)=NCHANS
         DIMS(2)=BIN

*  Copy Intensity Data

         CALL TSP_CREATE_2D(LOC,NCHANS,BIN,'QUV',.TRUE.,.TRUE.,
     :         STATUS)
         CALL TSP_MAP_DATA(LOC,'WRITE',DPTR,DLOC,STATUS)
         CALL TSP_MAP_VAR(LOC,'WRITE',VPTR,VLOC,STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,I,MAXLEN,%VAL(DPTR),STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,IE,MAXLEN,%VAL(VPTR),STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)

*  Copy Q data

         CALL TSP_GET_STOKES(LOC,'Q',SLOC,STATUS)
         CALL TSP_MAP_DATA(SLOC,'WRITE',DPTR,DLOC,STATUS)
         CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,Q,MAXLEN,%VAL(DPTR),STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,QE,MAXLEN,%VAL(VPTR),STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)
         CALL DAT_ANNUL(SLOC,STATUS)

*  Copy U data

         CALL TSP_GET_STOKES(LOC,'U',SLOC,STATUS)
         CALL TSP_MAP_DATA(SLOC,'WRITE',DPTR,DLOC,STATUS)
         CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,U,MAXLEN,%VAL(DPTR),STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,UE,MAXLEN,%VAL(VPTR),STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)
         CALL DAT_ANNUL(SLOC,STATUS)

*  Copy V data

         CALL TSP_GET_STOKES(LOC,'V',SLOC,STATUS)
         CALL TSP_MAP_DATA(SLOC,'WRITE',DPTR,DLOC,STATUS)
         CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,V,MAXLEN,%VAL(DPTR),STATUS)
         CALL TSP_HDSPLOTCOPY(NCHANS,BIN,VE,MAXLEN,%VAL(VPTR),STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)
         CALL DAT_ANNUL(SLOC,STATUS)

*  Copy Axis data

         CALL TSP_MAP_LAMBDA(LOC,'WRITE',DPTR,DLOC,STATUS)
         CALL GEN_MOVE(4*NCHANS,WAVES,%VAL(DPTR))
         CALL TSP_WLU_LAMBDA(LOC,'Wavelength','Angstroms',STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)

         CALL TSP_MAP_TIME(LOC,'WRITE',DPTR,DLOC,STATUS)
         CALL GEN_MOVE(8*BIN,TIMES,%VAL(DPTR))
         CALL TSP_WLU_TIME(LOC,'MJD(UTC)','Days',STATUS)
         CALL TSP_UNMAP(DLOC,STATUS)

         CALL DAT_ANNUL(LOC,STATUS)
      ENDIF
      END



      SUBROUTINE TSP_EXTRACT(NCHANS,BUFFER,D,E)

*  Extract data from data line in ASCII file

      IMPLICIT NONE
      INTEGER NCHANS
      CHARACTER*(*) BUFFER
      REAL D(NCHANS), E(NCHANS)
      REAL VALUES(12)
      INTEGER ICODES(12)
      INTEGER NEXT,I,L

      L = LEN(BUFFER)
      DO I=1,L
         IF (.NOT.  ((BUFFER(I:I).GE.'0' .AND. BUFFER(I:I).LE.'9')
     :               .OR. (BUFFER(I:I) .EQ. '.')
     :               .OR. (BUFFER(I:I) .EQ. '-'))) THEN
            BUFFER(I:I) = ' '
         ENDIF
      ENDDO

      CALL ICH_NUMGT(BUFFER,1,' ',';',2*NCHANS,VALUES,ICODES,NEXT)
      DO I=1,NCHANS
         D(I) = VALUES(2*I-1)
         E(I) = VALUES(2*I)
      ENDDO
      END


      SUBROUTINE TSP_HDSPLOTCOPY(NCHANS,NBINS,IN,MAXLEN,OUT,STATUS)
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INTEGER STATUS,NCHANS,NBINS,MAXLEN
      REAL IN(6,MAXLEN), OUT(NCHANS,NBINS)
      INTEGER I,J

      IF (STATUS .EQ. SAI__OK) THEN
         DO J=1,NBINS
            DO I=1,NCHANS
               OUT(I,J)=IN(I,J)
            ENDDO
         ENDDO
      ENDIF
      END

