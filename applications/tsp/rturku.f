C+
      SUBROUTINE RTURKU(STATUS)
C
C            R T U R K U
C
C     Command name:
C        RTURKU
C
C     Function:
C        Read ASCII files of Data from the Turku UBVRI Polarimeter.
C
C     Description:
C        RTURKU reads ASCII files of data from the Turku University UBVRI
C        polarimeter. The raw data must first be reduced using the POLRED
C        (for linear polarimetery) or CIRLIN (for simultaneous circular/linear
C        polarimetry) programs. The resulting files are read by RTURKU
C        (one file for the linear case, two for the circular/linear case).
C        The data on a given star is selected from the file by specifying
C        its number, and output as a TSP time series dataset.
C
C     Parameters:
C        CIRLIN     (Logical)  TRUE for circular+linear data
C                              FALSE for linear only data.
C        LINFILE    (Char)     The name of the linear data input file.
C        CIRFILE    (Char)     The name of the circular data input file.
C        STAR       (Integer)  The Star Number
C        OUTPUT     (TSP, 2D)  The output dataset to be created.
C
C
C     Support:
C        Jeremy Bailey, AAO
C
C     Version date:
C        4/11/1988
C
C-
C
C  History:
C    4/11/1988   Original Version.   JAB/JAC
C


      IMPLICIT NONE

*  Status argument
      INTEGER STATUS
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Maximum length of data
      INTEGER MAXLEN
      PARAMETER (MAXLEN = 2000)

*  File name for linear polarization ASCII file
      CHARACTER*80 LINFILE

*  File name for linear polarization ASCII file
      CHARACTER*80 CIRFILE

*  Length of name
      INTEGER LEN

*  I/O status
      INTEGER IOS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,SLOC,DLOC,VLOC

*  Data read from file
      REAL I(5,MAXLEN), Q(5,MAXLEN), U(5,MAXLEN), V(5,MAXLEN)
      REAL IE(5,MAXLEN), QE(5,MAXLEN), UE(5,MAXLEN), VE(5,MAXLEN)

*  Wavelength and zero point of each channel
      REAL WAVES(5),ZEROPT(5)

*  Time axis array
      DOUBLE PRECISION TIMES(MAXLEN)

*  BIN number
      INTEGER BIN

*  Channel number
      INTEGER CHAN

*  Data pointers
      INTEGER DPTR,VPTR

*  Dimensions of data array
      INTEGER DIMS(2)

*  Loop counters
      INTEGER J,K

*  Circular polarization flag
      LOGICAL CIRCULAR

*  data read from file
      INTEGER ST,FI,AP,JD,PX,PY,PE1,PE2
      INTEGER MG,ST1,FI1,AP1,JD1,VX,VY,VE1,VE2
      INTEGER STAR

*  CHR functions
      INTEGER CHR_LEN

*  Get circular polarization flag

      CALL PAR_GET0L('CIRLIN',CIRCULAR,STATUS)

*  Get the name of the input files


*  Linear file
      CALL PAR_GET0C('LINFILE',LINFILE,STATUS)
      LEN = CHR_LEN(LINFILE)
      IF (STATUS .EQ. SAI__OK) THEN

*  Open the file

         OPEN(UNIT=10,FILE=LINFILE(1:LEN),STATUS='OLD',
     :      IOSTAT=IOS)
         IF (IOS .NE. 0) THEN
            CALL MSG_OUT('MSG','Cannot open file',STATUS)
            STATUS = USER__001
            RETURN
         ENDIF

      ENDIF
      IF (CIRCULAR) THEN

*  Circular file
          CALL PAR_GET0C('CIRFILE',CIRFILE,STATUS)
          LEN = CHR_LEN(CIRFILE)
          IF (STATUS .EQ. SAI__OK) THEN

*  Open the file

            OPEN(UNIT=11,FILE=CIRFILE(1:LEN),STATUS='OLD',
     :         IOSTAT=IOS)
            IF (IOS .NE. 0) THEN
                CALL MSG_OUT('MSG','Cannot open file',STATUS)
                STATUS = USER__001
                CLOSE(UNIT=10)
                RETURN
            ENDIF

         ENDIF
      ENDIF

*  Get Star Number

      CALL PAR_GET0I('STAR',STAR,STATUS)

*  Get Output TSP File

      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',LOC,STATUS)

*  Set up wavelength and zero point array

*  U band
      WAVES(1) = 3600
      ZEROPT(1) = 1810

*  B band
      WAVES(2) = 4400
      ZEROPT(2) = 4260

*  V band
      WAVES(3) = 5500
      ZEROPT(3) = 3640

*  R band
      WAVES(4) = 6400
      ZEROPT(4) = 3080

*  I band
      WAVES(5) = 7900
      ZEROPT(5) = 2550

*  Loop over bins and channels
      BIN = 1
      CHAN = 1
      IOS = 0
      DO WHILE (IOS .EQ. 0)

*  Read data from linear file
         READ(10,'(I7,I9,I2,I10,2I7,3I6)',IOSTAT=IOS) ST,FI,AP,JD,
     :          PX,PY,PE1,PE2,MG
         IF (CIRCULAR) THEN

*  Read data from circular file
            READ(11,'(I7,I9,I2,I10,2I7,3I6)',IOSTAT=IOS) ST1,FI1,AP1,
     :             JD1,VX,VY,VE1,VE2,MG

*  Check that the two files are consistent
            IF (ST .NE. ST1 .OR. FI .NE. FI1 .OR. AP .NE. AP1
     :           .OR. JD .NE. JD1) THEN
               CALL MSG_OUT(' ',
     :            'Circ and Lin files not consistent',STATUS)
               STATUS = USER__001
               CLOSE(UNIT=10)
               CLOSE(UNIT=11)
               CALL DAT_ANNUL(LOC,STATUS)
               RETURN
            ENDIF
         ENDIF

         IF (IOS .EQ. 0) THEN

*  Is it the write star number
            IF (ST .EQ. STAR) THEN

*  Check the filter sequence is valid
               IF (CHAN .NE. FI) THEN
                  CALL MSG_OUT(' ','Invalid filter sequence',STATUS)
                  STATUS = USER__001
                  CLOSE(UNIT=10)
                  CLOSE(UNIT=11)
                  CALL DAT_ANNUL(LOC,STATUS)
                  RETURN
               ENDIF

*  Fill times array with MJD
               TIMES(BIN) = DBLE(JD)/10000.0D0 + 39999.5D0

*  Write data to intensity array converting magnitude to flux
               I(CHAN,BIN) = ZEROPT(CHAN) / (10**(REAL(MG)/2500.0))
               IE(CHAN,BIN) = 0.0

*  Write Q and U stokes array with polarization values
               Q(CHAN,BIN) = I(CHAN,BIN) * REAL(PX) / 100000.0
               U(CHAN,BIN) = I(CHAN,BIN) * REAL(PY) / 100000.0

*  Write Q and U errors
               QE(CHAN,BIN) =
     :              (REAL(MAX(PE1,PE2))/REAL(PX)*Q(CHAN,BIN))**2
               UE(CHAN,BIN) =
     :              (REAL(MAX(PE1,PE2))/REAL(PY)*U(CHAN,BIN))**2
               IF (CIRCULAR) THEN

*  Write V stokes parameter and error
                  V(CHAN,BIN) = I(CHAN,BIN) * REAL(VX) / 100000.0
                  VE(CHAN,BIN) =
     :                 (REAL(MAX(VE1,VE2))/REAL(VX)*V(CHAN,BIN))**2
               ENDIF

*  Increment channel number
               CHAN = CHAN+1
               IF (CHAN .GT. 5) THEN

*  If we are past five reset to one and increment the bin number
                   CHAN = 1
                   BIN = BIN+1

*  Output message giving bin number if it is a multiple of 100
                   IF (BIN/100*100 .EQ. BIN) THEN
                      CALL MSG_SETI('BIN',BIN)
                      CALL MSG_OUT(' ','Processsing bin number ^BIN',
     :                     STATUS)
                   ENDIF

*  Complain if there are too many bins
                   IF (BIN .GT. MAXLEN) THEN
                      CALL MSG_OUT(' ','Too many data bins in file',
     :                     STATUS)
                      IOS = 1
                   ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO

*  Final number of bins
      BIN = BIN - 1
      CLOSE(UNIT=10)
      CLOSE(UNIT=11)

*  Copy intensity data

      IF (STATUS .EQ. SAI__OK) THEN

*  Create the output structure
         IF (CIRCULAR) THEN
            CALL TSP_CREATE_2D(LOC,5,BIN,'QUV',.TRUE.,.TRUE.,STATUS)
         ELSE
            CALL TSP_CREATE_2D(LOC,5,BIN,'QU',.TRUE.,.TRUE.,STATUS)
         ENDIF

*  Map the output data
         CALL TSP_MAP_DATA(LOC,'WRITE',DPTR,DLOC,STATUS)

*  Map the variance
         CALL TSP_MAP_VAR(LOC,'WRITE',VPTR,VLOC,STATUS)

*  Copy the intensity data
         CALL TSP_TURKUCOPY(5,BIN,I,MAXLEN,%VAL(DPTR),STATUS)

*  Copy the intensity variance
         CALL TSP_TURKUCOPY(5,BIN,IE,MAXLEN,%VAL(VPTR),STATUS)

*  Unmap the arrays
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)

*  Copy Q data

*  Get Q stokes parameter
         CALL TSP_GET_STOKES(LOC,'Q',SLOC,STATUS)

*  Map the Q stokes array
         CALL TSP_MAP_DATA(SLOC,'WRITE',DPTR,DLOC,STATUS)

*  Map the Q stokes variance
         CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)

*  Copy the Q stokes data
         CALL TSP_TURKUCOPY(5,BIN,Q,MAXLEN,%VAL(DPTR),STATUS)

*  Copy the Q stokes variance
         CALL TSP_TURKUCOPY(5,BIN,QE,MAXLEN,%VAL(VPTR),STATUS)

*  Unmap the arrays
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)
         CALL DAT_ANNUL(SLOC,STATUS)


*  Copy U data

*  Get the U stokes parameter
         CALL TSP_GET_STOKES(LOC,'U',SLOC,STATUS)

*  Map the U stokes data
         CALL TSP_MAP_DATA(SLOC,'WRITE',DPTR,DLOC,STATUS)

*  Map the U stokes variance
         CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)

*  Copy the U stokes data
         CALL TSP_TURKUCOPY(5,BIN,U,MAXLEN,%VAL(DPTR),STATUS)

*  Copy the U stokes variance
         CALL TSP_TURKUCOPY(5,BIN,UE,MAXLEN,%VAL(VPTR),STATUS)

*  Unmap the U stokes data
         CALL TSP_UNMAP(DLOC,STATUS)
         CALL TSP_UNMAP(VLOC,STATUS)
         CALL DAT_ANNUL(SLOC,STATUS)

*  Copy V data

         IF (CIRCULAR) THEN

*  Get the V stokes parameter
            CALL TSP_GET_STOKES(LOC,'V',SLOC,STATUS)

*  Map the V stokes data
            CALL TSP_MAP_DATA(SLOC,'WRITE',DPTR,DLOC,STATUS)

*  Map the V stokes variance
            CALL TSP_MAP_VAR(SLOC,'WRITE',VPTR,VLOC,STATUS)

*  Copy the V stokes data
            CALL TSP_TURKUCOPY(5,BIN,V,MAXLEN,%VAL(DPTR),STATUS)

*  Copy the V stokes variance
            CALL TSP_TURKUCOPY(5,BIN,VE,MAXLEN,%VAL(VPTR),STATUS)

*  Unmap the arrays
            CALL TSP_UNMAP(DLOC,STATUS)
            CALL TSP_UNMAP(VLOC,STATUS)
            CALL DAT_ANNUL(SLOC,STATUS)
         ENDIF

*  Copy wavelength axis data
         CALL TSP_MAP_LAMBDA(LOC,'WRITE',DPTR,DLOC,STATUS)
         CALL TSP_GEN_MOVE(5,WAVES,%VAL(DPTR))

*  Set the units of the wavelength axis
         CALL TSP_WLU_LAMBDA(LOC,'Wavelength','Angstroms',STATUS)

*  Unmap the wavelength axis array
         CALL TSP_UNMAP(DLOC,STATUS)

*  Map the time axis
         CALL TSP_MAP_TIME(LOC,'WRITE',DPTR,DLOC,STATUS)

*  Copy the time axis data
         CALL TSP_GEN_MOVE(2*BIN,TIMES,%VAL(DPTR))

*  Set units of the time axis
         CALL TSP_WLU_TIME(LOC,'MJD(UTC)','Days',STATUS)

*  Unmap the time axis array
         CALL TSP_UNMAP(DLOC,STATUS)
      ENDIF
      CALL DAT_ANNUL(LOC,STATUS)
      END


      SUBROUTINE TSP_TURKUCOPY(NCHANS,NBINS,IN,MAXLEN,OUT,STATUS)
*+
*
*   T S P _ T U R K U C O P Y
*
*   RTURKU command
*
*   Copy data from input array to output array
*
*   Parameters:
*
*   (>)   NCHANS    (Integer)               Number of channels
*   (>)   NBINS     (Integer)               Number of bins
*   (>)   IN        (Real array(5,MAXLEN)   Input array
*   (>)   MAXLEN    (Integer)               Maximum number of bins
*   (<)   OUT       (Real array(NCHANS,NBINS)  Output array
*   (!)   STATUS    (Integer)               Status value
*
*   Jeremy Bailey      4/11/1988
*
*+
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      INTEGER STATUS,NCHANS,NBINS,MAXLEN
      REAL IN(5,MAXLEN), OUT(NCHANS,NBINS)

*  Local variables
      INTEGER I,J

      IF (STATUS .EQ. SAI__OK) THEN

*  Loop over bins and channels
         DO J=1,NBINS
            DO I=1,NCHANS

*  Copy input to output
               OUT(I,J)=IN(I,J)
            ENDDO
         ENDDO
      ENDIF
      END

