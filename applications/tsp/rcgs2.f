C+
      SUBROUTINE RCGS2(STATUS)
C
C            R C G S 2
C
C     Command name:
C        RCGS2
C
C     Function:
C        Read CGS2 Polarimetry Data
C
C     Description:
C        RCGS2 reads a polarimetry data file in Figaro format as produced
C        by the CGS2 Polarimetry system at UKIRT and reduces it to
C        a TSP polarization spectrum.
C
C        The CGS2 Figaro files are 4 dimensional arrays produced by the
C        DRT, in which the dimensions are: WAVEPLATE POSITIONS by SPECTRAL
C        CHANNELS by BEAMS (i.e. OFFSET or MAIN) by CYCLES.
C
C     Parameters:
C    (1) FIGARO     (Char)     The IRPS Figaro file to read.
C    (2) OUTPUT     (TSP, 1D)  The output time series dataset.
C    (3) NSIGMA     (Real)     Sigma level for despiking.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         28/8/1990
C
C-
C
C  History:
C    28/8/1990   Original Version.   JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,OIPTR,OVPTR,XPTR
      INTEGER QPTR,UPTR,DPTR,QVPTR,UVPTR

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,ALOC,QLOC,ULOC,VLOC
      CHARACTER*(DAT__SZLOC) SQLOC,SULOC,QVLOC,UVLOC,DLOC

*  Data size
      INTEGER NDIM, DIMS(7)

*  Length of file name
      INTEGER LENNAME

*  Data size information (4 dimensions of data array)
      INTEGER NPOINTS,NCYCLES,NPLATES,NBEAMS
      INTEGER I
      INTEGER EL,SL,XSLOT
      INTEGER J

      CHARACTER*80 FNAME,XLABEL,XUNITS

*  Strings from ...GET_INFO
      CHARACTER*80 STRINGS(2)

*  Dummy argument for ...GET_INFO
      DOUBLE PRECISION DUMMY

*  Despiking threshhold
      REAL NSIGMA

*  ICH function
      INTEGER ICH_LEN

*  Access the Figaro frame

      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      LENNAME = ICH_LEN(FNAME)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME(:LENNAME),STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN

*  get size of data
         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,EL,STATUS)

*  Check that it is four dimensional as required for CGS2
         IF (NDIM .NE. 4) THEN
            CALL MSG_OUT(' ','This is not a CGS2 Data File',
     :          STATUS)
            CALL MSG_OUT(' ','Incorrect number of Dimensions',
     :          STATUS)
            STATUS = USER__001
         ELSE

*  Copy the dimensions
            NPOINTS = DIMS(2)
            NPLATES = DIMS(1)
            NBEAMS = DIMS(3)
            NCYCLES = DIMS(4)

*  Check that NPLATES is 4 as it must be for linear polarimetry data
            IF (NPLATES .NE. 4) THEN
                CALL MSG_OUT(' ','This is not polarimetry data',
     :            STATUS)
                STATUS = USER__001
                GOTO 100

*  Check for too many spectral channels
            ELSE IF (NPOINTS .GT. 200) THEN
                CALL MSG_OUT(' ',
     :             'Too many wavelength channels (max 200)',
     :              STATUS)
                STATUS = USER__001
                GOTO 100
            ENDIF

*  Map the input data array
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,SL,STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT('MSG','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Map the wavelength axis data

            CALL DSA_MAP_AXIS_DATA('INPUT',2,'READ','FLOAT',XPTR,
     :           XSLOT,STATUS)

*  Get the label and units

            CALL DSA_GET_AXIS_INFO('INPUT',2,2,STRINGS,0,DUMMY,STATUS)
            XUNITS = STRINGS(1)
            XLABEL = STRINGS(2)

*  Create the output file

             CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure

             DIMS(1) = NPOINTS
             CALL TSP_CREATE_1D(OLOC,NPOINTS,'QU',.TRUE.,
     :               .TRUE.,STATUS)

*  Map the output data ...
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)
*  ... and its variance
             CALL TSP_MAP_VAR(OLOC,'WRITE',OVPTR,VLOC,STATUS)

*  Map the Q stokes data ...
             CALL TSP_GET_STOKES(OLOC,'Q',SQLOC,STATUS)
             CALL TSP_MAP_DATA(SQLOC,'WRITE',QPTR,QLOC,STATUS)

*  ... and its variance
             CALL TSP_MAP_VAR(SQLOC,'WRITE',QVPTR,QVLOC,STATUS)

*  Map the U stokes data
             CALL TSP_GET_STOKES(OLOC,'U',SULOC,STATUS)
             CALL TSP_MAP_DATA(SULOC,'WRITE',UPTR,ULOC,STATUS)

*  ... and its variance
             CALL TSP_MAP_VAR(SULOC,'WRITE',UVPTR,UVLOC,STATUS)

*  Get the sigma for despiking
             CALL PAR_GET0R('NSIGMA',NSIGMA,STATUS)

*  Copy the data

             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_RCGS2_COPY(NPLATES,NPOINTS,NBEAMS,NCYCLES,
     :           %VAL(IPTR),%VAL(OIPTR),%VAL(OVPTR),%VAL(QPTR),
     :           %VAL(QVPTR),%VAL(UPTR),%VAL(UVPTR),NSIGMA)
             ENDIF

*  Copy the wavelength axis data

             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',DPTR,DLOC,STATUS)
             CALL GEN_MOVE(4*NPOINTS,%VAL(XPTR),%VAL(DPTR))
             CALL TSP_WLU_LAMBDA(OLOC,XLABEL,XUNITS,STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(QLOC,STATUS)
             CALL TSP_UNMAP(ULOC,STATUS)
             CALL TSP_UNMAP(VLOC,STATUS)
             CALL TSP_UNMAP(QVLOC,STATUS)
             CALL TSP_UNMAP(UVLOC,STATUS)
             CALL DAT_ANNUL(SQLOC,STATUS)
             CALL DAT_ANNUL(SULOC,STATUS)
             CALL TSP_UNMAP(XLOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)
          ENDIF
100       CONTINUE
      ENDIF
      CALL DSA_CLOSE(STATUS)
      END




      SUBROUTINE TSP_RCGS2_COPY(N1,N2,N3,N4,IN,I,IV,Q,QV,U,UV,NSIGMA)
*+
*
*   T S P _ R C G S 2 _ C O P Y
*
*   RCGS2 command
*
*   Reduce the polarimetry data from a CGS2 observation. The input array
*   is a four dimensional array of data consisting of PLATE POSITIONS
*   by SPECTRAL POINTS by BEAMS by CYCLES. This is used to derive the
*   intensity and stokes parameter spectra for output. Spikes in the data
*   are removed if they deviate by more than NSIGMA from the mean
*
*   Parameters:
*
*   (>)  N1     (Integer)          First dimension of input data (number
*                                     of plate positions)
*   (>)  N2     (Integer)          Second dimension of input data (number
*                                     of spectral points)
*   (>)  N3     (Integer)          Third dimension of input data (number
*                                     of BEAMS (i.e. STAR/SKY)
*   (>)  N4     (Integer)          Fourth dimension of input data (number
*                                     of CYCLES)
*   (>)  IN     (Real array(N1,N2,N3,N4)  Input array
*   (<)  I      (Real array(N2))   Output Intensity array
*   (<)  IV     (Real array(N2))   Variance on intensity array
*   (<)  Q      (Real array(N2))   Output Q stokes parameter array
*   (<)  QV     (Real array(N2))   Variance on Q stokes array
*   (<)  U      (Real array(N2))   Output U stokes parameter array
*   (<)  UV     (Real array(N2))   Variance on U stokes array
*   (>)  NSIGMA (Real)             Multiple of sigma for despiking
*
*   Jeremy Bailey    28/8/1990
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER N1,N2,N3,N4
      REAL IN(N1,N2,N3,N4)
      REAL I(N2),IV(N2),Q(N2),QV(N2),U(N2),UV(N2)
      REAL NSIGMA

*  Local constant
      INTEGER MAX_CHANS
      PARAMETER (MAX_CHANS=200)

*  Local variables
      INTEGER J,IP,CYCLE
      DOUBLE PRECISION RAW(8,MAX_CHANS)
      DOUBLE PRECISION C(MAX_CHANS),CE(MAX_CHANS),M(MAX_CHANS),
     :   ME(MAX_CHANS),QQ(MAX_CHANS),QQE(MAX_CHANS),UU(MAX_CHANS),
     :   UUE(MAX_CHANS),P(MAX_CHANS),PE(MAX_CHANS),PA(MAX_CHANS),
     :   PAE(MAX_CHANS)
      REAL MAIN,OFFSET

*  Initialize for polarimetry reduction
      CALL POL_INIT

*  Add data into cumulative arrays
      DO CYCLE = 1,N4
          DO J = 1,N2
              DO IP = 1,4

*  Main beam
                  MAIN = IN(IP,J,1,CYCLE)
                  IF (MAIN .EQ. 0.0) THEN
*
*  This test for zero data is included to detect data which is zero
*  as a result of a bug in the DRT. It can probably be removed eventually
*
                      PRINT *,'Zero in cycle ',CYCLE,J, 'Main beam'
                      IF (CYCLE .NE. 1) THEN
                          MAIN = IN(IP,J,1,CYCLE-1)
                      ENDIF
                  ENDIF

*  Offset beam
                  OFFSET = IN(IP,J,2,CYCLE)
                  IF (OFFSET .EQ. 0.0) THEN
                      PRINT *,'Zero in cycle ',CYCLE,J, 'Offset beam'
                      IF (CYCLE .NE. 1) THEN
                          OFFSET = IN(IP,J,2,CYCLE-1)
                      ENDIF
                  ENDIF

*  Use MAIN - OFFSET as raw data for polarimetry reduction
                  RAW(IP,J) = MAIN - OFFSET
                  RAW(IP+4,J) = RAW(IP,J)
              ENDDO
          ENDDO

*  Add into polarimetry reduction
          CALL POL_ADD(N2,RAW,1.0D0)
      ENDDO

*  Despike the data if we have more than two cycles
      IF (N4 .GT. 2) THEN
          CALL POL_DESPIKE(N2,NSIGMA)
      ENDIF

*  Despike again if we have more than 3 cycles
      IF (N4 .GT. 3) THEN
          CALL POL_DESPIKE(N2,NSIGMA)
      ENDIF

*  Reduce the data
      CALL POL_REDUCE(N2,C,CE,M,ME,QQ,QQE,UU,UUE,P,PE,PA,PAE)

*  Convert to format for output arrays
*  Stokes parameters are converted from percentage to intensity units
*  Errors are converted to variances
      DO J=1,N2
          I(J) = C(J)
          IV(J) = CE(J)*CE(J)
          Q(J) = QQ(J)*C(J)/100.0
          U(J) = UU(J)*C(J)/100.0
          QV(J) = QQE(J)*C(J)/100.0
          QV(J) = QV(J)*QV(J)
          UV(J) = UUE(J)*C(J)/100.0
          UV(J) = UV(J)*UV(J)
      ENDDO

      END
