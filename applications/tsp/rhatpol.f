C+
      SUBROUTINE RHATPOL(STATUS)
C
C            R H A T P O L
C
C     Command name:
C        RHATPOL
C
C     Function:
C        Read Hatfield Polarimeter Data
C
C     Description:
C        RHATPOL reads polarimetry data files in Figaro format as produced
C        by the Hatfield Polarimeter systems on UKIRT or the AAT.
C        A time series dataset is created containing the reduced linear
C        or linear+circular polarimetry data.
C
C        A calibration file is used to specify the values of calibration
C        parameters (efficiency, position angle zero point, photometric
C        zero point, anc circular calibration).
C
C     Parameters:
C    (1) FIGARO     (Char)     The IRPS Figaro file to read.
C    (2) NPTS       (Integer)  Number of points per cycle (1 or 2).
C    (3) LINEAR     (Logical)  True for linear data, false for circular.
C    (4) OUTPUT     (TSP, 2D)  The output time series dataset.
C    (5) CFILE      (File)     Name of calibration file.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         3/3/1990
C
C-
C
C  History:
C    3/3/1990   Original Version.   JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,OIPTR,XPTR
      INTEGER QPTR,UPTR,VPTR,DPTR

*  Number of elements
      INTEGER NELM

*  Array dimensions
      INTEGER NDIM, DIMS(7)

*  File name length
      INTEGER LENNAME

*  Size of dimensions of data array
      INTEGER NPOINTS,NCYCLES,NPTS,N2
      INTEGER I

*  Needed for time conversion
      INTEGER NSTRT
      INTEGER IH,IM,IY,ID

*  Number of elements and slot id
      INTEGER EL,SL

*  Comment string for FITS header reads
      CHARACTER*64 COM

*  IRPS gain
      REAL AG

*  For MJD calculation
      DOUBLE PRECISION SEC,DJ1,DJ2,DJM
      INTEGER J

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,ALOC,QLOC,ULOC,VLOC
      CHARACTER*(DAT__SZLOC) SQLOC,SULOC,SVLOC,DLOC

*  Figaro file name
      CHARACTER*80 FNAME

*  Label and units
      CHARACTER*80 LABEL,UNITS

*  UT strings
      CHARACTER*80 UTSTART,UTEND,UTDATE

*  Array of calibration parameters for each of the six channels
      REAL WAVES(6),ZEROPT(6),PA(6),EFF(6),PHOT(6),EXT(6),VV(6),WW(6)

*  TRUE for linear (as opposed to circular) polarization
      LOGICAL LINEAR

*  Channel name
      CHARACTER*20 NAME

*  File descriptor for calibration file
      INTEGER FD
      INTEGER NC
      CHARACTER*80 BUFFER

      INTEGER ICH_LEN

*  Access the Figaro frame
      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      LENNAME = ICH_LEN(FNAME)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME(:LENNAME),STATUS)

*  Get the data array
      IF (STATUS .EQ. SAI__OK) THEN

*  Find size of data
         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,EL,STATUS)

*  Check that it is three dimensional
         IF (NDIM .NE. 3) THEN
            CALL MSG_OUT('MSG','Dimensions of Input File Invalid',
     :          STATUS)
            STATUS = USER__001
         ELSE

*  Copy the dimensions
            NPOINTS = DIMS(1)
            N2 = DIMS(2)
            NCYCLES = DIMS(3)

*  Map the data
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,SL,STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT('MSG','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Get information from FITS header

*  Get object name
            CALL DSA_GET_FITS_C('INPUT','OBJECT',0,LABEL,COM,STATUS)

*  Get Start and end UT for data
            CALL DSA_GET_FITS_C('INPUT','UTDATE',0,UTDATE,COM,STATUS)
            CALL DSA_GET_FITS_C('INPUT','UTSTART',0,UTSTART,COM,STATUS)
            CALL DSA_GET_FITS_C('INPUT','UTEND',0,UTEND,COM,STATUS)

*  Get IRPS gain
            CALL DSA_GET_FITS_F('INPUT','IRPS_AG',0,AG,COM,STATUS)
            STATUS = SAI__OK

*  Replace colons in times with spaces
            DO I=1,64
                IF (UTSTART(I:I) .EQ. ':') UTSTART(I:I)=' '
                IF (UTEND(I:I) .EQ. ':') UTEND(I:I)=' '
                IF (UTDATE(I:I) .EQ. '/') UTDATE(I:I)=' '
            ENDDO

*  Decode UTSTART string
            NSTRT = 1

*  Hours
            CALL SLA_INTIN(UTSTART,NSTRT,IH,J)

*  Minutes
            CALL SLA_INTIN(UTSTART,NSTRT,IM,J)

*  Seconds
            CALL SLA_DFLTIN(UTSTART,NSTRT,SEC,J)

*  Calculate fraction of a day
            CALL SLA_DTF2D(IH,IM,SEC,DJ1,J)

*  Decode UTEND string
            NSTRT = 1

*  Hours
            CALL SLA_INTIN(UTEND,NSTRT,IH,J)

*  Minutes
            CALL SLA_INTIN(UTEND,NSTRT,IM,J)

*  Seconds
            CALL SLA_DFLTIN(UTEND,NSTRT,SEC,J)

*  Convert to fraction of a day
            CALL SLA_DTF2D(IH,IM,SEC,DJ2,J)

*  Decode UT data string
            NSTRT = 1

*  Year
            CALL SLA_INTIN(UTDATE,NSTRT,IY,J)

*  Month
            CALL SLA_INTIN(UTDATE,NSTRT,IM,J)

*  Day
            CALL SLA_INTIN(UTDATE,NSTRT,ID,J)

*  Calculate MJD
            CALL SLA_CLDJ(IY,IM,ID,DJM,J)

*  Start MJD
            DJ1 = DJM+DJ1

*  End MJD
            DJ2 = DJM+DJ2

*  Points per cycle
            CALL PAR_GET0I('NPTS',NPTS,STATUS)
            IF (NPTS .NE. 1 .AND. NPTS .NE. 2) THEN
                CALL MSG_OUT(' ','Must be 1 or 2',STATUS)
                GOTO 100
            ENDIF

*  Linear or circular
            CALL PAR_GET0L('LINEAR',LINEAR,STATUS)

*  Get the output file
             CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure
             DIMS(1) = 6
             DIMS(2) = NCYCLES*NPTS

*  Create an output structure with either two Stokes parameter (QU) for
*  the linear case, or 3 stokes parameters (QUV) for the circular case.
             IF (LINEAR) THEN
                 CALL TSP_CREATE_2D(OLOC,6,DIMS(2),'QU',.FALSE.,
     :               .FALSE.,STATUS)
             ELSE
                 CALL TSP_CREATE_2D(OLOC,6,DIMS(2),'QUV',.FALSE.,
     :               .FALSE.,STATUS)
             ENDIF

*  Map the output intensity
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Map the Q stokes parameter
             CALL TSP_GET_STOKES(OLOC,'Q',SQLOC,STATUS)
             CALL TSP_MAP_DATA(SQLOC,'WRITE',QPTR,QLOC,STATUS)

*  Map the U stokes parameter
             CALL TSP_GET_STOKES(OLOC,'U',SULOC,STATUS)
             CALL TSP_MAP_DATA(SULOC,'WRITE',UPTR,ULOC,STATUS)

*  For circular data map the V stokes parameter
             IF (.NOT. LINEAR) THEN
                 CALL TSP_GET_STOKES(OLOC,'V',SVLOC,STATUS)
                 CALL TSP_MAP_DATA(SVLOC,'WRITE',VPTR,VLOC,STATUS)
             ELSE
                 VPTR = UPTR
             ENDIF

*  Set label and units of times axis
             CALL TSP_WLU_TIME(OLOC,'MJD(UTC)','Days',STATUS)

*  Map the time axis
             CALL TSP_MAP_TIME(OLOC,'WRITE',XPTR,XLOC,STATUS)
             IF (STATUS .NE. SAI__OK) THEN
                 GOTO 100
             ENDIF

*  Open the calibration files
             CALL FIO_ASSOC('CFILE','READ','LIST',0,FD,STATUS)

*  And read in the calibration data
             DO J=1,6
                 CALL FIO_READ(FD,BUFFER,NC,STATUS)
                 IF (LINEAR) THEN
                    READ(BUFFER,*) PA(J),EFF(J),PHOT(J),EXT(J)
                 ELSE
                    READ(BUFFER,*) PA(J),EFF(J),PHOT(J),EXT(J),
     :                 VV(J),WW(J)
                 ENDIF
             ENDDO

*  Loop over channels, getting the channel name and wavelength
             DO J=1,6
              IF (WAVES(J) .LT. 1) THEN
               CALL MSG_SETI('CHN',J)
               CALL MSG_OUT('MSG','Enter Name of channel ^CHN',STATUS)
               CALL PAR_GET0C('CHANNEL',NAME,STATUS)
               CALL PAR_CANCL('CHANNEL',STATUS)

*  Set up the zero point and wavelength if it is a standard band
               IF (NAME .EQ. 'K') THEN

*  K band
                  WAVES(J) = 22000
                  ZEROPT(J) = 650
               ELSE IF (NAME .EQ. 'H') THEN

*  H band
                  WAVES(J) = 16400
                  ZEROPT(J) = 1030
               ELSE IF (NAME .EQ. 'J') THEN

*  J band
                  WAVES(J) = 12000
                  ZEROPT(J) = 1640
               ELSE IF (NAME .EQ. 'I') THEN

*  I band
                  WAVES(J) = 7900
                  ZEROPT(J) = 2250
               ELSE IF (NAME .EQ. 'R') THEN

*  R band
                  WAVES(J) = 6400
                  ZEROPT(J) = 3080
               ELSE IF (NAME .EQ. 'V') THEN

*  V band
                  WAVES(J) = 5500
                  ZEROPT(J) = 3640
               ELSE IF (NAME .EQ. 'B') THEN

*  B band
                  WAVES(J) = 4400
                  ZEROPT(J) = 4260
               ELSE IF (NAME .EQ. 'U') THEN

*  U band
                  WAVES(J) = 3600
                  ZEROPT(J) = 1810
               ELSE

*  Otherwise prompt for the wavelength
                  ZEROPT(J) = 3000
                  CALL PAR_GET0R('WAVELENGTH',WAVES(J),STATUS)
                  CALL PAR_CANCL('WAVELENGTH',STATUS)
               ENDIF
              ENDIF
             ENDDO

*  Copy the data
             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_RHATPOL_COPY(NCYCLES,N2,NPOINTS,NPTS,AG,
     :           LINEAR,DJ1,DJ2,PA,EFF,PHOT,VV,WW,
     :           %VAL(IPTR),%VAL(OIPTR),%VAL(QPTR),
     :           %VAL(UPTR),%VAL(VPTR),%VAL(XPTR))
             ENDIF

*  Copy wavelength axis data
             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',DPTR,DLOC,STATUS)
             CALL GEN_MOVE(4*6,WAVES,%VAL(DPTR))
             CALL TSP_WLU_LAMBDA(OLOC,'Wavelength','Angstroms',STATUS)
             CALL TSP_UNMAP(DLOC,STATUS)

*  Unmap output arrays and annul locators
             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(QLOC,STATUS)
             CALL TSP_UNMAP(ULOC,STATUS)
             CALL DAT_ANNUL(SQLOC,STATUS)
             CALL DAT_ANNUL(SULOC,STATUS)
             IF (.NOT. LINEAR) THEN
                 CALL TSP_UNMAP(VLOC,STATUS)
                 CALL DAT_ANNUL(SVLOC,STATUS)
             ENDIF
             CALL TSP_UNMAP(XLOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)
          ENDIF

*  Close the calibration file
          CALL FIO_CANCL('CFILE',STATUS)
100       CONTINUE
      ENDIF

*  Close DSA
      CALL DSA_CLOSE(STATUS)
      END




      SUBROUTINE TSP_RHATPOL_COPY(NCYCLES,N2,NPOINTS,NPTS,AG,LINEAR,
     :     JD1,JD2,PA,EFF,PHOT,VV,WW,IN,OUT,Q,U,V,TIMES)
*+
*
*  T S P _ R H A T P O L _ C O P Y
*
*  Reduce Hatfield polarimeter data and copy results to the Stokes
*  arrays of the output dataset
*
*   (>)   NCYCLES   (Integer)          Number of cycles of data
*   (>)   N2        (Integer)          Second dimension of data array
*   (>)   NPOINTS   (Integer)          Number of points in original data
*   (>)   NPTS      (Integer)          Number of points per cycle
*   (>)   AG        (Integer)          IRPS amplifier gain
*   (>)   LINEAR    (Logical)          TRUE for linear polarization
*   (>)   JD1       (Double)           Start MJD
*   (>)   JD2       (Double)           End MJD
*   (>)   PA        (Real array(6))    Position angle zero points
*   (>)   EFF       (Real array(6))    Efficency factors
*   (>)   PHOT      (Real array(6))    Photometric zero points
*   (>)   VV        (Real array(6))    Circular poln vector 1
*   (>)   WW        (Real array(6))    Circular poln vector 2
*   (>)   IN        (Real array(NPOINTS,N2,NCYCLES)   Input data array
*   (<)   OUT       (Real array(6,NCYCLES*NPTS))      Output intensity array
*   (<)   Q         (Real array(6,NCYCLES*NPTS))      Q Stokes array
*   (<)   U         (Real array(6,NCYCLES*NPTS))      U Stokes array
*   (<)   V         (Real array(6,NCYCLES*NPTS))      V Stokes array
*   (<)   TIMES     (Double array(NCYCLES*NPTS))      Array of MJDs
*
*   Jeremy Bailey    3/3/1990
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER NCYCLES,NPOINTS,NPTS,N2
      DOUBLE PRECISION JD1,JD2,TIMES(NCYCLES*NPTS)
      REAL PA(6),EFF(6),PHOT(6),VV(6),WW(6)
      REAL IN(NPOINTS,N2,NCYCLES),OUT(6,NCYCLES*NPTS)
      REAL Q(6,NCYCLES*NPTS),U(6,NCYCLES*NPTS),V(6,NCYCLES*NPTS)
      REAL AG
      LOGICAL LINEAR

*  Local variables
      INTEGER I,J,K,ND
      REAL SX(4)
      DOUBLE PRECISION JD,JDS

*  calculate duration of one cycle
      JDS = (JD2-JD1)/(NCYCLES*NPTS)

*  Calculate start time
      JD = JD1-JDS/2.0

*  Fill time axis array with times
      DO I=1,NCYCLES*NPTS
          JD = JD+JDS
          TIMES(I) = JD
      ENDDO

*  Do the reduction of the polarimetry data
      IF (.NOT. LINEAR) THEN

*  Reduce the circular
          CALL TSP_WORK_ROUTINEC(NPOINTS,N2,NCYCLES,NPTS,EFF,PHOT,
     :    VV,WW,IN,OUT,V)

*  Halve the efficiency for linear polarimetry with a quarter wave plate
          DO I=1,6
             EFF(I)=EFF(I)*2.0
          ENDDO
      ENDIF

*  Reduce the linear
      CALL TSP_WORK_ROUTINE(NPOINTS,N2,NCYCLES,NPTS,PA,EFF,PHOT,
     :    IN,OUT,Q,U)

      END




      Subroutine TSP_WORK_ROUTINE(N1,N2,N3,NPTS,CPA,EFF,PHOT,
     :    ARRAY,OUT,Q,U)
*+
*
*   T S P _ W O R K _ R O U T I N E
*
*   RHATPOL command
*
*   Reduce linear polarization data from the Hatfield polarimeter
*   This routine is used to reduce linear polarization data taken
*   with either a half-wave plate or quarter-wavel plate. TSP_WORK_ROUTINEC
*   is used to reduce the circular part of the data taken with a
*   quarter wavel plate.
*
*   Parameters:
*
*   (>)  N1       (Integer)          First dimension of input array
*   (>)  N2       (Integer)          Second dimension of input array
*   (>)  N3       (Integer)          Third dimension of input array
*   (>)  NPTS     (Integer)          Number of points per cycle
*   (>)  CPA      (Real array(6))    Position angle calibration
*   (>)  EFF      (Real array(6))    Efficiency factor
*   (>)  PHOT     (Real array(6))    Photometric zero point
*   (>)  ARRAY    (Real array(N1,N2,N3))  Input data array
*   (<)  OUT      (Real array(6,N3*NPTS))  Output Intensity array
*   (<)  Q        (Real array(6,N3*NPTS))  Output Q array
*   (<)  U        (Real array(6,N3*NPTS))  Output U array
*
*   Jeremy Bailey     3/3/1990
*
*   Based on a Figaro program (HATPOL) by Andrew Mead (ROE)
*
*+
      implicit none

*     Arguments
      integer   N1,N2,N3,NPTS
      real      ARRAY(N1,N2,N3)
      real      OUT(6,N3*NPTS),Q(6,N3*NPTS),U(6,N3*NPTS)
      REAL      CPA(6),EFF(6),PHOT(6)

*     Local variables
      integer   NCHANNELS
      parameter (NCHANNELS=6)
      real DEGRAD

*     Number of cycles
      INTEGER NCYCLE
      integer   I,J,K,KK,NOPT   ! counting variables

*     Arrays for data reduction, and output arrays from the POL routines
      double precision    OPT_RAW(1000,8,6),RAW(8,6),
     $          C(6),M(6),P(6),QQ(6),UU(6),PA(6),
     $          CE(6),ME(6),PE(6),QQE(6),UUE(6),PAE(6)
      real      VALUE
      logical   FINISHED
      integer   DCYCLE
      integer   ROTS
      double precision    C1,C2
      INTEGER   STATUS
      character  NAME*64

      DEGRAD = 45.0/ATAN(1.0)

*     Load data into array OPT_RAW

      NCYCLE = N3
      do I=1,NCYCLE
       do J=1,8
        do K=1,NCHANNELS
         if (N2 .EQ. 4) THEN

*  UKIRT data (one point per beam position)

          OPT_RAW((2*I)-1,J,K)=(ARRAY(((J-1)*6)+K,1,I)+
     $       ARRAY(((J-1)*6)+K+48,1,I))-(ARRAY(((J-1)*6)+K,2,I)+
     $       ARRAY(((J-1)*6)+K+48,2,I))
          OPT_RAW(2*I,J,K)=(ARRAY(((J-1)*6)+K,4,I)+
     $       ARRAY(((J-1)*6)+K+48,4,I))-(ARRAY(((J-1)*6)+K,3,I)+
     $       ARRAY(((J-1)*6)+K+48,3,I))

*  Invert data for IR channel for which star and sky are reversed

          IF (K .EQ. 1) THEN
              OPT_RAW((2*I)-1,J,K) = -OPT_RAW((2*I)-1,J,K)
              OPT_RAW((2*I),J,K) = -OPT_RAW((2*I),J,K)
          ENDIF
         else

*      AAT Data (More than one point per beam position)

*      Zero C1 and C2

          C1 = 0.0D0
          C2 = 0.0D0
          ROTS = (N2-2)/4
          do KK = 1,N2

*      Accumulate data in C1 and C2

             IF (KK .LE. ROTS) THEN
                C1=C1+ARRAY(((J-1)*6)+K,KK,I)+
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ELSE IF (KK .GT. ROTS+1 .AND. KK .LE. 2*ROTS+1) THEN
                C1=C1-ARRAY(((J-1)*6)+K,KK,I)-
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ELSE IF (KK .GT. 2*ROTS+1 .AND. KK .LE. 3*ROTS+1) THEN
                C1=C1-ARRAY(((J-1)*6)+K,KK,I)-
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ELSE IF (KK .GT. 3*ROTS+2 .AND. KK .LE. 4*ROTS+2) THEN
                C1=C1+ARRAY(((J-1)*6)+K,KK,I)+
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ENDIF
          END DO

*   Copy to OPT_RAW arrays dividing by number of rotations which
*   have been included

          OPT_RAW(2*I-1,J,K) = C1/ROTS
          OPT_RAW(2*I,J,K) = C2/ROTS

*  Invert data for IR channel for which star and sky are reversed

          IF (K .EQ. 1) THEN
              OPT_RAW((2*I)-1,J,K) = -OPT_RAW((2*I)-1,J,K)
              OPT_RAW((2*I),J,K) = -OPT_RAW((2*I),J,K)
          ENDIF
         ENDIF

        end do !K
       end do !J
      end do !I

      NOPT=2*NCYCLE

*  Initialize polarimetry reduction

      CALL POL_INIT

*  Clear raw data array

      DO K = 1,8
          DO J=1,6
              RAW(K,J) = 0D0
          ENDDO
      ENDDO
      DO I = 1,N3*2

*  Add data into raw data array

          DO K = 1,8
              DO J = 1,6
                  RAW(K,J) = RAW(K,J) + OPT_RAW(I,K,J)
              ENDDO
          ENDDO

*  Add data into polarimetry reduction arrays. If NPTS equals 2 we do
*  this on every half cycle, but otherwise we only do it on complete
*  cycles

          IF (NPTS .EQ. 2 .OR. I/2*2 .EQ. I) THEN
              CALL POL_ADD(6,RAW,0.9D0)

*  Reduce data for this cycle

              CALL POL_CYCLE(6,C,M,QQ,UU,P,PA)
              DO K = 1,8
                 DO J=1,6
                    RAW(K,J) = 0D0
                 ENDDO
              ENDDO
              IF (NPTS .EQ. 2) THEN
                  J = I
              ELSE
                  J = I/2
              ENDIF

*  Calibrate data and write to output arrays
              DO K=1,6
                  OUT(K,J) = C(K)/PHOT(K)
                  PA(K) = PA(K) - ABS(CPA(K))
                  IF (PA(K) .LT. 0.0) PA(K) = PA(K)+180.0
                  IF (CPA(K) .LT. 0.0) PA(K) = 180.0-PA(K)
                  QQ(K) = P(K)*COS(2.0*PA(K)/DEGRAD)
                  UU(K) = P(K)*SIN(2.0*PA(K)/DEGRAD)
                  Q(K,J) = QQ(K)*OUT(K,J)/(100.0*EFF(K))
                  U(K,J) = UU(K)*OUT(K,J)/(100.0*EFF(K))
              ENDDO
          ENDIF
      ENDDO

*  Reduce the cululative data for the run

      CALL POL_REDUCE(6,C,CE,M,ME,QQ,QQE,UU,UUE,P,PE,PA,PAE)
      STATUS = 0

*  Encode into message tokens

      DO I=1,6
          CALL MSG_FMTR('C','F10.0',C(I))
          CALL MSG_FMTR('CE','F7.0',CE(I))
          CALL MSG_FMTR('P','F7.2',P(I))
          CALL MSG_FMTR('PE','F6.2',PE(I))
          CALL MSG_FMTR('PA','F8.2',PA(I))
          CALL MSG_FMTR('PAE','F8.2',PAE(I))

*  And output to the user

          CALL MSG_OUT(' ','^C+/-^CE  P ^P+/-^PE  Theta ^PA+/-^PAE',
     :           STATUS)
      ENDDO

*  Encode into message tokens

      CALL MSG_OUT(' ','Calibrated Data',STATUS)
      DO I=1,6
          PA(I) = PA(I) - ABS(CPA(I))
          IF (PA(I) .LT. 0.0) PA(I) = PA(I)+180.0
          IF (CPA(I) .LT. 0.0) PA(I) = 180.0-PA(I)
          P(I) = P(I)/EFF(I)
          PE(I) = PE(I)/EFF(I)
          C(I) = C(I)/PHOT(I)
          CE(I) = CE(I)/PHOT(I)
          CALL MSG_FMTR('C','F10.0',C(I))
          CALL MSG_FMTR('CE','F7.0',CE(I))
          CALL MSG_FMTR('P','F7.2',P(I))
          CALL MSG_FMTR('PE','F6.2',PE(I))
          CALL MSG_FMTR('PA','F8.2',PA(I))
          CALL MSG_FMTR('PAE','F8.2',PAE(I))

*  And output to the user

          CALL MSG_OUT(' ','^C+/-^CE  P ^P+/-^PE  Theta ^PA+/-^PAE',
     :           STATUS)
      ENDDO
      END


      Subroutine TSP_WORK_ROUTINEC(N1,N2,N3,NPTS,EFF,PHOT,VV,WW,
     :    ARRAY,OUT,V)
*+
*
*   T S P _ W O R K _ R O U T I N E C
*
*   RHATPOL command
*
*   Reduce circular polarization data from the Hatfield polarimeter
*   Circular polarization data is obtained with a quarter-wave plate
*   modulator, and produces a modulation with a period of half the
*   rotation period. It is reduced using the routines intended for linear
*   polarization by folding the data with twice the normal period.
*
*   A call to the normal linear routine TSP_WORK_ROUTINE can be used to
*   reduce the linear data obtained with the quarter wavel plate which
*   is modulated with a period of one quarter of the rotation period, but
*   has half the efficiency of normal half-wave plate linear data.
*
*   Parameters:
*
*   (>)  N1       (Integer)          First dimension of input array
*   (>)  N2       (Integer)          Second dimension of input array
*   (>)  N3       (Integer)          Third dimension of input array
*   (>)  NPTS     (Integer)          Number of points per cycle
*   (>)  VV       (Real array(6))    Circular calibration vector 1
*   (>)  WW       (Real array(6))    Circular calibration vector 2
*   (>)  ARRAY    (Real array(N1,N2,N3))  Input data array
*   (<)  OUT      (Real array(6,N3*NPTS))  Output Intensity array
*   (<)  V        (Real array(6,N3*NPTS))  Output V array
*
*   Jeremy Bailey     3/3/1990
*
*   Based on a Figaro program (HATPOL) by Andrew Mead (ROE)
*
*+

      implicit none

*     Arguments

      integer   N1,N2,N3,NPTS
      real      ARRAY(N1,N2,N3)
      real      OUT(6,N3*NPTS),V(6,N3*NPTS)
      REAL      EFF(6),PHOT(6),VV(6),WW(6)

*     Local variables

      integer   NCHANNELS
      parameter (NCHANNELS=6)

*     Number of cycles
      INTEGER NCYCLE
      integer   I,J,K,KK,NOPT   ! counting variables

*     Arrays for data reduction, and output arrays from the POL routines

      double precision    OPT_RAW(1000,8,6),RAW(8,6),
     $          C(6),M(6),P(6),QQ(6),UU(6),PA(6),
     $          CE(6),ME(6),PE(6),QQE(6),UUE(6),PAE(6),VVV
      real      VALUE
      logical   FINISHED
      integer   DCYCLE
      integer   ROTS
      double precision    C1,C2
      INTEGER   STATUS
      character  NAME*64

*     Load data into array OPT_RAW

      NCYCLE = N3
      do I=1,NCYCLE
       do J=1,8
        do K=1,NCHANNELS
         if (N2 .EQ. 4) THEN

*  UKIRT data (One point pre beam)


          OPT_RAW((2*I)-1,J,K)=(ARRAY(((J-1)*6)+K,1,I)+
     $       ARRAY(((J-1)*6)+K+48,1,I))-(ARRAY(((J-1)*6)+K,2,I)+
     $       ARRAY(((J-1)*6)+K+48,2,I))
          OPT_RAW(2*I,J,K)=(ARRAY(((J-1)*6)+K,4,I)+
     $       ARRAY(((J-1)*6)+K+48,4,I))-(ARRAY(((J-1)*6)+K,3,I)+
     $       ARRAY(((J-1)*6)+K+48,3,I))
          IF (K .EQ. 1) THEN

*  Invert data for IR channel which has star and sky reversed

              OPT_RAW((2*I)-1,J,K) = -OPT_RAW((2*I)-1,J,K)
              OPT_RAW((2*I),J,K) = -OPT_RAW((2*I),J,K)
          ENDIF
         else

*      AAT Data  (More than one point per beam position)

*      Zero C1 and C2

          C1 = 0.0D0
          C2 = 0.0D0
          ROTS = (N2-2)/4
          do KK = 1,N2

*      Accumulate data in C1 and C2

             IF (KK .LE. ROTS) THEN
                C1=C1+ARRAY(((J-1)*6)+K,KK,I)+
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ELSE IF (KK .GT. ROTS+1 .AND. KK .LE. 2*ROTS+1) THEN
                C1=C1-ARRAY(((J-1)*6)+K,KK,I)-
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ELSE IF (KK .GT. 2*ROTS+1 .AND. KK .LE. 3*ROTS+1) THEN
                C1=C1-ARRAY(((J-1)*6)+K,KK,I)-
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ELSE IF (KK .GT. 3*ROTS+2 .AND. KK .LE. 4*ROTS+2) THEN
                C1=C1+ARRAY(((J-1)*6)+K,KK,I)+
     $              ARRAY(((J-1)*6)+K+48,KK,I)
             ENDIF
          END DO

*   Copy to OPT_RAW arrays dividing by number of rotations which
*   have been included

          OPT_RAW(2*I-1,J,K) = C1/ROTS
          OPT_RAW(2*I,J,K) = C2/ROTS
          IF (K .EQ. 1) THEN

*  Invert data for IR channel which has star and sky reversed

              OPT_RAW((2*I)-1,J,K) = -OPT_RAW((2*I)-1,J,K)
              OPT_RAW((2*I),J,K) = -OPT_RAW((2*I),J,K)
          ENDIF
         ENDIF

        end do !K
       end do !J
      end do !I

      NOPT=2*NCYCLE

*  Initialize polarimetry reduction

      CALL POL_INIT

*  Clear raw data array

      DO K = 1,8
          DO J=1,6
              RAW(K,J) = 0D0
          ENDDO
      ENDDO
      DO I = 1,N3*2

*  Add data into raw array

          DO K = 1,8
              DO J = 1,6
                  RAW((K+1)/2,J) = RAW((K+1)/2,J) + OPT_RAW(I,K,J)
                  RAW((K+1)/2+4,J) = RAW((K+1)/2+4,J) + OPT_RAW(I,K,J)
              ENDDO
          ENDDO

*  Add data into polarimetry reduction arrays. If NPTS equals 2 we do
*  this on every half cycle, but otherwise we only do it on complete
*  cycles

          IF (NPTS .EQ. 2 .OR. I/2*2 .EQ. I) THEN

              CALL POL_ADD(6,RAW,0.9D0)

*  Reduce data for current cycle

              CALL POL_CYCLE(6,C,M,QQ,UU,P,PA)
              DO K = 1,8
                 DO J=1,6
                    RAW(K,J) = 0D0
                 ENDDO
              ENDDO
              IF (NPTS .EQ. 2) THEN
                  J = I
              ELSE
                  J = I/2
              ENDIF

*  Calibrate data and copy to output arrays

              DO K=1,6
                  OUT(K,J) = 0.5*C(K)/PHOT(K)
                  V(K,J) = (QQ(K)*VV(K) + UU(K)*WW(K))*
     :                OUT(K,J)/(100.0*EFF(K))
              ENDDO
          ENDIF
      ENDDO

*  Reduce the cumulative data for this run

      CALL POL_REDUCE(6,C,CE,M,ME,QQ,QQE,UU,UUE,P,PE,PA,PAE)
      STATUS = 0
      DO I=1,6

*  Set message tokens

          CALL MSG_FMTR('C','F10.0',0.5*C(I))
          CALL MSG_FMTR('CE','F7.0',0.5*CE(I))
          CALL MSG_FMTR('V','F7.2',QQ(I))
          CALL MSG_FMTR('VE','F6.2',QQE(I))
          CALL MSG_FMTR('W','F7.2',UU(I))
          CALL MSG_FMTR('WE','F6.2',UUE(I))
          VVV = VV(I)*QQ(I) + WW(I)*UU(I)

*  Output results to user

          CALL MSG_OUT(' ','^C+/-^CE  V ^V+/-^VE  W ^W+/-^WE',
     :           STATUS)
      ENDDO
      END
