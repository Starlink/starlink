C+
      SUBROUTINE RIRPS(STATUS)
C
C            R I R P S
C
C     Command name:
C        RIRPS
C
C     Function:
C        Read IRPS Photometry Data
C
C     Description:
C        RIRPS reads photometry data files in Figaro format as produced
C        by the IRPS (AAO Infrared Photometer Spectrometer) ADAM system.
C        A time series dataset is created. Either P1 or P4 data may be
C        read.
C
C     Parameters:
C    (1) FIGARO     (Char)     The IRPS Figaro file to read.
C    (2) NDWELLS    (Integer)  The Number of IRPS dwells to use for each
C                               data point (1,2 or 4).
C    (3) OUTPUT     (TSP, 2D)  The output time series dataset.
C        ZEROPT     (Real)     Magnitude zero point.
C
C     Support:
C          Jeremy Bailey, AAO
C
C     Version date:
C          27/2/1988
C
C-
C
C  History:
C    Nov/1987   Original Version.   JAB/AAO
C    27/2/1988   TSP Monolith version.  JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Input pointer
      INTEGER IPTR

*  Output intensity pointer
      INTEGER OIPTR

*  X axis pointer
      INTEGER XPTR

*  Number of elements in input array
      INTEGER NELM

*  Dimensions of input array
      INTEGER NDIM, DIMS(7)

*  File name length
      INTEGER LENNAME

*  Dimensions of input array
      INTEGER NPOINTS,NCYCLES,NDWELLS
      INTEGER I
      INTEGER NSTRT

*  Time conversion variables
      INTEGER IH,IM,IY,ID

*  IRPS parameters
      REAL FL,AG,IT,ZPT

*  MJD calculation variables
      DOUBLE PRECISION SEC,DJ1,DJ2,DJM
      INTEGER J

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,ALOC

*  File name
      CHARACTER*80 FNAME

*  Label and units of data
      CHARACTER*80 LABEL,UNITS

*  UT time and data strings from FITS header
      CHARACTER*80 UTSTART,UTEND,UTDATE

*  DSA slot number
      INTEGER SL

*  Comment string from FITS header
      CHARACTER*64 COM

*  Axis pointer
      INTEGER APTR

*  ICH function
      INTEGER ICH_LEN

*  Access the Figaro frame

      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      LENNAME = ICH_LEN(FNAME)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME(:LENNAME),STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN
*  Find size of data array
         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,NELM,STATUS)

*  Check that it is three dimensional
         IF (NDIM .NE. 3) THEN
            CALL MSG_OUT(' ','Dimensions of Input File Invalid',
     :          STATUS)
            STATUS = USER__001
         ELSE

*  Copy dimensions
            NPOINTS = DIMS(1)
            NCYCLES = DIMS(3)

*  Map the data
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,SL,STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_OUT(' ','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Get information from FITS header of input file

*  Get object name
            CALL DSA_GET_FITS_C('INPUT','OBJECT',0,LABEL,COM,STATUS)

*  Get Start and end UT for data
            CALL DSA_GET_FITS_C('INPUT','UTDATE',0,UTDATE,COM,STATUS)
            CALL DSA_GET_FITS_C('INPUT','UTSTART',0,UTSTART,COM,
     :             STATUS)
            CALL DSA_GET_FITS_C('INPUT','UTEND',0,UTEND,COM,STATUS)

*  Get filter wavelength
            CALL DSA_GET_FITS_F('INPUT','IRPS_FW',0,FL,COM,STATUS)
            FL = FL*10000

*  Get IRPS amplifier gain
            CALL DSA_GET_FITS_F('INPUT','IRPS_AG',0,AG,COM,STATUS)

*  Get IRPS integration time
            CALL DSA_GET_FITS_F('INPUT','IRPS_IT',0,IT,COM,STATUS)

*  Replace colons in time/date strings with spaces
            DO I=1,64
                IF (UTSTART(I:I) .EQ. ':') UTSTART(I:I)=' '
                IF (UTEND(I:I) .EQ. ':') UTEND(I:I)=' '
                IF (UTDATE(I:I) .EQ. '/') UTDATE(I:I)=' '
            ENDDO

*  Decode UT start time string
            NSTRT = 1

*  Hours
            CALL SLA_INTIN(UTSTART,NSTRT,IH,J)

*  Minutes
            CALL SLA_INTIN(UTSTART,NSTRT,IM,J)

*  Seconds
            CALL SLA_DFLTIN(UTSTART,NSTRT,SEC,J)

*  Calculate fraction of day
            CALL SLA_DTF2D(IH,IM,SEC,DJ1,J)

*  Decode UT end time string
            NSTRT = 1

*  Hours
            CALL SLA_INTIN(UTEND,NSTRT,IH,J)

*  Minutes
            CALL SLA_INTIN(UTEND,NSTRT,IM,J)

*  Seconds
            CALL SLA_DFLTIN(UTEND,NSTRT,SEC,J)

*  Calculate fraction of day
            CALL SLA_DTF2D(IH,IM,SEC,DJ2,J)

*  Decode UT date string
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

*  Get number of dwells per points
            CALL PAR_GET0I('NDWELLS',NDWELLS,STATUS)

*  Get the output file

             CALL DAT_CREAT('OUTPUT','ENDIF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Zero point

             CALL PAR_GET0R('ZEROPT',ZPT,STATUS)

*  Create the output structure

             DIMS(1) = 1
             DIMS(2) = (4/NDWELLS)*NCYCLES
             CALL TSP_CREATE_2D(OLOC,1,DIMS(2),' ',.FALSE.,.FALSE.,
     :              STATUS)

*  Map the data array
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Map the wavelength axis array
             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',APTR,ALOC,STATUS)

*  Set the wavelength
             IF (STATUS .EQ. SAI__OK) THEN
                 CALL GEN_MOVE(4,FL,%VAL(APTR))
             ENDIF

*  Unmap the wavelength axis array
             CALL TSP_UNMAP(ALOC,STATUS)

*  Map the tiem axis
             CALL TSP_MAP_TIME(OLOC,'WRITE',XPTR,XLOC,STATUS)

*  Set the label and units of the time axis
             CALL TSP_WLU_TIME(OLOC,'MJD(UTC)','Days',STATUS)

*  Copy the data

             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_RIRPS_COPY(NCYCLES,NPOINTS,NDWELLS,AG,IT,ZPT,
     :           FL,DJ1,DJ2,%VAL(IPTR),%VAL(OIPTR),%VAL(XPTR))
             ENDIF

*  Unmap output arrays and annul locators

             CALL TSP_UNMAP(ILOC,STATUS)
             CALL TSP_UNMAP(XLOC,STATUS)
             CALL DAT_ANNUL(OLOC,STATUS)

          ENDIF
100       CONTINUE
          CALL DSA_CLOSE(STATUS)
      ENDIF
      END




      SUBROUTINE TSP_RIRPS_COPY(NCYCLES,NPOINTS,NDWELLS,AG,IT,ZPT,
     :     FW,JD1,JD2,IN,OUT,TIMES)
*+
*
*   T S P _ R I R P S _ C O P Y
*
*   RIRPS command
*
*   Copy IRPS data from its original data array to a TSP time
*   series dataset
*
*   Parameters:
*
*   (>)  NCYCLES    (Integer)              Number of beamswitch cycles
*   (>)  NPOINTS    (Integer)              Number of  points
*   (>)  NDWELLS    (Integer)              Number of dwells per bin
*   (>)  AG         (Real)                 IRPS amplifier gain
*   (>)  IT         (Real)                 IRPS integration time
*   (>)  ZPT        (Real)                 Photometric zero point
*   (>)  FW         (Real)                 Filter wavelength
*   (>)  JD1        (Double)               Starting MJD
*   (>)  JD2        (Double)               Ending MJD
*   (>)  IN         (Real array(NPOINTS,4,NCYCLES)   Input data array
*   (<)  OUT        (Real array(NCYCLES*(4/NDWELLS)) Output array
*   (<)  TIMES      (Real array(NCYCLES*(4?NDWELLS)) Time axis array
*
*   Jeremy Bailey    27/2/1988
*
*   Modified:
*      16/12/1991
*
*+

      IMPLICIT NONE

*  Parameters
      INTEGER NCYCLES,NPOINTS,NDWELLS
      DOUBLE PRECISION JD1,JD2,TIMES(NCYCLES*(4/NDWELLS))
      REAL IN(NPOINTS,4,NCYCLES),OUT(NCYCLES*(4/NDWELLS))
      REAL AG,IT,ZPT,FW

*  Local variables
      INTEGER I,J,K,ND
      REAL SX(4)
      REAL FACTOR
      DOUBLE PRECISION JD,JDS

*  Calculate number of points per cycle
      ND = 4/NDWELLS

*  Calculate duration per point
      JDS = (JD2-JD1)/(NCYCLES*ND)

*  Calculate gain factor
      FACTOR = 2*IT*AG

*  Start MJD
      JD = JD1-JDS/2.0

*  Loop over cycles
      DO I=1,NCYCLES

*  Zero accumulation arrays
          DO K=1,4
              SX(K)=0.0
          ENDDO

*  Loop over points adding into arrays
          DO J=1,NPOINTS,2
              DO K=1,4
                  SX(K) = SX(K)+IN(J,K,I)
              ENDDO
          ENDDO

*  Divide by number of points
          DO K=1,4
              SX(K) = 2.0*SX(K)/NPOINTS
          ENDDO

*  For NDWELLS equals 1 invert points belonging to second beam
*  Copy into data array and fill time array
          IF (NDWELLS .EQ. 1) THEN
              OUT(I*ND-3) = 2.0*SX(1)
              TIMES(I*ND-3) = JD
              JD = JD+JDS
              OUT(I*ND-2) = -2.0*SX(2)
              TIMES(I*ND-2) = JD
              JD = JD+JDS
              OUT(I*ND-1) = -2.0*SX(3)
              TIMES(I*ND-1) = JD
              JD = JD+JDS
              OUT(I*ND)   = 2.0*SX(4)
              TIMES(I*ND) = JD
              JD = JD+JDS

*  For NDWELLS equals 2 combine data in star/sky pairs
          ELSE IF (NDWELLS .EQ. 2) THEN
              OUT(I*ND-1) = SX(1)-SX(2)
              TIMES(I*ND-1) = JD
              JD = JD+JDS
              OUT(I*ND) = SX(4)-SX(3)
              TIMES(I*ND) = JD
              JD = JD+JDS

*  For NDWELLS equals 4 combine all four beam positions
          ELSE IF (NDWELLS .EQ. 4) THEN
              TIMES(I) = JD
              JD = JD+JDS
              OUT(I) = 0.5*(SX(1)-SX(2)-SX(3)+SX(4))
          ENDIF
      ENDDO

*  Scale data and convert to magnitude
      DO I=1,NCYCLES*ND
          OUT(I)=OUT(I)/FACTOR
          OUT(I)=-2.5*ALOG10(OUT(I))
          OUT(I)=OUT(I)+ZPT

*  Convert it back to flux using appropriate zero point for wavelength
          IF (FW .EQ. 12000) THEN
              OUT(I) = 1640 / (10**(OUT(I)/2.5))
          ELSE IF (FW .EQ. 16400) THEN
              OUT(I) = 1030 / (10**(OUT(I)/2.5))
          ELSE IF (FW .EQ. 22000) THEN
              OUT(I) = 650 / (10**(OUT(I)/2.5))
          ENDIF
      ENDDO
      END

