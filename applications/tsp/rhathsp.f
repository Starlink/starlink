C+
      SUBROUTINE RHATHSP(STATUS)
C
C            R H A T H S P
C
C     Command name:
C        RHATHSP
C
C     Function:
C        Read Hatfield Polarimeter High Speed Photometry Data
C
C     Description:
C        RHATHSP reads data files in Figaro format as produced
C        by the Hatfield Polarimeter at the AAT running in its
C        5 channel high speed photometry mode.
C
C        It outputs a 5 channel TSP time series dataset containing
C        the light curves in each of the five channels. An accurate
C        time axis array is created using the approximate start time
C        and the additional timing information written to the sixth
C        channel of the data array.
C
C     Parameters:
C    (1) FIGARO     (Char)     The Hatfield Figaro file to read.
C    (2) OUTPUT     (TSP, 2D)  The output time series dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         2/12/1988
C
C-
C
C  History:
C    Nov/1987   Original Version (RIRPS)   JAB/AAO
C    27/2/1988   TSP Monolith version.  JAB/AAO
C    2/12/1988   Modified from RIRPS   JAB/AAO
C    3/12/1988   Handle Timing Accurately  JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR
      INTEGER OIPTR
      INTEGER XPTR

*  Number of elements
      INTEGER NELM

*  Dimensions of data array
      INTEGER NDIM, DIMS(7)
      INTEGER LENNAME
      INTEGER NPOINTS,NCYCLES,NPHASE
      INTEGER I
      INTEGER NSTRT
      INTEGER IH,IM,IY,ID
      REAL IT,RT
      DOUBLE PRECISION SEC,DJ1,DJ2,DJM
      INTEGER J
      CHARACTER*(DAT__SZLOC) OLOC,ILOC,XLOC,ALOC
      CHARACTER*80 FNAME,LABEL,UNITS,UTSTART,UTEND,UTDATE
      LOGICAL OK
      INTEGER APTR
      INTEGER SIZE
      REAL WAVES(5)
      INTEGER SL
      CHARACTER*64 COM

      INTEGER ICH_LEN

*  Access the Figaro frame
      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      LENNAME = ICH_LEN(FNAME)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME(:LENNAME),STATUS)

*  Get the data array

      IF (STATUS .EQ. SAI__OK) THEN

*  Check that it is three dimensional
         CALL DSA_DATA_SIZE('INPUT',7,NDIM,DIMS,NELM,STATUS)
         IF (NDIM .NE. 3) THEN
            CALL MSG_OUT('MSG','Dimensions of Input File Invalid',
     :          STATUS)
            STATUS = USER__001
         ELSE

*  Copy the dimensions
            NPOINTS = DIMS(1)
            NPHASE = DIMS(2)
            NCYCLES = DIMS(3)

*  Map the data
            CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,SL,STATUS)
            IF (STATUS .NE. 0) THEN
               CALL MSG_OUT('MSG','Error Mapping Input Data',STATUS)
               GOTO 100
            ENDIF

*  Get object name
            CALL DSA_GET_FITS_C('INPUT','OBJECT',0,LABEL,COM,STATUS)

*  Get Start and end UT for data
            CALL DSA_GET_FITS_C('INPUT','UTDATE',0,UTDATE,COM,STATUS)
            CALL DSA_GET_FITS_C('INPUT','UTSTART',0,UTSTART,COM,
     :             STATUS)
            CALL DSA_GET_FITS_C('INPUT','UTEND',0,UTEND,COM,STATUS)

*  Get integration and reset time
            CALL DSA_GET_FITS_F('INPUT','IRPS_IT',0,IT,COM,STATUS)
            CALL DSA_GET_FITS_F('INPUT','IRPS_RT',0,RT,COM,STATUS)

*  Time interval between points is integration time plus reset time
            IT = IT+RT

*  Remove colons from time strings
            DO I=1,64
                IF (UTSTART(I:I) .EQ. ':') UTSTART(I:I)=' '
                IF (UTEND(I:I) .EQ. ':') UTEND(I:I)=' '
                IF (UTDATE(I:I) .EQ. '/') UTDATE(I:I)=' '
            ENDDO

*  Convert time strings
            NSTRT = 1

*  UT start hours, minutes and seconds
            CALL SLA_INTIN(UTSTART,NSTRT,IH,J)
            CALL SLA_INTIN(UTSTART,NSTRT,IM,J)
            CALL SLA_DFLTIN(UTSTART,NSTRT,SEC,J)

*  Convert to fraction of a day
            CALL SLA_DTF2D(IH,IM,SEC,DJ1,J)
            NSTRT = 1

*  UT Date Years, Month and Day
            CALL SLA_INTIN(UTDATE,NSTRT,IY,J)
            CALL SLA_INTIN(UTDATE,NSTRT,IM,J)
            CALL SLA_INTIN(UTDATE,NSTRT,ID,J)

*  Convert to MJD
            CALL SLA_CLDJ(IY,IM,ID,DJM,J)

*  Form start MJD
            DJ1 = DJM+DJ1

*  Get the output file

             CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
             CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create the structure

             DIMS(1) = 5
             DIMS(2) = NPOINTS/6*NPHASE*NCYCLES
             SIZE = DIMS(2)
             CALL TSP_CREATE_2D(OLOC,5,DIMS(2),' ',.FALSE.,.FALSE.,
     :              STATUS)

*  Map the output data array
             CALL TSP_MAP_DATA(OLOC,'WRITE',OIPTR,ILOC,STATUS)

*  Map the output wavelength array
             CALL TSP_MAP_LAMBDA(OLOC,'WRITE',APTR,ALOC,STATUS)

*  Fill the output array with the wavelengths of the optical channels
*  of the Hatfield polarimeter
             IF (STATUS .EQ. SAI__OK) THEN
                 WAVES(1) = 7900.0
                 WAVES(2) = 6400.0
                 WAVES(3) = 5500.0
                 WAVES(4) = 4400.0
                 WAVES(5) = 3600.0
                 CALL GEN_MOVE(20,WAVES,%VAL(APTR))
             ENDIF

*  Unmap the wavelength data
             CALL TSP_UNMAP(ALOC,STATUS)

*  Map the Time axis
             CALL TSP_MAP_TIME(OLOC,'WRITE',XPTR,XLOC,STATUS)

*  Set label and units of the time
             CALL TSP_WLU_TIME(OLOC,'MJD(UTC)','Days',STATUS)

*  Copy the data
             IF (STATUS .EQ. SAI__OK) THEN
                CALL TSP_RHATHSP_COPY(SIZE,
     :           DJ1,SEC,IT,%VAL(IPTR),%VAL(OIPTR),%VAL(XPTR))
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




      SUBROUTINE TSP_RHATHSP_COPY(SIZE,JD1,SEC,IT,IN,OUT,TIMES)
*+
*
*   T S P _ R H A T H S P _ C O P Y
*
*   RHATHSP command
*
*   Copy data from input file to output file. Also fill the time
*   axis of the output dataset.
*
*   Parameters:
*
*   (>)   SIZE    (Integer)          Size of data (number of time bins)
*   (>)   JD1     (Double)           Starting MJD
*   (>)   SEC     (Double)           Start UT seconds
*   (>)   IT      (Real)             Spacing of data points (milliseconds)
*   (>)   IN      (Real array(6,SIZE)) Input array
*   (<)   OUT     (Real array(5,SIZE)) Output array
*   (<)   TIMES   (Double array(SIZE))  Time (MJD) axis of output array
*
*   Jeremy Bailey   27/2/1988
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE
      DOUBLE PRECISION JD1,SEC,TIMES(SIZE)
      REAL IN(6,SIZE),OUT(5,SIZE)
      REAL IT

*  Local variables
      INTEGER I,J,K,ND
      DOUBLE PRECISION JD,JDS

*  Time per bin  (convert IT from milliseconds to days)

      JDS = IT/(1000D0 * 3600D0 * 24D0)

*  Approximate start time from header is in JD1. There will actually
*  be a few seconds delay between this and the actual start time.
*  Correct start time using second markers in 1st channel of data.

      PRINT *,SEC,IN(1,1)
      IF (IN(1,1) .LT. SEC) SEC = SEC - 60D0

*  Whole seconds correction

      JD1 = JD1 + (IN(1,1) - SEC)/(3600D0 * 24D0)
      PRINT *,'Correction = ',IN(1,1)-SEC

*  Fractions of second correction - look for second transition

      J = 2
      DO WHILE (NINT(IN(1,J)) .EQ. NINT(IN(1,1)))
         J = J+1
      ENDDO
      JD1 = JD1 + (1D0 - (J-0.5D0)*(IT/1000D0))*JDS
      PRINT *,'Correction = ',(1D0 - (J-0.5D0)*(IT/1000D0))

*  Copy data to output array and fill time array
      JD = JD1
      DO I=1,SIZE
          DO J=1,5
              OUT(J,I) = IN(J+1,I)
          ENDDO
          JD = JD+JDS
          TIMES(I) = JD
      ENDDO
      END

