C+
      SUBROUTINE RCCDTS(STATUS)
C
C            R C C D T S
C
C     Command name:
C        RCCDTS
C
C     Function:
C        Read AAO CCD Time Series data
C
C     Description:
C        Read an AAO CCD time series data set from the raw figaro file
C        and build a 3D TSP dataset.
C
C        The AAO time series mode takes a time series of data by shifting
C        data out of the CCD on some regular period. The whole time series
C        is treated as a single readout and therefore appears as a two
C        dimensional array in which slices of the array are each indivdual
C        frames of the time series. RCCDTS takes these frames out of the 2D
C        array and builds a 3D TSP dataset to represent the resulting time
C        series image. It also creates a time axis from the timing information
C        contained in the FITS header.
C
C     Parameters:
C    (1) FIGARO     (Char)     The Figaro file containing the time series data
C    (2) OUTPUT     (TSP, 3D)  The output time series dataset.
C
C     Support: Jeremy Bailey, JAC
C
C     Version date: 26/10/1989
C
C-
C
C  History:
C    26/10/1989   Original Version.   JAB/JAC
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  Data pointers
      INTEGER IPTR,OPTR,TPTR

      INTEGER NFRAMES,NDIM,ELEMENTS,ISLOT,FRAME
      CHARACTER*64 FNAME

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,OLOC2,TLOC
      INTEGER DIMS(3)
      DOUBLE PRECISION MJD,PERIOD,D
      CHARACTER*64 COM,UTDATE,UTSTART


*  Get the Figaro file
      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)

*  Find its size
      CALL DSA_DATA_SIZE('INPUT',3,NDIM,DIMS,ELEMENTS,STATUS)

*  Get number of cycles
      IF (NDIM .EQ. 2) THEN
         CALL DSA_GET_FITS_I('INPUT','CCD_NSW',0,NFRAMES,COM,STATUS)
      ELSE
         NFRAMES=DIMS(3)
      ENDIF

*  Create the output data set
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

      IF (NDIM .EQ. 2) THEN
         CALL TSP_CREATE_3D(OLOC,DIMS(1),DIMS(2)/NFRAMES,NFRAMES,' ',
     :              .FALSE.,.FALSE.,STATUS)
      ELSE
         CALL TSP_CREATE_3D(OLOC,DIMS(1),DIMS(2),DIMS(3),' ',
     :              .FALSE.,.FALSE.,STATUS)
      ENDIF

*  Map the input data
      CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,ISLOT,STATUS)

*  Map the output data
      CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)

*  Map the time axis of the output data
      CALL TSP_MAP_TIME(OLOC,'WRITE',TPTR,TLOC,STATUS)

*  Set the label and units of the output time axis
      CALL TSP_WLU_TIME(OLOC,'MJD(UTC)','Days',STATUS)

*  Get timing information from the FITS header of the input frame
      CALL DSA_GET_FITS_C('INPUT','UTDATE',0,UTDATE,COM,STATUS)
      CALL DSA_GET_FITS_C('INPUT','UTSTART',0,UTSTART,COM,STATUS)
      PRINT *,UTDATE,' ',UTSTART,STATUS
      CALL DSA_GET_FITS_D('INPUT','PERIOD',0,PERIOD,COM,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
          STATUS = SAI__OK
          CALL PAR_GET0D('PERIOD',PERIOD,STATUS)
      ENDIF

*  Convert period to days
      PERIOD = PERIOD/(86400D0)

*  Calculate start MJD
      CALL TCV_S2MJD(UTDATE,MJD,STATUS)
      CALL TCV_S2TIME(UTSTART,D,STATUS)
      MJD = MJD+D

*  Copy the data
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_RCCDTS_COPY(ELEMENTS,NFRAMES,%VAL(IPTR),
     :         %VAL(OPTR),MJD,PERIOD,%VAL(TPTR))
      ENDIF

*  Tidy up
      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DSA_CLOSE(STATUS)
      END


      SUBROUTINE TSP_RCCDTS_COPY(N,NFRAMES,IN,OUT,MJD,PERIOD,TIME)
*+
*
*   T S P _ R C C D T S _ C O P Y
*
*   RCCDTS command
*
*   Copy data from the original 2D frame to the output 3D frame. Note
*   that nothing actually needs doing to the data. It is already in the
*   correct form for the 3D array and merely needs copying into it. The
*   arrays are treated as 1 dimensional in this routine.
*
*   Parameters:
*
*   (>)  N       (Integer)         Size of the data arrays
*   (>)  NFRAMES (Integer)         Number of frames (time slices) in the data
*   (>)  IN      (Real array(N))   The input array (really 2D)
*   (<)  OUT     (Real array(N))   The output array (really 3D)
*   (>)  MJD     (Double)          Modified Julian Date of start
*   (>)  PERIOD  (Double)          Period between frames (days)
*   (>)  TIME    (Double array(NFRAMES))  Time axis of output data
*
*   Jeremy Bailey   26/10/1989
*
*   Modified:
*      13/12/1991
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER N,NFRAMES
      REAL IN(N), OUT(N)
      DOUBLE PRECISION MJD,PERIOD,TIME(NFRAMES)

*  Local variables
      INTEGER I

*  Copy input data to output
      DO I = 1,N
         OUT(I) = IN(I)
      ENDDO

*  Fill the time axis with MJD of each frame
      DO I = 1,NFRAMES
         TIME(I) = MJD+PERIOD*(DBLE(I)-0.5D0)
      ENDDO
      END
