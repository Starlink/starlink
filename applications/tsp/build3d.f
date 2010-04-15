C+
      SUBROUTINE BUILD3D(STATUS)
C
C            B U I L D 3 D
C
C     Command name:
C        BUILD3D
C
C     Function:
C        Insert a figaro frame into a time series image
C
C     Description:
C        BUILD3D is used to create a time series image from a number
C        of figaro images. Each invocation of BUILD3D inserts one frame
C        into the time series. A new time series dataset can be created
C        by specifying the NEW parameter and the required number of frames
C        The date and time of each frame is obtained from the FITS header
C        if possible - otherwise it is prompted for.
C
C     Parameters:
C    (1) FIGARO     (Char)     The Figaro files to insert.
C    (2) FRAME      (Integer)  The frame number at which to insert it.
C    (3) NEW        (Logical)  TRUE to create a new time series.
C    (4) OUTPUT     (TSP, 3D)  The output time series dataset.
C        FRAMES     (Integer)  The number of frames in the time series.
C        UTDATE     (Char)     The UT date of the frame
C        UT         (Char)     The UT time of the frame
C
C     Support: Jeremy Bailey, AAO
C
C     Version date: 07/03/1992
C
C-
C
C  History:
C    19/10/1989   Original Version.   JAB/JAC
C    27/10/1989   Obtain time from header  JAB/JAC
C    07/03/1992   Modify to insert one frame each time  JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  Local variables
      INTEGER NFRAMES,NDIM,ELEMENTS,IPTR,ISLOT,OPTR,FRAME,TPTR
      CHARACTER*64 FNAME

*  HDS locators
      CHARACTER*(DAT__SZLOC) OLOC,OLOC2,TLOC
      INTEGER DIMS(2)
      DOUBLE PRECISION MJD
      LOGICAL NEW

*  Get the Figaro frame
      CALL PAR_GET0C('FIGARO',FNAME,STATUS)
      CALL DSA_OPEN(STATUS)
      CALL DSA_NAMED_INPUT('INPUT',FNAME,STATUS)
      CALL DSA_DATA_SIZE('INPUT',2,NDIM,DIMS,ELEMENTS,STATUS)

*  Get frame number
      CALL PAR_GET0I('FRAME',FRAME,STATUS)

*  Get NEW parameter
      CALL PAR_GET0L('NEW',NEW,STATUS)

*  Get the output file
      IF (NEW) THEN
          CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
          CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Get number of frames
          CALL PAR_GET0I('FRAMES',NFRAMES,STATUS)

*  Create the output data set
          CALL TSP_CREATE_3D(OLOC,DIMS(1),DIMS(2),NFRAMES,' ',.FALSE.,
     :              .FALSE.,STATUS)
          CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)
      ELSE
          CALL DAT_ASSOC('OUTPUT','UPDATE',OLOC,STATUS)
          CALL TSP_MAP_DATA(OLOC,'UPDATE',OPTR,OLOC2,STATUS)
      ENDIF

*  Map the input and output data
      CALL DSA_MAP_DATA('INPUT','READ','FLOAT',IPTR,ISLOT,STATUS)

*  Map the time axis data
      CALL TSP_MAP_TIME(OLOC,'WRITE',TPTR,TLOC,STATUS)

*  Write the label and units of time axis
      CALL TSP_WLU_TIME(OLOC,'MJD(UTC)','Days',STATUS)
      CALL TSP_BUILD3D_TIME(MJD,STATUS)

*  Do the copy for the frame
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_BUILD3D_COPY(DIMS(1),DIMS(2),
     :         NFRAMES,FRAME,%VAL(IPTR),
     :         %VAL(OPTR),MJD,%VAL(TPTR))
      ENDIF

*  Tidy up
      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DSA_CLOSE(STATUS)
      END


      SUBROUTINE TSP_BUILD3D_COPY(N1,N2,N3,N,IN,OUT,MJD,TIME)
*+
*     Copy an input 2D frame into a slice of a 3D array, and
*     its MJD into the time axis of the output array
*
*     (>)  N1   (Integer)  First dimensionn of 3D array
*     (>)  N2   (Integer)  Second dimension of 3D array
*     (>)  N3   (Integer)  Third dimension of 3D array
*     (>)  N    (Integer)  Slice number to insert into
*     (>)  IN   (Real array(N1,N2)) Input 2D array
*     (<)  OUT  (Real array(N1,N2,N3)) Output 3D array
*     (>)  MJD  (Double)   MJD of frame
*     (<)  TIME (Double array(N3)) Time axis of output array
*
*+
      INTEGER N1,N2,N3,N
      REAL IN(N1,N2), OUT(N1,N2,N3)
      INTEGER I1,I2
      DOUBLE PRECISION MJD,TIME(N3)

*  Copy the data
      DO I2 = 1,N2
         DO I1 = 1,N1
            OUT(I1,I2,N) = IN(I1,I2)
         ENDDO
      ENDDO

*  Copy the MJD
      TIME(N)=MJD
      END


      SUBROUTINE TSP_BUILD3D_TIME(MJD,STATUS)

*+
*
*   T S P _ B U I L D 3 D _ T I M E
*
*   BUILD3D command
*
*   Get the MJD (Modified Julian Date) of a Figaro frame for use
*   by BUILD3D
*
*   First try to extract the necessary information from the FITS header
*   (assuming that the header contains the standard items UTDATE,
*   UTSTART and EXPOSED according to the AAO standard). Convert these
*   to an MJD for the beginning of the exposure.
*
*   If this fails ask the user for a UT date and time and convert these
*   to an MJD.
*
*   Parameters:
*
*   (<)  MJD    (Double)  Modified Julian Date of the frame
*   (!)  STATUS (Integer)  Status value
*
*   Jeremy Bailey     27/10/1989
*
*   Modified:
*       17/1/1992
*+

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Parameters
      DOUBLE PRECISION MJD
      INTEGER STATUS

*  Local variables
      LOGICAL FINISHED
      CHARACTER*80 UTDATE,UTC,COM
      DOUBLE PRECISION D,EXP

      FINISHED = .FALSE.

*  First try to get date and time from the FITS header
      CALL DSA_GET_FITS_C('INPUT','UTDATE',0,UTDATE,COM,STATUS)
      CALL DSA_GET_FITS_C('INPUT','UTSTART',0,UTC,COM,STATUS)
      CALL DSA_GET_FITS_D('INPUT','EXPOSED',0,EXP,COM,STATUS)

*  Convert the UT date and time to an MJD - First the data
      CALL TCV_S2MJD(UTDATE,MJD,STATUS)

*  Then convert the time to a fraction of a day
      CALL TCV_S2TIME(UTC,D,STATUS)
      IF (STATUS .EQ. SAI__OK) THEN

*  ... And if no errors add on to the MJD, and add half the exposure
*      time to get the time of mid exposure
          MJD = MJD+D+0.5D0*EXP/86400D0
      ELSE

*  If not found in header prompt user
          STATUS = SAI__OK
          DO WHILE (.NOT. FINISHED)

*  First get the UT date
              CALL PAR_GET0C('UTDATE',UTDATE,STATUS)

*  And convert to MJD
              CALL TCV_S2MJD(UTDATE,MJD,STATUS)

*  If there are errors cancel the parameter and try again
              IF (STATUS .NE. SAI__OK) THEN
                  STATUS = SAI__OK
              ELSE
                  FINISHED  = .TRUE.
              ENDIF
              CALL PAR_CANCL('UTDATE',STATUS)
          ENDDO

          FINISHED = .FALSE.
          DO WHILE (.NOT. FINISHED)

*  Then get the UT time
              CALL PAR_GET0C('UT',UTC,STATUS)

*  And convert to fraction of a day
              CALL TCV_S2TIME(UTC,D,STATUS)

*  If there are errors cancel the parameter and try again
              IF (STATUS .NE. SAI__OK) THEN
                  STATUS = SAI__OK
              ELSE
                  FINISHED  = .TRUE.
              ENDIF
              CALL PAR_CANCL('UT',STATUS)
          ENDDO

*  Combine date and time to give MJD
          MJD = MJD+D
      ENDIF
      END
