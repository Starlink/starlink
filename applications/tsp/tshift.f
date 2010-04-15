C+
      SUBROUTINE TSHIFT(STATUS)
C
C            T S H I F T
C
C     Command name:
C        TSHIFT
C
C     Function:
C        Apply a time shift to a dataset.
C
C     Description:
C        TSHIFT adds a constant to the time axis values of a time
C        series dataset. It can be used to correct for time errors
C        in the original data.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) SHIFT      (Double)   Time shift to apply (days).
C    (3) OUTPUT     (TSP, 2D)  The output corrected dataset.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         28/2/1988
C
C-
C
C  History:
C    20/1/1988   Original Version.   JAB/AAO
C    28/2/1988   TSP Monolith version.  JAB/AAO
C

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,ALOC,OLOC

*  Array size
      INTEGER SIZE

*  Data pointer
      INTEGER PTR

*  Amount of shift
      DOUBLE PRECISION SHIFT

*  Array dimensions
      INTEGER DIMS(7),ACTDIM

*  Get Locator to the dataset
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Create the output dataset
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy input to output

      CALL TSP_COPY(LOC,OLOC,STATUS)

*  Get size of data
      CALL TSP_SIZE(OLOC,7,DIMS,ACTDIM,STATUS)
      SIZE = DIMS(ACTDIM)

*  Map the time axis
      CALL TSP_MAP_TIME(OLOC,'UPDATE',PTR,ALOC,STATUS)

*  Get Shift
      CALL PAR_GET0D('SHIFT',SHIFT,STATUS)

*  Perform Shift
      IF (STATUS .EQ. SAI__OK) THEN
         CALL TSP_SHIFT(SIZE,%VAL(PTR),SHIFT,STATUS)
      ENDIF

*  Unmap data and annul locators

      CALL TSP_UNMAP(ALOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      END



      SUBROUTINE TSP_SHIFT(SIZE,TIMES,SHIFT,STATUS)
*+
*
*  T S P _ S H I F T
*
*  TSHIFT command
*
*  Add a constant to each component of a double precision array - used
*  to apply a time shift to the time axis of a dataset
*
*  Parameters:
*
*  (>)   SIZE   (Integer)               Size of array
*  (!)   TIMES  (Double array(SIZE))    Array of values to be shifted
*  (>)   SHIFT  (Double)                Shift value to be added to each
*                                          element of the array
*  (!)   STATUS (Integer)               Status return
*
*  Jeremy Bailey  20/1/1988
*
*+
      IMPLICIT NONE

*  Parameters
      INTEGER SIZE,STATUS
      DOUBLE PRECISION TIMES(SIZE),SHIFT

*  Local variables
      INTEGER I

*  Loop over points adding shift value
      DO I=1,SIZE
          TIMES(I) = TIMES(I) + SHIFT
      ENDDO
      END

