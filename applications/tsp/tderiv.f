C+
      SUBROUTINE TDERIV(STATUS)
C
C            T D E R I V
C
C     Command name:
C        TDERIV
C
C     Function:
C        Calculate Time Derivative of a Dataset.
C
C     Description:
C        TDERIV calculates the time derivative of the intensity
C        data in a dataset and outputs it as a new dataset.
C        For each point in the time series the slope of a straight
C        line fitted through n points is used to obtain the derivative.
C
C     Parameters:
C    (1) INPUT      (TSP, 2D)  The input time series dataset.
C    (2) NPOINTS    (Integer)  Number of points for line fit.
C    (3) OUTPUT     (TSP, 2D)  The output corrected dataset.
C
C     Support:
C        Jeremy Bailey, AAO
C
C     Version date:
C        1/3/1988
C
C-
C
C  History:
C    1/3/1988   Original Version.   JAB/AAO
C

      IMPLICIT NONE

*  Status argument
      INTEGER STATUS
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,LOC2,ILOC,OLOC,TLOC,TILOC

*  Dimensions of data
      INTEGER DIMS(2),ACTDIM

*  Data pointers
      INTEGER IPTR,OPTR,TPTR,TIPTR
      INTEGER NPTS


*  Get the input data
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  get the number of points for the line fit
      CALL PAR_GET0I('NPOINTS',NPTS,STATUS)

*  Create the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',LOC2,STATUS)

*  Copy input to output
      CALL TSP_COPY(LOC,LOC2,STATUS)

*  Get size of data
      CALL TSP_SIZE(LOC2,2,DIMS,ACTDIM,STATUS)

*  Map the input data
      CALL TSP_MAP_DATA(LOC,'READ',IPTR,ILOC,STATUS)

*  Map the output data
      CALL TSP_MAP_DATA(LOC2,'WRITE',OPTR,OLOC,STATUS)

*  Map the input time axis
      CALL TSP_MAP_TIME(LOC,'READ',TIPTR,TILOC,STATUS)

*  Map the output time axis
      CALL TSP_MAP_TIME(LOC2,'UPDATE',TPTR,TLOC,STATUS)

*  Calculate the time derivative dataset
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_TDERIV(DIMS(1),DIMS(2),NPTS,%VAL(IPTR),
     :             %VAL(OPTR),%VAL(TPTR),%VAL(TIPTR))
      ENDIF

*  Tidy up
      CALL TSP_UNMAP(ILOC,STATUS)
      CALL TSP_UNMAP(OLOC,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL TSP_UNMAP(TILOC,STATUS)
      CALL DAT_ANNUL(LOC2,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)

      END


      SUBROUTINE TSP_TDERIV(NC,NT,NPTS,IN,OUT,X,XI)
*+
*
*   T S P _ T D E R I V
*
*   TDERIV command
*
*   Calculate the time derivative of an input array
*
*   Parameters:
*
*   (>)  NC    (Integer)            Number of channels
*   (>)  NT    (Integer)            Number of time bins
*   (>)  NPTS  (Integer)            Number of points to calculate line fit over
*   (>)  IN    (Real array(NC,NT))  Input array
*   (<)  OUT   (Real array(NC,NT))  Output array
*   (>)  X     (Double array(NT))   Output X axis array
*   (>)  XI    (Double array(NT))   Input X axis array
*
*   Jeremy Bailey    1/3/1988
*
*+

*  Parameters
      INTEGER NC,NT,NPTS
      REAL IN(NC,NT), OUT(NC,NT)
      DOUBLE PRECISION X(NT),XI(NT)

*  Local variables
      INTEGER I,J

*  Zero values at end of range
      DO I=1,NC
         DO J=1,NPTS/2
            OUT(I,J) = 0.0
            OUT(I,NT-J+1) = 0.0
         ENDDO
      ENDDO

*  Loop over remaining points fitting straight lines
      DO J = 1+NPTS/2, NT-NPTS/2
         DO I=1,NC
            CALL TSP_LFIT(NPTS,NC,I,XI(J-NPTS/2),IN(1,J-NPTS/2),
     :            X(J),OUT(I,J))
         ENDDO
      ENDDO
      END


      SUBROUTINE TSP_LFIT(N,NC,C,X,Y,XM,SLOPE)
*+
*
*   T S P _ L F I T
*
*   TDERIV command
*
*   Fit a straight line to a region of data from the input array
*   and return its slope
*
*   Parameters:
*
*   (>)  N      (Integer)            Number of points in time series
*   (>)  NC     (Integer)            Number of channels in time series
*   (>)  C      (Integer)            Channel to fit to
*   (>)  X      (Double array(N))    Time axis array
*   (>)  Y      (Real array(NC,N))   Main input array
*   (<)  XM     (Double)             Mean X value
*   (<)  SLOPE  (Real)               Slope of straight line fit
*
*   Jeremy Bailey    1/3/1988
*
*   Modified:
*       17/12/1991   -  Handle bad values
*
*+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER N,NC,C
      DOUBLE PRECISION X(N)
      REAL Y(NC,N)
      DOUBLE PRECISION XM
      REAL SLOPE

*  Local variables
      DOUBLE PRECISION SL
      DOUBLE PRECISION SX,SY,SXX,SXY
      INTEGER I
      DOUBLE PRECISION DELTA

*  Initialize sums
      SX=0.0D0
      SY=0.0D0
      SXX=0.0D0
      SXY=0.0D0

*  Loop over data points
      DO I=1,N
        IF (Y(C,I) .NE. VAL__BADR) THEN
          SX = SX + (X(I)-X(1))
          SXX = SXX + (X(I)-X(1))*(X(I)-X(1))
          SY = SY + Y(C,I)
          SXY = SXY + (X(I)-X(1))*Y(C,I)
        ENDIF
      ENDDO

*  Calculate results
      DELTA = N*SXX - SX*SX
      SL = (N*SXY - SX*SY)/DELTA
      SLOPE = SL / 86400.0
      XM = SX/N + X(1)

      END
