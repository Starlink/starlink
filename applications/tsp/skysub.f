C+
      SUBROUTINE SKYSUB(STATUS)
C
C            S K Y S U B
C
C     Command name:
C        SKYSUB
C
C     Function:
C        Subtract Sky from a time series image dataset
C
C     Description:
C        Sky subtraction from each frame of a time series image is
C        performed by selecting two areas on each side of a star to
C        be observed and linearly interpolating between the mean or
C        median of the values in these.
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series dataset to be sky subtracted.
C    (2) OUTPUT    (TSP, 3D)   The dataset after sky subtraction.
C    (3) Y1        (Integer)   Lowest Y value to use
C    (4) Y2        (Integer)   Highest Y value to use
C    (5) XL1       (Integer)   Lowest X value for left sky region
C    (6) XL2       (Integer)   Highest X value for left sky region
C    (7) XR1       (Integer)   Lowest X value for right sky region
C    (8) XR2       (Integer)   Highest X value for right sky region
C    (9) MEDIAN    (Logical)   Use median rather than mean
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         26/10/1989
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

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,LOC2,TLOC

*  Data pointers
      INTEGER PTR,TPTR
      INTEGER Y1,Y2,XL1,XL2,XR1,XR2
      LOGICAL MEDIAN
      INTEGER DIMS(3),NDIMS

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Create the output data set
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Copy the input dataset to the output
      CALL TSP_COPY(LOC,OLOC,STATUS)
      CALL TSP_MAP_DATA(OLOC,'UPDATE',PTR,LOC2,STATUS)
      CALL TSP_SIZE(OLOC,3,DIMS,NDIMS,STATUS)

*  Create temporary file for median calculation
      CALL TSP_TEMP(DIMS(1)*DIMS(2),'_REAL',TPTR,TLOC,STATUS)

*  Get parameters selecting sky region
      CALL PAR_GET0I('Y1',Y1,STATUS)
      CALL PAR_GET0I('Y2',Y2,STATUS)
      CALL PAR_GET0I('XL1',XL1,STATUS)
      CALL PAR_GET0I('XL2',XL2,STATUS)
      CALL PAR_GET0I('XR1',XR1,STATUS)
      CALL PAR_GET0I('XR2',XR2,STATUS)
      CALL PAR_GET0L('MEDIAN',MEDIAN,STATUS)

*  Do the sky subtraction
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_SKYSUB(DIMS(1),DIMS(2),DIMS(3),Y1,Y2,XL1,XL2,XR1,XR2,
     :         %VAL(PTR),MEDIAN,DIMS(1)*DIMS(2),%VAL(TPTR))
      ENDIF

*  Tidy up
      CALL TSP_UNMAP(LOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END


      SUBROUTINE TSP_SKYSUB(N1,N2,N3,Y1,Y2,XL1,XL2,XR1,XR2,X,MEDIAN,
     :     NW,W)
*+
*
*   T S P _ S K Y S U B
*
*   SKYSUB command
*
*   Subroutine to do the sky subtraction
*
*   Parameters:
*
*   (>)   N1      (Integer)         First dimension of data array
*   (>)   N2      (Integer)         Second dimension of data array
*   (>)   N3      (Integer)         Third dimension of data array
*   (>)   Y1      (Integer)         First Y value to use
*   (>)   Y2      (Integer)         Last Y value to use
*   (>)   XL1     (Integer)         First X value for left sky region
*   (>)   XL2     (Integer)         Last X value for left sky region
*   (>)   XR1     (Integer)         First X value for right sky region
*   (>)   XR2     (Integer)         Last X value for right sky region
*   (!)   X       (Real array(N1,N2,N3))   Data array
*   (>)   MEDIAN  (Logical)         If TRUE use median rather than mean
*   (>)   NW      (Integer)         Size of workspace array
*   (W)   W       (Real array(NW))  Workspace array
*
*   Jeremy Bailey     26/10/1989
*
*   Modified:
*       16/12/1991
*+


      IMPLICIT NONE

*  Parameters
      INTEGER N1,N2,N3
      REAL X(N1,N2,N3)
      INTEGER Y1,Y2,XL1,XL2,XR1,XR2
      LOGICAL MEDIAN
      INTEGER NW
      REAL W(NW)

*  Local variables
      INTEGER I1,I2,I3,J
      REAL SUML,SUMR,POSL,POSR,FACTOR

*  Loop over frames
      DO I3=1,N3
          SUML=0.0
          SUMR=0.0

*  Form median or mean of left sky area
          IF (MEDIAN) THEN
              J=1

*  Copy data into workspace array
              DO I2=Y1,Y2
                  DO I1=XL1,XL2
                      W(J)=X(I1,I2,I3)
                      J=J+1
                  ENDDO
              ENDDO

*  Sort the array
              CALL TSP_SORT(J-1,W)

*  And select middle value (or average of two middle value if there are an
*  even number
              IF (J/2*2 .EQ. J) THEN
                  SUML = W(J/2)
              ELSE
                  SUML = 0.5*(W(J/2)+W(J/2+1))
              ENDIF
          ELSE

*  Otherwise form mean

*  Sum data points
              DO I2=Y1,Y2
                  DO I1=XL1,XL2
                      SUML=SUML+X(I1,I2,I3)
                  ENDDO
              ENDDO

*  Divide by number of points
              SUML = SUML/REAL((Y2-Y1+1)*(XL2-XL1+1))
          ENDIF
          POSL = 0.5*REAL(XL2+XL1)

*  Form median or mean of right sky area
          IF (MEDIAN) THEN
              J=1

*  Copy data into workspace array
              DO I2=Y1,Y2
                  DO I1=XR1,XR2
                      W(J)=X(I1,I2,I3)
                      J=J+1
                  ENDDO
              ENDDO

*  Sort the array
              CALL TSP_SORT(J-1,W)

*  And select middle value (or average of two middle value if there are an
*  even number
              IF (J/2*2 .EQ. J) THEN
                  SUMR = W(J/2)
              ELSE
                  SUMR = 0.5*(W(J/2)+W(J/2+1))
              ENDIF
          ELSE

*  Otherwise form mean

*  Sum data points
              DO I2=Y1,Y2
                  DO I1=XR1,XR2
                      SUMR=SUMR+X(I1,I2,I3)
                  ENDDO
              ENDDO

*  Divide by number of points
              SUMR = SUMR/REAL((Y2-Y1+1)*(XR2-XR1+1))
          ENDIF
          POSR = 0.5*REAL(XR2+XR1)
          PRINT *,'Frame ',I3,' Sum Left = ',SUML,' Sum Right = ',SUMR

*  Interpolate between sky areas and subtract - FUDGE TO DO 2 MORE ROWS THAN
*  Y1 TO Y2  (27/10/89)
          FACTOR = (SUMR-SUML)/(POSR-POSL)
          DO I2=MAX(Y1-2,1),MIN(Y2+2,N2)
              DO I1 = 1,N1
                  X(I1,I2,I3)=X(I1,I2,I3)-FACTOR*(REAL(I1)-POSL)-SUML
              ENDDO
          ENDDO
      ENDDO
      END


C
      SUBROUTINE TSP_SORT(SIZE,ARRAY)
C+
C            T S P _ S O R T
C
C     Routine name
C        TSP_SORT
C
C     Function
C        Shell Sort a real array
C
C     Description
C        Sort a real array
C
C     Parameters
C        (>)  SIZE       (Integer, ref) Size of the array
C        (!)  ARRAY      (Real,ref) Array of values to sort
C
C     Support:
C          Jeremy Bailey, AAO
C
C     Version date
C           26/10/1989
C
C
C
C  History:
C    26/10/1989   Original Version.   JAB/JAC
C+

      IMPLICIT NONE


*  Parameters
      INTEGER SIZE
      REAL ARRAY(SIZE)

*  Local variables
      INTEGER GAP,I,J,JG
      REAL K

*  Set gap to half of array size
      GAP = SIZE/2

*  Loop until gap reaches zero
      DO WHILE (GAP .GT. 0)
          DO I = GAP+1,SIZE
              J = I-GAP
              DO WHILE (J .GT. 0)
                  JG = J+GAP
                  IF (ARRAY(J) .LE. ARRAY(JG)) THEN
                      J = 0
                  ELSE

*  Exchange points
                      K = ARRAY(J)
                      ARRAY(J) = ARRAY(JG)
                      ARRAY(JG) = K
                  ENDIF
                  J = J-GAP
              ENDDO
          ENDDO
          GAP = GAP/2
      ENDDO
      END

