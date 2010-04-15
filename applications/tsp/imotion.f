C+
      SUBROUTINE IMOTION(STATUS)
C
C           I M O T I O N
C
C     Command name:
C        IMOTION
C
C     Function:
C        Analyze the image motion in a time series image
C
C     Description:
C        Given a time series image produce an output time series
C        which is a measure of the image motion in the 2 axes.
C        The first channel of the output time series is the image
C        motion in X and the second channel is the image motion in Y
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series image dataset.
C    (2) TEMPLATE  (TSP, 2D)   An image to be used as a template against
C                                which motion will be measured.
C    (3) OUTPUT    (TSP, 2D)   The output photometry dataset.
C    (4) X         (Real)      X position of centre of star
C    (5) Y         (Real)      Y position of centre of star
C    (6) RADIUS    (Real)      Radius of aperture (pixels)
C    (7) BPIXEL    (Logical)   Use brightest pixel (rather than centroid)
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         4/5/1994
C
C-
C
C  History:
C    16/11/1991   Original Version.   JAB/JAC
C    4/5/1994     Add BPIXEL option   JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,TLOC,OTLOC,OLOC2,LOC2,LLOC
      CHARACTER*(DAT__SZLOC) PLOC,PPLOC

*  Data pointers
      INTEGER PTR,IPTR,TPTR,OTPTR,OPTR,LPTR,PPTR

*  Array dimensions
      INTEGER NDIMS,DIMS(3)
      INTEGER WPTR,ZEROPT
      REAL X,Y,RADIUS,LAMBDA
      CHARACTER*64 LABEL,UNITS
      INTEGER PNDIMS,PDIMS(3)
      LOGICAL BPIXEL

*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get size of data
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)

*  Map time axis
      CALL TSP_MAP_TIME(LOC,'READ',TPTR,TLOC,STATUS)

*  Check that data is three dimensional
      IF (NDIMS .NE. 3) THEN
          CALL MSG_OUT(' ','Input must be 3 dimensional',STATUS)
          GOTO 100
      ENDIF

*  Map the data
      CALL TSP_MAP_DATA(LOC,'READ',PTR,LOC2,STATUS)

*  Get the template file
      CALL DAT_ASSOC('TEMPLATE','READ',PLOC,STATUS)
      CALL TSP_SIZE(PLOC,3,PDIMS,PNDIMS,STATUS)

*  Check Dimensions Match
      IF (PNDIMS .NE. 2) THEN
          CALL MSG_OUT(' ','Template must be 2 dimensional',STATUS)
          GOTO 100
      ELSE IF (DIMS(1) .NE. PDIMS(1) .OR. DIMS(2) .NE. PDIMS(2)) THEN
          CALL MSG_OUT(' ',
     :         'Dimensions of INPUT and TEMPLATE are different',
     :         STATUS)
          GOTO 100
      ENDIF

*  Map the template data
      CALL TSP_MAP_DATA(PLOC,'READ',PPTR,PPLOC,STATUS)

*  Make the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

      CALL PAR_GET0R('X',X,STATUS)
      CALL PAR_GET0R('Y',Y,STATUS)
      CALL PAR_GET0R('RADIUS',RADIUS,STATUS)

      CALL PAR_GET0L('BPIXEL',BPIXEL,STATUS)

*  Create 2D data structure
      CALL TSP_CREATE_2D(OLOC,3,DIMS(3),' ',.FALSE.,.FALSE.,STATUS)

*  Map the time axis
      CALL TSP_MAP_TIME(OLOC,'WRITE',OTPTR,OTLOC,STATUS)

*  Map the data array
      CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)

*  Set Label and Units
      CALL TSP_WLU(OLOC,'Image Motion','Pixels',STATUS)

*  Copy the time axis
      IF (STATUS .EQ. SAI__OK) THEN
          CALL TSP_CCDPHOT_COPYT(DIMS(3),%VAL(TPTR),%VAL(OTPTR))
      ENDIF

*  Copy units and label of time axis
      CALL TSP_RLU_TIME(LOC,LABEL,UNITS,STATUS)
      CALL TSP_WLU_TIME(OLOC,LABEL,UNITS,STATUS)

*  Do the photometry on the frames
      IF (STATUS .EQ. SAI__OK) THEN
           CALL TSP_IMOTION(DIMS(1),DIMS(2),DIMS(3),
     :           X,Y,RADIUS,BPIXEL,%VAL(PTR),%VAL(PPTR),%VAL(OPTR),
     :           STATUS)
      ENDIF

*  Tidy up
100   CONTINUE
      CALL TSP_UNMAP(PPLOC,STATUS)
      CALL DAT_ANNUL(PLOC,STATUS)
      CALL TSP_UNMAP(LOC2,STATUS)
      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL TSP_UNMAP(TLOC,STATUS)
      CALL TSP_UNMAP(OTLOC,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END



      SUBROUTINE TSP_IMOTION(NX,NY,NZ,X,Y,RADIUS,BPIXEL,IN,
     :   TEMPLATE,OUT,STATUS)
*+
*
*  T S P _ I M O T I O N
*
*  IMOTION command
*
*  Subroutine to do the image motion analysis of a time series
*  image
*
*  (>)  NX      (Integer)                First dimension of input data
*  (>)  NY      (Integer)                Second dimension of input data
*  (>)  NZ      (Integer)                Third dimension of input data
*  (>)  X       (Real)                   X position of star
*  (>)  Y       (Real)                   Y position of star
*  (>)  RADIUS  (Real)                   Radius to centorid over
*  (>)  BPIXEL  (Logical)                If TRUE use brightest pixel
*                                        If false find centroid
*  (>)  IN      (Real array(NX,NY,NZ))   Input array
*  (>)  TEMPLATE (Real array(NX,NY))      Template array
*  (<)  OUT     (Real array(3,NZ))       Output image motion array
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   1/3/1992
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,NZ
      REAL IN(NX,NY,NZ),OUT(3,NZ),TEMPLATE(NX,NY)
      REAL X,Y,RADIUS
      LOGICAL BPIXEL
      INTEGER STATUS

*  Local variables
      INTEGER I1,I2,IZ
      REAL SUM,SUM2
      INTEGER LX,HX,LY,HY
      REAL CENTX,CENTY,TX,TY
      INTEGER ICX,ICY
      REAL IMAX

*  Get limits for pixel search
      LX = INT(X-RADIUS)
      HX = INT(X+RADIUS)+1
      LY = INT(Y-RADIUS)
      HY = INT(Y+RADIUS)+1
      IF (LX .LT. 1) LX=1
      IF (HX .GT. NX) HX = NX
      IF (LY .LT. 1) LY=1
      IF (HY .GT. NY) HY = NY

* Calculate centroid of template

      IF (BPIXEL) THEN
         IMAX = -1E38
         DO I2 = LY,HY
            DO I1 = LX,HX
               IF (TEMPLATE(I1,I2) .GT. IMAX) THEN
                   TX = I1
                   TY = I2
                   IMAX = TEMPLATE(I1,I2)
               ENDIF
            ENDDO
         ENDDO
      ELSE
         CENTY = 0.0
         CENTX = 0.0
         SUM = 0.0
         DO I2 = LY,HY
            DO I1 = LX,HX
              CENTY = CENTY + TEMPLATE(I1,I2) * REAL(I2)
              CENTX = CENTX + TEMPLATE(I1,I2) * REAL(I1)
              SUM = SUM+TEMPLATE(I1,I2)
            ENDDO
         ENDDO
         TX = CENTX/SUM
         TY = CENTY/SUM
      ENDIF
      PRINT *,'Centroid = ',TX,TY

* Calculate centroids of time series frames
      DO IZ=1,NZ
        IF (BPIXEL) THEN
          IMAX = -1E38
          DO I2 = LY,HY
            DO I1 = LX,HX
               IF (IN(I1,I2,IZ) .GT. IMAX) THEN
                   CENTX = I1
                   CENTY = I2
                   IMAX = IN(I1,I2,IZ)
               ENDIF
            ENDDO
          ENDDO
          OUT(1,IZ) = CENTX - TX
          OUT(2,IZ) = CENTY - TY
        ELSE
          CENTY = 0.0
          CENTX = 0.0
          SUM = 0.0
          DO I2 = LY,HY
              DO I1 = LX,HX
                  CENTY = CENTY + IN(I1,I2,IZ) * REAL(I2)
                  CENTX = CENTX + IN(I1,I2,IZ) * REAL(I1)
                  SUM = SUM + IN(I1,I2,IZ)
              ENDDO
          ENDDO
          CENTX = CENTX/SUM
          CENTY = CENTY/SUM
          OUT(1,IZ) = CENTX - TX
          OUT(2,IZ) = CENTY - TY
          ICX = NINT(CENTX)
          ICY = NINT(CENTY)
          SUM=0.
         ENDIF

*          DO I2=ICY-1,ICY+1
*              DO I1=ICX-1,ICX+1
*                  SUM=SUM+IN(I1,I2,IZ)
*              ENDDO
*          ENDDO
*          SUM2=0.
*          DO I2=ICY-2,ICY+2
*              DO I1=ICX-2,ICX+2
*                  SUM2=SUM2+IN(I1,I2,IZ)
*              ENDDO
*          ENDDO
*          SUM2=SUM2-SUM
*          OUT(3,IZ)=SUM/SUM2
           OUT(3,IZ) = 0

      IF (IZ/100*100 .EQ. IZ) THEN
          PRINT *,IZ,'Centroid = ',CENTX,CENTY,OUT(3,IZ)
      ENDIF
      ENDDO


      END
