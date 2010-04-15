C+
      SUBROUTINE SHIFTADD(STATUS)
C
C           SHIFTADD
C
C     Command name:
C        SHIFTADD
C
C     Function:
C        Add frames of time series image correcting for image motion
C
C     Description:
C        Obtain an output image by summing the framesof a time series
C        image after shifting each frame to correct for image motion.
C        The image motion information is obtained from a time series
C        containing the X and Y motion in its first and second channels.
C        Such a time series can be generated using the IMOTION command.
C
C     Parameters:
C    (1) INPUT     (TSP, 3D)   The time series image dataset.
C    (2) MOTION    (TSP, 2D)   The image motion time series.
C    (3) OUTPUT    (TSP, 2D)   The output image.
C
C     Support:
C         Jeremy Bailey, AAO
C
C     Version date:
C         1/3/1992
C
C-
C
C  History:
C    1/3/1992   Original Version.   JAB/AAO
C



      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'USER_ERR'

*  Status argument
      INTEGER STATUS

*  HDS locators
      CHARACTER*(DAT__SZLOC) LOC,OLOC,LOC2,OLOC2
      CHARACTER*(DAT__SZLOC) PLOC,PPLOC

*  Data pointers
      INTEGER PTR,OPTR,PPTR,TPTR

*  Array dimensions
      INTEGER NDIMS,DIMS(3)
      INTEGER PNDIMS,PDIMS(3)
      INTEGER NZ,IZ

*  Interpolation factor
      INTEGER INTERP

*  NDF placeholder
      INTEGER PLACE

*  NDF bounds
      INTEGER LBND(2),UBND(2)

*  NDF identifiers
      INTEGER IDTEMP,IDSUM,IDCOUNT,IDTEMP2,IDSUM2,IDCOUNT2
      INTEGER ID(3)
      INTEGER PTR1,PTR2,PTR3,SPTR
      INTEGER SHIFT(2)

      INTEGER NX2,NY2,ELS
*  Get the input file
      CALL DAT_ASSOC('INPUT','READ',LOC,STATUS)

*  Get size of data
      CALL TSP_SIZE(LOC,3,DIMS,NDIMS,STATUS)

*  Check that data is three dimensional
      IF (NDIMS .NE. 3) THEN
          CALL MSG_OUT(' ','Input must be 3 dimensional',STATUS)
          GOTO 100
      ENDIF

*  Map the data
      CALL TSP_MAP_DATA(LOC,'READ',PTR,LOC2,STATUS)

*  Get the image motion file
      CALL DAT_ASSOC('MOTION','READ',PLOC,STATUS)
      CALL TSP_SIZE(PLOC,3,PDIMS,PNDIMS,STATUS)

*  Check Dimensions Match
      IF (PNDIMS .NE. 2) THEN
          CALL MSG_OUT(' ',
     :       'Image motion dataset must be 2 dimensional',STATUS)
          GOTO 100
      ELSE IF (DIMS(3) .NE. PDIMS(2)) THEN
          CALL MSG_OUT(' ',
     :         'Size of INPUT and TEMPLATE are different in time axis',
     :         STATUS)
          GOTO 100
      ENDIF

*  Map the time series data
      CALL TSP_MAP_DATA(PLOC,'READ',PPTR,PPLOC,STATUS)

*  Make the output file
      CALL DAT_CREAT('OUTPUT','NDF',0,0,STATUS)
      CALL DAT_ASSOC('OUTPUT','WRITE',OLOC,STATUS)

*  Create 2D data structure
      CALL TSP_CREATE_2D(OLOC,DIMS(1),DIMS(2),' ',.FALSE.,
     :     .FALSE.,STATUS)

*  Map the data array
      CALL TSP_MAP_DATA(OLOC,'WRITE',OPTR,OLOC2,STATUS)

*  Get interpolation factor

      CALL PAR_GET0I('INTERP',INTERP,STATUS)

*  Create workspace array for interpolated image
      CALL NDF_TEMP(PLACE,STATUS)
      LBND(1)=1
      LBND(2)=1
      UBND(1)=DIMS(1)*INTERP
      UBND(2)=DIMS(2)*INTERP
      CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,IDTEMP,STATUS)

*  Create workspace array for summed image
      CALL NDF_TEMP(PLACE,STATUS)
      CALL NDF_NEW('_REAL',2,LBND,UBND,PLACE,IDSUM,STATUS)

*  Create workspace array for count image
      CALL NDF_TEMP(PLACE,STATUS)
      CALL NDF_NEW('_INTEGER',2,LBND,UBND,PLACE,IDCOUNT,STATUS)

*  Zero the sum and count arrays
      CALL NDF_MAP(IDTEMP,'DATA','_REAL','WRITE/ZERO',PTR1,ELS,STATUS)
      CALL NDF_MAP(IDSUM,'DATA','_REAL','WRITE/ZERO',PTR2,ELS,STATUS)
      CALL NDF_MAP(IDCOUNT,'DATA','_REAL','WRITE/ZERO',PTR3,ELS,STATUS)

*  Loop over time series
      NZ = DIMS(3)
      DO IZ=1,NZ
          IF (IZ/100*100 .EQ. IZ) THEN
              PRINT *,IZ
          ENDIF

*  Interpolate input frame into temporary frame
          NX2=DIMS(1)*INTERP
          NY2=DIMS(2)*INTERP
*          CALL TSP_BICUBIC(DIMS(1),DIMS(2),NZ,IZ,%VAL(PTR),INTERP,
*     :          NX2,NY2,%VAL(PTR1),STATUS)
          CALL TSP_BILINT(DIMS(1),DIMS(2),NZ,IZ,%VAL(PTR),INTERP,
     :          NX2,NY2,%VAL(PTR1),STATUS)

*  Shift temporary frame
          CALL TSP_GETSHIFT(NZ,IZ,%VAL(PPTR),INTERP,SHIFT,STATUS)
          CALL TSP_SHIFTADD(NX2,NY2,SHIFT,%VAL(PTR1),%VAL(PTR2),
     :         %VAL(PTR3),STATUS)
      ENDDO

*  Squash summed image back to original size
      CALL TSP_SQUASH(NX2,NY2,%VAL(PTR2),%VAL(PTR3),DIMS(1),DIMS(2),
     :      %VAL(OPTR),INTERP,STATUS)

*  Tidy up
100   CONTINUE
      CALL TSP_UNMAP(PPLOC,STATUS)
      CALL DAT_ANNUL(PLOC,STATUS)
      CALL TSP_UNMAP(LOC2,STATUS)
      CALL TSP_UNMAP(OLOC2,STATUS)
      CALL DAT_ANNUL(LOC,STATUS)
      CALL DAT_ANNUL(OLOC,STATUS)
      END



      SUBROUTINE TSP_BILINT(NX,NY,NZ,IZ,IN,INTERP,NX2,NY2,OUT,STATUS)
*+
*
*  T S P _ B I L I N T
*
*  SHIFTADD command
*
*  Bilinear interpolate a frame of the input image.
*
*  (>)  NX      (Integer)                First dimension of input data
*  (>)  NY      (Integer)                Second dimension of input data
*  (>)  NZ      (Integer)                Third dimension of input data
*  (>)  IZ      (Integer)                Frame of time series to use
*  (>)  IN      (Real array(NX,NY,NZ))   Input array
*  (>)  INTERP  (Integer)                Interpolation factor
*  (>)  NX2     (Integer)                First dimension of output image
*  (>)  NY2     (Integer)                Second dimension of output image
*  (<)  OUT     (Real array(NX2,NY2))    Output image
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   1/3/1992
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,NZ,IZ,INTERP,NX2,NY2
      REAL IN(NX,NY,NZ),OUT(NX2,NY2)
      INTEGER STATUS

*  Local variables
      INTEGER IX,IY,JX,JY,KX,KY
      REAL T,U

      IF (INTERP .EQ. 1) THEN
          DO IY=1,NY
              DO IX=1,NX
                  OUT(IX,IY)=IN(IX,IY,IZ)
              ENDDO
          ENDDO
          RETURN
      ENDIF

      DO IY=1,NY
          DO IX=1,NX
              DO JY=1,INTERP
                  DO JX=1,INTERP
                      KY=(IY-1)*INTERP+JY
                      KX=(IX-1)*INTERP+JX
                      OUT(KX,KY)=IN(IX,IY,IZ)
                  ENDDO
              ENDDO
          ENDDO
       ENDDO


*      DO IY=1,NY2-INTERP
*          JY = (IY-1)/INTERP + 1
*          DO IX=1,NX2-INTERP
*              JX=(IX-1)/INTERP + 1
*              T = REAL((IX-1) - INTERP*(JX-1))/REAL(INTERP)
*              U = REAL((IY-1) - INTERP*(JY-1))/REAL(INTERP)
*              OUT(IX,IY)=(1.0-T)*(1.0-U)*IN(JX,JY,IZ) +
*     :                T*(1.0-U)*IN(JX+1,JY,IZ) + T*U*IN(JX+1,JY+1,IZ)
*     :                + (1.0-T)*U*IN(JX,JY+1,IZ)
*          ENDDO
*      ENDDO
      END

      SUBROUTINE TSP_BICUBIC(NX,NY,NZ,IZ,IN,INTERP,NX2,NY2,OUT,STATUS)
*+
*
*  T S P _ B I C U B I C
*
*  SHIFTADD command
*
*  Bicubic spline interpolate a frame of the input image.
*
*  (>)  NX      (Integer)                First dimension of input data
*  (>)  NY      (Integer)                Second dimension of input data
*  (>)  NZ      (Integer)                Third dimension of input data
*  (>)  IZ      (Integer)                Frame of time series to use
*  (>)  IN      (Real array(NX,NY,NZ))   Input array
*  (>)  INTERP  (Integer)                Interpolation factor
*  (>)  NX2     (Integer)                First dimension of output image
*  (>)  NY2     (Integer)                Second dimension of output image
*  (<)  OUT     (Real array(NX2,NY2))    Output image
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   1/3/1992
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,NZ,IZ,INTERP,NX2,NY2
      REAL IN(NX,NY,NZ),OUT(NX2,NY2)
      INTEGER STATUS
      REAL YA(31,31),Y2A(31,31),X1A(100),X2A(100)

*  Local variables
      INTEGER IX,IY,JX,JY
      REAL T,U
      REAL X1(300),X2,Y(300)

      IF (INTERP .EQ. 1) THEN
          DO IY=1,NY
              DO IX=1,NX
                  OUT(IX,IY)=IN(IX,IY,IZ)
              ENDDO
          ENDDO
          RETURN
      ENDIF

      DO IY=1,NY
          X2A(IY)=IY
          DO IX=1,NX
              YA(IX,IY)=IN(IX,IY,IZ)
          ENDDO
      ENDDO
      DO IX=1,NX
          X1A(IX)=IX
      ENDDO
      CALL SPLIE2(X1A,X2A,YA,NX,NY,Y2A)
      DO JY=1,NY2-INTERP
          X2=(JY-1)/INTERP+1
          DO JX=1,NX2-INTERP
              X1(JX)=(JX-1)/INTERP+1
          ENDDO
          CALL SPLIN2(X1A,X2A,YA,Y2A,NX,NY,NX2,X1,X2,Y)
          DO JX=1,NX2-INTERP
              OUT(JX,JY)=Y(JX)
          ENDDO
      ENDDO
      END


      SUBROUTINE TSP_GETSHIFT(NZ,IZ,IN,INTERP,SHIFT,STATUS)
*+
*
*  T S P _ G E T S H I F T
*
*  SHIFTADD command
*
*  Get shift values for a frame in a time series
*
*  (>)  NZ      (Integer)                Third dimension of input data
*  (>)  IZ      (Integer)                Frame of time series to use
*  (>)  IN      (Real array(3,NZ))       Input image motion time
*  (>)  INTERP  (Integer)                Interpolation factor
*  (<)  SHIFT   (Integer array(2))       Array of shift values
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   1/3/1992
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NZ,IZ,INTERP
      REAL IN(3,NZ)
      INTEGER SHIFT(2)
      INTEGER STATUS

      SHIFT(1) = NINT(INTERP*IN(1,IZ))
      SHIFT(2) = NINT(INTERP*IN(2,IZ))
      END


      SUBROUTINE TSP_SHIFTADD(NX,NY,SHIFT,FRAME,SUM,COUNT,STATUS)
*+
*
*  T S P _ S H I F T A D D
*
*  SHIFTADD command
*
*  Add a frame into the running sum, and incrment the count of pixels
*
*  (>)  NX      (Integer)                Size of data in X
*  (>)  NY      (Integer)                Size of data in Y
*  (>)  SHIFT   (Integer array(2))       Shit in X and Y
*  (>)  FRAME   (Real array(NX))         Frame to be added in
*  (!)  SUM     (Real array(NX))         Running sum
*  (!)  COUNT   (Integer array(NX))      Count of pixels added to each point
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   1/3/1992
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY
      INTEGER SHIFT(2)
      REAL FRAME(NX,NY),SUM(NX,NY),COUNT(NX,NY)
      INTEGER STATUS

*  Local variables
      INTEGER IX,IY,JX,JY

      IF (SHIFT(1) .NE. 0 .OR. SHIFT(2) .NE. 0) RETURN
      DO IX=1,NX
          DO IY=1,NY
              JX = IX-SHIFT(1)
              JY = IY-SHIFT(2)
              IF (JX .GE. 1 .AND. JX .LE. NX .AND. JY .GE. 1
     :             .AND. JY .LE. NY) THEN
                  SUM(JX,JY) = SUM(JX,JY)+FRAME(IX,IY)
                  COUNT(JX,JY) = COUNT(JX,JY)+1
              ENDIF
          ENDDO
      ENDDO
      END



      SUBROUTINE TSP_SQUASH(NX2,NY2,SUM,COUNT,NX,NY,OUT,INTERP,STATUS)
*+
*
*  T S P _ S Q U A S H
*
*  SHIFTADD command
*
*  Squash the summed frame back to its original size
*
*  (>)  NX2     (Integer)                First dimension of input data
*  (>)  NY2     (Integer)                Second dimension of input data
*  (>)  SUM     (Real array(NX2,NY2)     Input sum array
*  (>)  COUNT   (Integer array(NX2,NY2)  Input count array
*  (>)  NX      (Integer)                First dimension of output image
*  (>)  NY      (Integer)                Second dimension of output image
*  (<)  OUT     (Real array(NX,NY))      Output image
*  (>)  INTERP  (Integer)                Interpolation factor
*  (!)  STATUS  (Integer)                Status argument
*
*  Jeremy Bailey   1/3/1992
*
*+
      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,INTERP,NX2,NY2
      REAL SUM(NX2,NY2),COUNT(NX2,NY2),OUT(NX,NY)
      INTEGER STATUS

*  Local variables
      INTEGER IX,IY,JX,JY
      INTEGER IXST,IXEN,IYST,IYEN,PCOUNT

*  Divide summed image by count
      DO IY=1,NY2
          DO IX=1,NX2
              SUM(IX,IY)=SUM(IX,IY)/COUNT(IX,IY)
          ENDDO
      ENDDO
      IF (INTERP .EQ. 1) THEN
          DO IY=1,NY2
              DO IX=1,NX2
                  OUT(IX,IY)=SUM(IX,IY)
              ENDDO
          ENDDO
          RETURN
      ENDIF

*  Form output image
      DO IX=1,NX
          DO IY=1,NY
              IXST = (IX-1)*INTERP+1
              IXEN = IXST+INTERP-1
              IF (IXST .LT. 1) IXST=1
              IF (IXEN .GT. NX2) IXEN=NX2
              IYST = (IY-1)*INTERP+1
              IYEN = IYST+INTERP-1
              IF (IYST .LT. 1) IYST=1
              IF (IYEN .GT. NY2) IYEN=NY2
              PCOUNT=0.0
              OUT(IX,IY)=0.0
              DO JY=IYST,IYEN
                  DO JX=IXST,IXEN
                      OUT(IX,IY)=OUT(IX,IY)+SUM(JX,JY)
                      PCOUNT = PCOUNT+1.0
                  ENDDO
              ENDDO
              OUT(IX,IY)=OUT(IX,IY)/PCOUNT
          ENDDO
      ENDDO
      END



      SUBROUTINE SPLIN2(X1A,X2A,YA,Y2A,M,N,NX1,X1,X2,Y)
      PARAMETER(NN=100)
      REAL X1A(M),X2A(N),YA(M,N),Y2A(M,N),YTMP(NN),Y2TMP(NN),YYTMP(NN)
      REAL X1(NX1),Y(NX1)
      DO J=1,M
          DO K=1,N
              YTMP(K)=YA(J,K)
              Y2TMP(K)=Y2A(J,K)
          ENDDO
          CALL SPLINT(X2A,YTMP,Y2TMP,N,X2,YYTMP(J))
      ENDDO
      CALL SPLINE(X1A,YYTMP,M,Y2TMP)
      DO I=1,NX1
          CALL SPLINT(X1A,YYTMP,Y2TMP,M,X1(I),Y(I))
      ENDDO
      END


      SUBROUTINE SPLIE2(X1A,X2A,YA,M,N,Y2A)
      PARAMETER(NN=100)
      REAL X1A(M),X2A(N),YA(M,N),Y2A(M,N),YTMP	(NN),Y2TMP(NN)
      DO J=1,M
          DO K=1,N
              YTMP(K)=YA(J,K)
          ENDDO
          CALL SPLINE(X2A,YTMP,N,Y2TMP)
          DO K=1,N
              Y2A(J,K)=Y2TMP(K)
          ENDDO
      ENDDO
      END

      SUBROUTINE SPLINE(X,Y,N,Y2)
      PARAMETER (NMAX=100)
      REAL X(N),Y(N),Y2(N),U(NMAX)
      Y2(1)=0.
      U(1)=0.
      DO I=2,N-1
          SIG=(X(I)-X(I-1))/(X(I+1)-X(I-1))
          P=SIG*Y2(I-1)+2
          Y2(I)=(SIG-1.)/P
          U(I)=(6.*((Y(I+1)-Y(I))/(X(I+1)-X(I))-(Y(I)-Y(I-1))
     :         /(X(I)-X(I-1)))/(X(I+1)-X(I-1))-SIG*U(I-1))/P
      ENDDO
      Y2(N)=0.
      DO K=N-1,1,-1
          Y2(K)=Y2(K)*Y2(K+1)+U(K)
      ENDDO
      END

      SUBROUTINE SPLINT(XA,YA,Y2A,N,X,Y)
      REAL XA(N),YA(N),Y2A(N)
      KLO=1
      KHI=N
1     IF(KHI-KLO.GT.1) THEN
          K=(KHI+KLO)/2
          IF (XA(K) .GT. X) THEN
              KHI=K
          ELSE
              KLO=K
          ENDIF
          GOTO 1
      ENDIF
      H=XA(KHI)-XA(KLO)
      A=(XA(KHI)-X)/H
      B=(X-XA(KLO))/H
      Y=A*YA(KLO)+B*YA(KHI)+
     :    ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
      END
