      SUBROUTINE VELVCT (U,LU,V,LV,M,N,FLO,HI,NSET,LENGTH,ISPV,SPV)
      COMMON /VEC1/   ASH        ,EXT        ,ICTRFG     ,ILAB       ,
     +                IOFFD      ,IOFFM      ,ISX        ,ISY        ,
     +                RMN        ,RMX        ,SIDE       ,SIZE       ,
     +                XLT        ,YBT        ,ZMN        ,ZMX
C
      COMMON /VEC2/   BIG        ,INCX       ,INCY
C
C FORCE THE BLOCK DATA ROUTINE, WHICH SETS DEFAULT VARIABLES, TO LOAD.
C
      EXTERNAL        VELDAT
C
C ARGUMENT DIMENSIONS.
C
      DIMENSION       U(LU,N)    ,V(LV,N)    ,SPV(2)
        CHARACTER*10    LABEL
        REAL WIND(4), VIEW(4)
C
C ---------------------------------------------------------------------
C
C INTERNAL PARAMETERS OF VELVCT ARE AS FOLLOWS.  THE DEFAULT VALUES OF
C THESE PARAMETERS ARE DECLARED IN THE BLOCK DATA ROUTINE VELDAT.
C
C                        NAME   DEFAULT  FUNCTION
C                        ----   -------  --------
C
C                        BIG   R1MACH(2) CONSTANT USED TO INITIALIZE
C                                        POSSIBLE SEARCH FOR HI.
C
C                        EXT     0.25    THE LENGTHS OF THE SIDES OF THE
C                                        PLOT ARE PROPORTIONAL TO M AND
C                                        N WHEN NSET IS LESS THAN OR
C                                        EQUAL TO ZERO, EXCEPT WHEN
C                                        MIN(M,N)/MAX(M,N) IS LESS THAN
C                                        EXT, IN WHICH CASE A SQUARE
C                                        GRAPH IS PLOTTED.
C
C                        ICTRFG    1     FLAG TO CONTROL THE POSITION OF
C                                        THE ARROW RELATIVE TO  A BASE
C                                        POINT AT (MX,MY).
C
C                                        ZERO - CENTER AT (MX,MY)
C
C                                        POSITIVE - TAIL AT (MX,MY)
C
C                                        NEGATIVE -  HEAD AT (MX,MY)
C
C                        ILAB      0     FLAG TO CONTROL THE DRAWING OF
C                                        LINE LABELS.
C
C                                        ZERO - DO NOT DRAW THE LABELS
C
C                                        NON-ZERO - DRAW THE LABELS
C
C                        INCX      1     X-COORDINATE STEP SIZE FOR LESS
C                                        DENSE ARRAYS.
C
C                        INCY      1     Y-COORDINATE STEP SIZE.
C
C                        IOFFD     0     FLAG TO CONTROL NORMALIZATION
C                                        OF LABEL NUMBERS.
C
C                                        ZERO - INCLUDE A DECIMAL POINT
C                                        WHEN POSSIBLE
C
C                                        NON-ZERO - NORMALIZE ALL LABEL
C                                        NUMBERS BY ASH
C
C                        IOFFM     0     FLAG TO CONTROL PLOTTING OF
C                                        THE MESSAGE BELOW THE PLOT.
C
C                                        ZERO - PLOT THE MESSAGE
C
C                                        NON-ZERO - DO NOT PLOT IT
C
C                        RMN     160.    ARROW SIZE BELOW WHICH THE
C                                        HEAD NO LONGER SHRINKS, ON A
C                                        2**15 X 2**15 GRID.
C
C                        RMX    6400.    ARROW SIZE ABOVE WHICH THE
C                                        HEAD NO LONGER GROWS LARGER,
C                                        ON A 2**15 X 2**15 GRID.
C
C                        SIDE    0.90    LENGTH OF LONGER EDGE OF PLOT.
C                                        (SEE ALSO EXT.)
C
C                        SIZE    256.    WIDTH OF THE CHARACTERS IN
C                                        VECTOR LABELS, ON A 2**15 X
C                                        2**15 GRID.
C
C                        XLT     0.05    LEFT HAND EDGE OF THE PLOT.
C                                        (0 IS THE LEFT EDGE OF THE
C                                        FRAME, 1 THE RIGHT EDGE.)
C
C                        YBT     0.05    BOTTOM EDGE OF THE PLOT (0 IS
C                                        THE BOTTOM OF THE FRAME, 1 THE
C                                        TOP OF THE FRAME.)
C
C ---------------------------------------------------------------------
C
C INTERNAL FUNCTIONS WHICH MAY BE MODIFIED FOR DATA TRANSFORMATION -
C
C                        SCALE    COMPUTES A SCALE FACTOR USED IN THE
C                                 DETERMINATION OF THE LENGTH OF THE
C                                 VECTOR TO BE DRAWN.
C
C                        DIST     COMPUTES THE LENGTH OF A VECTOR.
C
C                        FX       RETURNS THE X INDEX AS THE
C                                 X-COORDINATE OF THE VECTOR BASE.
C
C                        MXF      RETURNS THE X-COORDINATE OF THE VECTOR
C                                 HEAD.
C
C                        FY       RETURNS THE Y INDEX AS THE
C                                 Y-COORDINATE OF THE VECTOR BASE.
C
C                        MYF      RETURNS THE Y-COORDINATE OF THE VECTOR
C                                 HEAD.
C
C                        VLAB     THE VALUE FOR THE VECTOR LABEL WHEN
C                                 ILAB IS NON-ZERO.
C
      SAVE
      DIST(XX,YY) = SQRT(XX*XX+YY*YY)
      FX(XX,YY) = XX
      FY(XX,YY) = YY
      MXF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MXX+IFIX(SFXX*UU)
      MYF(XX,YY,UU,VV,SFXX,SFYY,MXX,MYY) = MYY+IFIX(SFYY*VV)
      SCALEX(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1       LENN) = LENN/HAA
      SCALEY(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,XX4,YY3,YY4,
     1       LENN) = SCALEX(MM,NN,INCXX,INCYY,HAA,XX1,XX2,YY1,YY2,XX3,
     2                                                 XX4,YY3,YY4,LENN)
      VLAB(UU,VV,II,JJ) = DIST(UU,VV)
C
C ---------------------------------------------------------------------
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR.
C
      CALL Q8QST4 ('NSSL','VELVCT','VELVCT','VERSION  6')
C
C INITIALIZE AND TRANSFER SOME ARGUMENTS TO LOCAL VARIABLES.
C
      BIG = -R1MACH(2)
      MX = LU
      MY = LV
      NX = M
      NY = N
      GL = FLO
      HA = HI
      ISP = ISPV
      NC = 0
C
C COMPUTE CONSTANTS BASED ON THE ADDRESSABILITY OF THE PLOTTER.
C
      CALL GETUSV('XF',ISX)
      CALL GETUSV('YF',ISY)
      ISX = 2**(15-ISX)
      ISY = 2**(15-ISY)
      LEN = LENGTH*ISX
C
C SET UP THE SCALING OF THE PLOT.
C
        CALL GQCNTN(IERR,IOLDNT)
        CALL GQNT(IOLDNT,IERR,WIND,VIEW)
        X1 = VIEW(1)
        X2 = VIEW(2)
        Y1 = VIEW(3)
        Y2 = VIEW(4)
        X3 = WIND(1)
        X4 = WIND(2)
        Y3 = WIND(3)
        Y4 = WIND(4)
        CALL GETUSV('LS',IOLLS)
C
C     SAVE NORMALIZATION TRANSFORMATION 1
C
      CALL GQNT(1,IERR,WIND,VIEW)
C
      IF (NSET) 101,102,106
C
  101 X3 = 1.
      X4 = FLOAT(NX)
      Y3 = 1.
      Y4 = FLOAT(NY)
      GO TO 105
C
  102 X1 = XLT
      X2 = XLT+SIDE
      Y1 = YBT
      Y2 = YBT+SIDE
      X3 = 1.
      Y3 = 1.
      X4 = FLOAT(NX)
      Y4 = FLOAT(NY)
      IF (AMIN1(X4,Y4)/AMAX1(X4,Y4) .LT. EXT) GO TO 105
C
      IF (NX-NY) 103,105,104
  103 X2 = XLT+SIDE*X4/Y4
      GO TO 105
  104 Y2 = YBT+SIDE*Y4/X4
C
  105 CALL SET(X1,X2,Y1,Y2,X3,X4,Y3,Y4,1)
      IF (NSET .EQ. 0) CALL PERIM (1,0,1,0)
C
C CALCULATE A LENGTH IF NONE PROVIDED.
C
  106 IF (LEN .NE. 0) GO TO 107
      CALL FL2INT(FX(1.,1.),FY(1.,1.),MX,MY)
      CALL FL2INT(FX(FLOAT(1+INCX),FLOAT(1+INCY)),
     +            FY(FLOAT(1+INCX),FLOAT(1+INCY)),LX,LY)
      LEN = SQRT((FLOAT(MX-LX)**2+FLOAT(MY-LY)**2)/2.)
C
C SET UP SPECIAL VALUES.
C
  107 IF (ISP .EQ. 0) GO TO 108
      SPV1 = SPV(1)
      SPV2 = SPV(2)
      IF (ISP .EQ. 4) SPV2 = SPV(1)
C
C FIND THE MAXIMUM VECTOR LENGTH.
C
  108 IF (HA .GT. 0.) GO TO 118
C
      HA = BIG
      IF (ISP .EQ. 0) GO TO 115
C
      DO 114 J=1,NY,INCY
         DO 113 I=1,NX,INCX
            IF (ISP-2) 109,111,110
  109       IF (U(I,J) .EQ. SPV1) GO TO 113
            GO TO 112
  110       IF (U(I,J) .EQ. SPV1) GO TO 113
  111       IF (V(I,J) .EQ. SPV2) GO TO 113
  112       HA = AMAX1(HA,DIST(U(I,J),V(I,J)))
  113    CONTINUE
  114 CONTINUE
      GO TO 126
C
  115 DO 117 J=1,NY,INCY
         DO 116 I=1,NX,INCX
            HA = AMAX1(HA,DIST(U(I,J),V(I,J)))
  116    CONTINUE
  117 CONTINUE
C
C BRANCH IF NULL VECTOR SIZE.
C
  126 IF (HA .LE. 0.) GO TO 125
C
C COMPUTE SCALE FACTORS.
C
  118 SFX = SCALEX(M,N,INCX,INCY,HA,X1,X2,Y1,Y2,X3,X4,Y3,Y4,LEN)
      SFY = SCALEY(M,N,INCX,INCY,HA,X1,X2,Y1,Y2,X3,X4,Y3,Y4,LEN)
      IOFFDT = IOFFD
      IF (GL.NE.0.0 .AND. (ABS(GL).LT.0.1 .OR. ABS(GL).GE.1.E5))
     1    IOFFDT = 1
      IF (HA.NE.0.0 .AND. (ABS(HA).LT.0.1 .OR. ABS(HA).GE.1.E5))
     1    IOFFDT = 1
      ASH = 1.0
      IF (IOFFDT .NE. 0)
     1    ASH = 10.**(3-IFIX(ALOG10(AMAX1(ABS(GL),ABS(HA)))-500.)-500)
      IZFLG = 0
C
C COMPUTE ZMN AND ZMX, WHICH ARE USED IN DRWVEC.
C
      ZMN = LEN*(GL/HA)
      ZMX = FLOAT(LEN)+.01
C
C DRAW THE VECTORS.
C
      DO 123 J=1,NY,INCY
         DO 122 I=1,NX,INCX
            UI = U(I,J)
            VI = V(I,J)
            IF (ISP-1) 121,119,120
  119       IF (UI-SPV1) 121,122,121
  120       IF (VI .EQ. SPV2) GO TO 122
            IF (ISP .GE. 3) GO TO 119
  121       X = I
            Y = J
            CALL FL2INT(FX(X,Y),FY(X,Y),MX,MY)
            LX = MAX0(1,MXF(X,Y,UI,VI,SFX,SFY,MX,MY))
            LY = MAX0(1,MYF(X,Y,UI,VI,SFX,SFY,MX,MY))
            IZFLG = 1
            IF (ILAB .NE. 0) CALL ENCD(VLAB(UI,VI,I,J),ASH,LABEL,NC,
     +                                                           IOFFDT)
            CALL DRWVEC (MX,MY,LX,LY,LABEL,NC)
  122    CONTINUE
  123 CONTINUE
C
      IF (IZFLG .EQ. 0) GO TO 125
C
      IF (IOFFM .NE. 0) GO TO 200
      WRITE(LABEL,'(E10.3)')HA
      CALL DRWVEC (28768,608,28768+LEN,608,LABEL,10)
        IX = 1+(28768+LEN/2)/ISX
        IY = 1+(608-(5*ISX*MAX0(256/ISX,8))/4)/ISY
        CALL GQCNTN(IER,ICN)
        CALL GSELNT(0)
        XC = CPUX(IX)
        YC = CPUY(IY)
      CALL WTSTR (XC,YC,
     +                         'MAXIMUM VECTOR',MAX0(256/ISX,8),0,0)
      CALL GSELNT(ICN)
C
C DONE.
C
      GOTO 200
C
C ZERO-FIELD ACTION.
C
  125 IX = 1+16384/ISX
        IY = 1+16384/ISY
        CALL GQCNTN(IER,ICN)
        CALL GSELNT(0)
        XC = CPUX(IX)
        YC = CPUY(IY)
      CALL WTSTR (XC,YC,
     +                             'ZERO FIELD',MAX0(960/ISX,8),0,0)
        CALL GSELNT(ICN)
C
C RESTORE TRANS 1 AND LOG SCALING AND ORIGINAL TRANS NUMBER
C
  200 CONTINUE
      IF (NSET .LE. 0) THEN
        CALL SET(VIEW(1),VIEW(2),VIEW(3),VIEW(4),
     -           WIND(1),WIND(2),WIND(3),WIND(4),IOLLS)
      ENDIF
      CALL GSELNT(IOLDNT)
      RETURN
      END
