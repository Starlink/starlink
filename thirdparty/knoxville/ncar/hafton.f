      SUBROUTINE HAFTON (Z,L,M,N,FLO,HI,NLEV,NOPT,NPRM,ISPV,SPVAL)
      SAVE
      DIMENSION       Z(L,N)      ,PX(2)       ,PY(2)
      DIMENSION       ZLEV(16)    ,VWPRT(4)    ,WNDW(4)
      DIMENSION                    VWPR2(4)    ,WND2(4)
      CHARACTER*11    IDUMMY
      EXTERNAL        HFINIT
C
C
      COMMON /HAFTO1/ I          ,J          ,INTEN
      COMMON /HAFTO2/ GLO        ,HA         ,NOPTN      ,ALPHA      ,
     1                NSPV       ,SP         ,ICNST
      COMMON /HAFTO3/ XLT        ,YBT        ,SIDE       ,EXT        ,
     1                IOFFM      ,ALPH       ,MXLEV      ,NCRTG      ,
     2                NCRTF      ,IL(135)
      COMMON /HAFTO4/ NPTMAX     ,NPOINT     ,XPNT(50)  ,YPNT(50)
C
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','HAFTON','HAFTON','VERSION  1')
C
      NPOINT = 0
      ALPHA = ALPH
      GLO = FLO
      HA = HI
      NLEVL = MIN0(IABS(NLEV),MXLEV)
      IF (NLEVL .LE. 1) NLEVL = MXLEV
      NOPTN = NOPT
      IF (NOPTN .EQ. 0) NOPTN = 1
      NPRIM = NPRM
      NSPV = MAX0(MIN0(ISPV,4),0)
      IF (NSPV .NE. 0) SP = SPVAL
      MX = L
      NX = M
      NY = N
      CRTF = NCRTF
      MSPV = 0
C
C SET INTENSITY BOUNDARY LEVELS
C
      CALL ZLSET (Z,MX,NX,NY,ZLEV,NLEVL)
C
C SET UP PERIMETER
C
      X3 = NX
      Y3 = NY
      CALL GQCNTN (IERR,NTORIG)
      CALL GETUSV('LS',IOLLS)
      IF (NPRIM.LT.0) THEN
         CALL GQNT (NTORIG,IERR,WNDW,VWPRT)
         X1 = VWPRT(1)
         X2 = VWPRT(2)
         Y1 = VWPRT(3)
         Y2 = VWPRT(4)
      ELSE IF (NPRIM.EQ.0) THEN
         X1 = XLT
         X2 = XLT+SIDE
         Y1 = YBT
         Y2 = YBT+SIDE
         IF (AMIN1(X3,Y3)/AMAX1(X3,Y3) .GE. EXT) THEN
            IF (NX-NY.LT.0) THEN
               X2 =SIDE*X3/Y3+XLT
               X2 = (AINT(X2*CRTF/FLOAT(NCRTG))*FLOAT(NCRTG))/CRTF
            ELSE IF (NX-NY.GT.0) THEN
               Y2 = SIDE*Y3/X3+YBT
               Y2 = (AINT(Y2*CRTF/FLOAT(NCRTG))*FLOAT(NCRTG))/CRTF
            END IF
         END IF
      ELSE IF (NPRIM.GT.0) THEN
         X1 = 0.0
         X2 = 1.0
         Y1 = 0.0
         Y2 = 1.0
      END IF
      MX1 = X1*CRTF
      MX2 = X2*CRTF
      MY1 = Y1*CRTF
      MY2 = Y2*CRTF
      IF (NPRIM.GT.0) THEN
         MX1 = 1
         MY1 = 1
         MX2 = NCRTF
         MY2 = NCRTF
      END IF
C
C SAVE NORMALIZATION TRANS 1
C
      CALL GQNT (1,IERR,WNDW,VWPRT)
C
C DEFINE NORMALIZATION TRANS 1 AND LOG SCALING FOR USE WITH PERIM
C DRAW PERIMETER IF NPRIM EQUALS 0
C
      CALL SET(X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)
      IF (NPRIM .EQ. 0) CALL PERIM (NX-1,1,NY-1,1)
      IF (ICNST .NE. 0) THEN
        CALL GSELNT (0)
        CALL WTSTR(XLT*1.1,0.5,'CONSTANT FIELD',2,0,0)
        GO TO 132
      END IF
C
C FIND OFFSET FOR REFERENCE TO IL, WHICH IS TRIANGULAR
C
      IOFFST = NLEVL*((NLEVL-1)/2)+MOD(NLEVL-1,2)*(NLEVL/2)-1
C
C OUTPUT INTENSITY SCALE
C
      IF (NPRIM .GT. 0) GO TO 112
      LEV = 0
      KX = (1.1*XLT+SIDE)*CRTF
      KY = YBT*CRTF
      NNX = KX/NCRTG
  109 LEV = LEV+1
      ISUB = IOFFST+LEV
      INTEN = IL(ISUB)
      IF (NOPTN .LT. 0) INTEN = MXLEV-INTEN
      NNY = KY/NCRTG
      DO 111 JJ=1,3
         DO 110 II=1,10
            I = NNX+II
            J = NNY+JJ
            CALL GRAY
  110    CONTINUE
  111 CONTINUE
      IF (LEV .GE. NLEVL) GO TO 112
        WRITE(IDUMMY,'(G11.4)') ZLEV(LEV)
      TKX = KX
      TKY = KY+38
      CALL GQNT(1,IERR,WND2,VWPR2)
      CALL SET(0.,1.,0.,1.,0.,1023.,0.,1023.,1)
      CALL WTSTR (TKX,TKY,IDUMMY,0,0,-1)
      CALL SET(VWPR2(1),VWPR2(2),VWPR2(3),VWPR2(4),
     -         WND2(1),WND2(2),WND2(3),WND2(4),1)
C
C ADJUST 38 TO PLOTTER.
C
      KY = KY+52
C
C ADJUST 52 TO PLOTTER.
C
      GO TO 109
C
C STEP THROUGH PLOTTER GRID OF INTENSITY CELLS.
C
  112 IMIN = (MX1-1)/NCRTG+1
      IMAX = (MX2-1)/NCRTG
      JMIN = (MY1-1)/NCRTG+1
      JMAX = (MY2-1)/NCRTG
      XL = IMAX-IMIN+1
      YL = JMAX-JMIN+1
      XN = NX
      YN = NY
      LSRT = NLEVL/2
      DO 130 J=JMIN,JMAX
C
C FIND Y FOR THIS J AND Z FOR THIS Y.
C
       YJ = (FLOAT(J-JMIN)+.5)/YL*(YN-1.)+1.
         LOWY = YJ
         YPART = YJ-FLOAT(LOWY)
         IF (LOWY .NE. NY) GO TO 113
         LOWY = LOWY-1
         YPART = 1.
  113    IPEN = 0
         ZLFT = Z(1,LOWY)+YPART*(Z(1,LOWY+1)-Z(1,LOWY))
         ZRHT = Z(2,LOWY)+YPART*(Z(2,LOWY+1)-Z(2,LOWY))
         IF (NSPV .EQ. 0) GO TO 114
         IF (Z(1,LOWY).EQ.SP .OR. Z(2,LOWY).EQ.SP .OR.
     1       Z(1,LOWY+1).EQ.SP .OR. Z(2,LOWY+1).EQ.SP) IPEN = 1
  114    IF (IPEN .EQ. 1) GO TO 117
C
C FIND INT FOR THIS Z.
C
         IF (ZLFT .GT. ZLEV(LSRT+1)) GO TO 116
  115    IF (ZLFT .GE. ZLEV(LSRT)) GO TO 117
C
C LOOK LOWER
C
         IF (LSRT .LE. 1) GO TO 117
         LSRT = LSRT-1
         GO TO 115
C
C LOOK HIGHER
C
  116    IF (LSRT .GE. NLEVL) GO TO 117
         LSRT = LSRT+1
         IF (ZLFT .GT. ZLEV(LSRT+1)) GO TO 116
C
C OK
C
  117    IRHT = 2
         LAST = LSRT
         DO 129 I=IMIN,IMAX
C
C FIND X FOR THIS I AND Z FOR THIS X AND Y.
C
            IADD = 1
            XI = (FLOAT(I-IMIN)+.5)/XL*(XN-1.)+1.
            LOWX = XI
            XPART = XI-FLOAT(LOWX)
            IF (LOWX .NE. NX) GO TO 118
            LOWX = LOWX-1
            XPART = 1.
C
C TEST FOR INTERPOLATION POSITIONING
C
  118       IF (LOWX .LT. IRHT) GO TO 119
C
C MOVE INTERPOLATION ONE CELL TO THE RIGHT
C
            ZLFT = ZRHT
            IRHT = IRHT+1
            ZRHT = Z(IRHT,LOWY)+YPART*(Z(IRHT,LOWY+1)-Z(IRHT,LOWY))
            IF (NSPV .EQ. 0) GO TO 118
            IPEN = 0
            IF (Z(IRHT-1,LOWY).EQ.SP .OR. Z(IRHT,LOWY).EQ.SP .OR.
     1          Z(IRHT-1,LOWY+1).EQ.SP .OR. Z(IRHT,LOWY+1).EQ.SP)
     2          IPEN = 1
            GO TO 118
  119       IF (IPEN .NE. 1) GO TO 123
C
C SPECIAL VALUE AREA
C
            GO TO (129,120,121,122),NSPV
  120       MSPV = 1
            GO TO 129
  121       PX(1) = I*NCRTG
            PY(1) = J*NCRTG
            PX(2) = PX(1)+NCRTG-1
            PY(2) = PY(1)+NCRTG-1
            CALL GPL (2,PX,PY)
            PYTMP = PY(1)
            PY(1) = PY(2)
            PY(2) = PYTMP
            CALL GPL (2,PX,PY)
C
            GO TO 129
  122       INTEN = MXLEV
            GO TO 128
  123       ZZ = ZLFT+XPART*(ZRHT-ZLFT)
C
C TEST FOR SAME INT AS LAST TIME.
C
            IF (ZZ .GT. ZLEV(LAST+1)) GO TO 126
  124       IF (ZZ .GE. ZLEV(LAST)) GO TO 127
C
C LOOK LOWER
C
            IF (LAST .LE. 1) GO TO 125
            LAST = LAST-1
            GO TO 124
  125       IF (ZZ .LT. ZLEV(LAST)) IADD = 0
            GO TO 127
C
C LOOK HIGHER
C
  126       IF (LAST .GE. NLEVL) GO TO 127
            LAST = LAST+1
            IF (ZZ .GE. ZLEV(LAST+1)) GO TO 126
C
C OK
C
  127       ISUB = LAST+IOFFST+IADD
            INTEN = IL(ISUB)
            IF (NOPTN .LT. 0) INTEN = MXLEV-INTEN
  128       CALL GRAY
  129    CONTINUE
  130 CONTINUE
C
C PUT OUT ANY REMAINING BUFFERED POINTS.
C
      IF (NPOINT.GT.0) THEN
         CALL GQNT(1,IERR,WND2,VWPR2)
         CALL SET(0.,1.,0.,1.,0.,1023.,0.,1023.,1)
         CALL POINTS(XPNT,YPNT,NPOINT,0,0)
         CALL SET(VWPR2(1),VWPR2(2),VWPR2(3),VWPR2(4),
     -            WND2(1),WND2(2),WND2(3),WND2(4),1)
      ENDIF
C
C CALL BOUND IF ISPV=2 AND SPECIAL VALUES WERE FOUND.
C
      IF (MSPV .EQ. 1) THEN
        CALL SET(X1,X2,Y1,Y2,1.0,X3,1.0,Y3,1)
        CALL BOUND (Z,MX,NX,NY,SP)
      END IF
  132 CONTINUE
C
C RESTORE NORMALIZATION TRANS 1 AND ORIGINAL NORMALIZATION NUMBER
C
      CALL SET(VWPRT(1),VWPRT(2),VWPRT(3),VWPRT(4),
     -         WNDW(1),WNDW(2),WNDW(3),WNDW(4),IOLLS)
      CALL SETUSV('LS',IOLLS)
      CALL GSELNT (NTORIG)
      RETURN
C
      END
