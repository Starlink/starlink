      SUBROUTINE OVERLP4(XBAR,YBAR,PMAX,IMS,JCOUNT,MM,TOTAL,IFLAG)
C     *** OVERLP  looks for overlapping images using multiple isophotes

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters

C     .. Scalar Arguments ..
      REAL PMAX,TOTAL,XBAR,YBAR
      INTEGER IFLAG,IMS,JCOUNT,MM
C     ..
C     .. Scalars in Common ..
      REAL CONST,OFFSET,THRESH
      INTEGER IANAL,IPIX,NBIT,NPT
C     ..
C     .. Arrays in Common ..
      REAL*4 PARM(16,IMNUM)
      INTEGER*4 IJIST(PIXLIM),ILIST(IMLIM),JJIST(PIXLIM),JLIST(IMLIM),
     +     KJIST(PIXLIM),KLIST(IMLIM),NJIST(PIXLIM),SILIST(PIXLIM),
     +     SJLIST(PIXLIM),SKLIST(PIXLIM)
C     ..
C     .. Local Scalars ..
      REAL ALGTHR,DELB,DELI,DIST,DLBYDR,PI,RADIUS,RADMAX,RADOLD,RADTHR,
     +     RATIO,SLOPE,SMUL,SUM,SUMINT,SXX,SXY,SYY,T,TEMP,TMUL,TTT,WT,X,
     +     XB,XEFF,XLEVEL,XLEVOL,XOFF,XSQ,XYSQ,Y,YB,YOFF,YSQ
      INTEGER I,IC,ICOUNT,IDIS,IDX,IGLAG,IH,II,IJMAX,IL,
     +     IOFFS,IPIXO2,ISC,ISOPH,ISPLIT,IT,ITER,ITHRES,ITMAX,J,JH,
     +     JJ,JL,K,L,LL,LPT,LX,LY,MIMNO,MPT,NEWTHR,NEXTHR,NOBJ,NUMIM
C     ..
C     .. Local Arrays ..
      REAL*4 POLYCF(3),XCOR(9),XDAT(9)
      INTEGER IPOINT(IMNUM),MPOINT(IMNUM),NPOINT(IMNUM)
      INTEGER*4 IAP(8)
      INTEGER*4 IBITL(IMNUM),IBITX(IMNUM),IBITY(IMNUM),MILIST(PIXLIM),
     +     MJLIST(PIXLIM),MKLIST(PIXLIM)
C     ..
C     .. Common blocks ..
      COMMON /AN/IJIST,JJIST,KJIST,NJIST,SILIST,SJLIST,SKLIST,NPT
      COMMON /OV/ILIST,JLIST,KLIST,THRESH,IPIX,PARM,NBIT,CONST,OFFSET,
     +     IANAL
C     ..
      ISOPH = IFLAG
      IOFFS = NINT(OFFSET)
      ALGTHR = ALOG(THRESH)
      PI = 4.0*ATAN(1.0)
      IL = IMS
      IH = IMS + JCOUNT - 1
      RADMAX = SQRT(FLOAT(JCOUNT)/PI)
C     *** 1/2 mag
      TMUL = 1.585
C     *** 1/4 mag
      TMUL = SQRT(TMUL)
      ITHRES = NINT(THRESH)
C     *** use higher intitial threshold if not smoothed
      SMUL = 2.0
      IF (IANAL.NE.1) SMUL = 2.5
      NEWTHR = NINT(SMUL*THRESH)
      IPIXO2 = (IPIX+1)/2
      IPIXO2 = MAX0(2,IPIXO2)
 10   CONTINUE
C     *** check no. of points above threshold and store
      NPT = 0
      DO 20 I = IL,IH
         IF (KLIST(I).GT.NEWTHR) THEN
            NPT = NPT + 1
            IF (NPT.GT.PIXLIM) THEN
               GOTO 30
            ELSE
               SILIST(NPT) = ILIST(I)
               SJLIST(NPT) = JLIST(I)
               SKLIST(NPT) = KLIST(I)
            ENDIF

         ENDIF

 20   CONTINUE
      GOTO 40

 30   NEWTHR = NEWTHR + NINT(THRESH)
      GOTO 10

 40   IF (NPT.LT.IPIX) THEN
         IFLAG = 0
         NBIT = 1

      ELSE
C     ***
C     *** main analysis loop at new thresholds
C     ***
         NBIT = 0
         LPT = 0
         MIMNO = 0
         ISPLIT = 0
 50      CONTINUE
         CALL ANALYS4
C     *** find image boundaries
         NOBJ = NJIST(1)
         ICOUNT = 1
         NUMIM = 0
         DO 60 I = 2,NPT
            IF (NOBJ.EQ.NJIST(I)) THEN
               ICOUNT = ICOUNT + 1
               LL = I
            ENDIF

            IF ((NOBJ.EQ.NJIST(I).AND.I.EQ.NPT) .OR.
     +           NOBJ.NE.NJIST(I)) THEN
C     *** reject small pixel fragments
               IF (ICOUNT.GE.IPIXO2) THEN
                  IF (NUMIM.NE.IMNUM) THEN
                     NUMIM = NUMIM + 1
                     IPOINT(NUMIM) = LL + 1 - ICOUNT
                     IF (ICOUNT.LT.IPIX) ICOUNT = -ICOUNT
                     NPOINT(NUMIM) = ICOUNT
                  ENDIF
               ENDIF
               ICOUNT = 1
               IF (I.NE.NPT) NOBJ = NJIST(I)
            ENDIF
 60      CONTINUE

C     *** test to see if lost it altogther
         IF (NUMIM.EQ.0) THEN
            NUMIM = 1
            IPOINT(NUMIM) = 1
            NPOINT(NUMIM) = -NPT
         ENDIF
C     *** flag for first splitting
         IF (NUMIM.GT.1 .AND. ISPLIT.EQ.0) ISPLIT = 1
C     *** for each image check no. of points above next threshold and flag
         ISC = 0
         NEXTHR = NINT(NEWTHR*TMUL)
         NEXTHR = MAX0(NEWTHR+ITHRES,NEXTHR)
         DO 70 I = 1,NUMIM
            JL = IPOINT(I)
            JH = JL + IABS(NPOINT(I)) - 1
            MPT = 0
            XB = 0.0
            YB = 0.0
            XSQ = 0.0
            YSQ = 0.0
            XYSQ = 0.0
            XOFF = IJIST(JL)
            YOFF = JJIST(JL)
            DO 80 J = 1,8
               IAP(J) = 0
 80         CONTINUE
            IJMAX = 0
            SUM = 0.0
            DO 90 J = JL,JH
               IF (KJIST(J).GE.NEXTHR) MPT = MPT + 1
               IT = KJIST(J) - NEWTHR
               T = IT
               X = IJIST(J) - XOFF
               Y = JJIST(J) - YOFF
               SUM = SUM + T
               XB = XB + T*X
               YB = YB + T*Y
C     *** + 1/12 to allow for finite pixel size
               XSQ = XSQ + (X*X+1.0/12.0)*T
               YSQ = YSQ + (Y*Y+1.0/12.0)*T
               XYSQ = XYSQ + X*Y*T
               CALL UPDATE(IAP,T,0.0,CONST,OFFSET)
               IF (IT.GT.IJMAX) THEN
                  LX = IJIST(J)
                  LY = JJIST(J)
                  IJMAX = IT
               ENDIF
 90         CONTINUE

C     *** compute image parameters
            XB = XB/SUM
            YB = YB/SUM
            SXX = AMAX1(0.0,XSQ/SUM-XB**2)
            SYY = AMAX1(0.0,YSQ/SUM-YB**2)
            SXY = XYSQ/SUM - XB*YB
            XB = XB + XOFF
            YB = YB + YOFF
            IF (SXY.GT.0.0) THEN
               SXY = AMIN1(SXY,SQRT(SXX*SYY))
               SXY = AMAX1(1.0e-4,SXY)
            ELSE
               SXY = AMAX1(SXY,-SQRT(SXX*SYY))
               SXY = AMIN1(-1.0e-4,SXY)
            ENDIF
C     *** see if image already in list
            IF (NBIT.NE.0) THEN
               DO 100 K = 1,NBIT
                  IF (IBITX(K).EQ.LX) THEN
                     IF (IBITY(K).EQ.LY) GOTO 110
                  ENDIF
 100           CONTINUE

               GOTO 120
C     *** same keep old info - if cog changed by lots overwrite
 110           IF ((XB-PARM(4,K))**2+ (YB-PARM(2,K))**2.GT.1.0 .OR.
     +              ISPLIT.EQ.1) THEN
                  PARM(1,K) = SUM
                  PARM(4,K) = XB
                  PARM(2,K) = YB
                  PARM(3,K) = NEWTHR
                  PARM(5,K) = SXX
                  PARM(6,K) = SXY
                  PARM(7,K) = SYY
                  PARM(8,K) = IJMAX
                  DO 130 J = 1,8
                     PARM(8+J,K) = IAP(J)
 130              CONTINUE
               ENDIF
               GOTO 140
            ENDIF
C     *** store new one
 120        CONTINUE
            NBIT = NBIT + 1
            IF (NBIT.GT.IMNUM) THEN
               NBIT = IMNUM
               WRITE (*,*) ' '
               WRITE (*,*) ' Warning there are more than',IMNUM,
     +              ' images in fragment'
               WRITE (*,*) ' No. of images truncated to',IMNUM
               WRITE (*,*) ' '
               GOTO 150
            ELSE
               IBITX(NBIT) = LX
               IBITY(NBIT) = LY
               PARM(1,NBIT) = SUM
               PARM(4,NBIT) = XB
               PARM(2,NBIT) = YB
               PARM(3,NBIT) = NEWTHR
               PARM(5,NBIT) = SXX
               PARM(6,NBIT) = SXY
               PARM(7,NBIT) = SYY
               PARM(8,NBIT) = IJMAX
               DO 160 J = 1,8
                  PARM(8+J,NBIT) = IAP(J)
 160           CONTINUE
            ENDIF

 140        IF (MPT.GE.IPIX .AND. NPOINT(I).GE.0) THEN
C     *** copy analysis lists to storage lists for next threshold analysis
               DO 170 J = JL,JH
                  IF (KJIST(J).GT.NEXTHR) THEN
                     ISC = ISC + 1
                     SILIST(ISC) = IJIST(J)
                     SJLIST(ISC) = JJIST(J)
                     SKLIST(ISC) = KJIST(J)
                  ENDIF

 170           CONTINUE
               GOTO 70

            ENDIF
C     *** update master storage list for terminated images
 150        CONTINUE
            MIMNO = MIMNO + 1
            IF (MIMNO.GT.IMNUM) THEN
               GOTO 180

            ELSE

               MPOINT(MIMNO) = LPT + 1
               DO 190 J = JL,JH
                  LPT = LPT + 1
                  MILIST(LPT) = IJIST(J)
                  MJLIST(LPT) = JJIST(J)
                  MKLIST(LPT) = KJIST(J)
 190           CONTINUE
            ENDIF

 70      CONTINUE
         IF (ISPLIT.EQ.1) ISPLIT = 2
         GOTO 200

 180     MIMNO = IMNUM
         WRITE (*,*) ' *** Warning *** more than',IMNUM, ' images found'
C     *** check for any thing left
 200     CONTINUE
         IF (ISC.NE.0 .AND. MIMNO.NE.IMNUM) THEN
C     *** set up next round
            NPT = ISC
            NEWTHR = NEXTHR
            GOTO 50

         ENDIF
C     ***
C     *** Now have master lists of tops of separate images
C     ***
         IF (MIMNO.EQ.1) THEN
            IFLAG = 0
            NBIT = 1

         ELSE
C     *** calculate image coordinates and peak height
            DO 210 K = 1,MIMNO
               JL = MPOINT(K)
               IF (K.EQ.MIMNO) THEN
                  JH = LPT

               ELSE

                  JH = MPOINT(K+1) - 1
               ENDIF

               ITMAX = 0
               DO 220 I = JL,JH
                  IT = MKLIST(I)
                  IF (IT.GT.ITMAX) THEN
                     LX = MILIST(I)
                     LY = MJLIST(I)
                     ITMAX = IT
                  ENDIF

 220           CONTINUE

C     *** match up tops with bits
               L = 0
               IDX = PIXLIM
               DO 230 I = 1,NBIT
                  IF (IBITX(I).GE.0) THEN
                     IDIS = (IBITX(I)-LX)**2 + (IBITY(I)-LY)**2
                     IF (IDIS.LE.IDX) THEN
                        L = I
                        IDX = IDIS
                     ENDIF

                  ENDIF

 230           CONTINUE
               IF (L.NE.0) THEN
C     *** flag parm list
                  PARM(1,L) = -PARM(1,L)
                  IBITX(L) = -1
               ENDIF

 210        CONTINUE

            IFLAG = 1
            DO 240 I = 1,NBIT
               IF (PARM(1,I).LT.0.0) THEN
                  IBITX(I) = 0
                  IBITL(I) = 0
                  PARM(1,I) = -PARM(1,I)

               ELSE

                  PARM(1,I) = -PARM(1,I)
               ENDIF

 240        CONTINUE

C     ***
C     *** for each image find true areal profile levels
C     ***
C     *** iterate to find local continuum
C     ***
            TEMP = THRESH
            IF (ISOPH.EQ.1) TEMP = 0.0
            ITER = 0
            SUMINT = 0.0
            IGLAG = 0
 250        CONTINUE
            ITER = ITER + 1
            DO 260 K = 1,NBIT
               IF (PARM(1,K).GE.0.0) THEN
                  XLEVOL = ALOG(PARM(8,K)+PARM(3,K)-IBITL(K))
                  RADOLD = 0.0
                  TTT = 0.0
                  SLOPE = 1.0
                  IC = 0
                  DO 270 I = 1,8
                     JJ = 17 - I
                     II = 9 - I
                     IF (PARM(JJ,K).GE.0.5) THEN
                        IF (II.EQ.1) THEN
                           XLEVEL = ALOG(PARM(3,K)-IBITL(K)+0.5)

                        ELSE

                           XLEVEL = ALOG(2.0** (II+IOFFS-1)+
     +                          PARM(3,K)-IBITL(K)-0.5)
                        ENDIF

                        RADIUS = SQRT(PARM(JJ,K)/PI)
                        IC = IC + 1
                        XDAT(IC) = XLEVEL
                        XCOR(IC) = RADIUS
                        DLBYDR = (XLEVOL-XLEVEL)/
     +                       AMAX1(0.01,RADIUS-RADOLD)
                        WT = AMAX1((RADIUS-RADOLD)*5.0,0.1)
                        WT = AMIN1(WT,1.0)
                        SLOPE = (1.0-0.5*WT)*SLOPE +
     +                       0.5*WT*AMIN1(5.0,DLBYDR)
                        RADOLD = RADIUS
                        XLEVOL = XLEVEL
                     ENDIF

 270              CONTINUE
                  IF (IGLAG.EQ.1) THEN
C     *** now update parameters
                     IF (IC.GT.2) THEN
                        CALL POLYNM(XDAT,XCOR,IC,POLYCF,3,0)
                        TTT = POLYCF(2) + 2.0*POLYCF(3)*RADIUS
                     ENDIF

                     SLOPE = AMAX1(-TTT,SLOPE)
                     RADTHR = RADIUS + (XLEVEL-ALGTHR)/SLOPE
                     IF (RADTHR.GT.RADMAX) THEN
                        SLOPE = (XLEVEL-ALGTHR)/ (RADMAX-RADIUS)
                        RADTHR = RADMAX
                     ENDIF
C     *** pixel area
                     DELB = PARM(9,K)* (PARM(3,K)-IBITL(K))
                     PARM(9,K) = PI*RADTHR**2
C     *** peak height
                     PARM(8,K) = PARM(8,K) + (PARM(3,K)-IBITL(K))
C     *** intensity
                     DELI = 2.0*PI* ((PARM(3,K)-IBITL(K))*
     +                    (1.0+SLOPE*RADIUS)-
     +                    TEMP* (1.0+SLOPE*RADTHR))/SLOPE**2
                     PARM(1,K) = PARM(1,K) + DELB + AMAX1(0.0,DELI)
                     DO 280 I = 1,7
                        PARM(I+9,K) = -1.0
 280                 CONTINUE
                     SUMINT = SUMINT + PARM(1,K)

                  ELSE
C     *** estimate effect on local continuum from each image
                     DO 290 I = 1,NBIT
                        IF (PARM(1,I).GE.0.0 .AND. I.NE.K) THEN
                           DIST = SQRT((PARM(4,K)-PARM(4,I))**2+
     +                          (PARM(2,K)-PARM(2,I))**2)
                           XEFF = XLEVEL - AMAX1(0.0,
     +                          SLOPE* (DIST-RADIUS))
                           IBITX(I) = IBITX(I) + NINT(EXP(XEFF))
                        ENDIF

 290                 CONTINUE
                  ENDIF

               ENDIF

 260        CONTINUE
            IF (IGLAG.NE.1) THEN
C     *** check changes in continuum
               IGLAG = 1
               DO 300 I = 1,NBIT
                  IF (PARM(1,I).GE.0.0) THEN
                     IF (IABS(IBITX(I)-IBITL(I)).GT.3) IGLAG = 0
                     IBITL(I) = IBITX(I)
                     IBITX(I) = 0
                     IBITL(I) = MIN0(IBITL(I),
     +                    NINT(PARM(3,I)-THRESH))
                  ENDIF

 300           CONTINUE
               IF (ITER.EQ.5) IGLAG = 1
               GOTO 250

            ENDIF

            IF (SUMINT.EQ.0.0) THEN
               IFLAG = 0
               NBIT = 1

            ELSE

               RATIO = TOTAL/SUMINT
C     *** remove surplus images
               II = 0
               DO 310 K = 1,NBIT
                  IF (PARM(1,K).GT.0.0) THEN
                     II = II + 1

                     IF (II.NE.K) THEN
                        DO 320 I = 1,16
                           PARM(I,II) = PARM(I,K)
 320                    CONTINUE
                     ENDIF

                  ELSE

                  ENDIF

 310           CONTINUE
               NBIT = II
               DO 330 I = 1,NBIT
                  PARM(1,I) = RATIO*PARM(1,I)
 330           CONTINUE
            ENDIF

         ENDIF

      ENDIF

      END
