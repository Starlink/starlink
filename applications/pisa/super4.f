      SUBROUTINE SUPER4(IMS,JCOUNT,AVCHI,NUMCHI,SIGMA,PSF,IFSM)

C     *** SUPER  does pseudo ls refinement using initial coord list

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters

C     .. Scalar Arguments ..
      REAL AVCHI,PSF,SIGMA
      INTEGER IFSM,IMS,JCOUNT,NUMCHI
C     ..
C     .. Scalars in Common ..
      REAL CHANGE,OFFSET,PARM1,PARM2,PARMN,PARMN1,PARMN2,PARMNN,PARRAD,
     +     PARSQ,PHRESH,Q,SKYCOR
      INTEGER IANAL,IB,IMODEL,IPIX,IUPP,KONST,NBIT
C     ..
C     .. Arrays in Common ..
      REAL*4 PARM(16,IMNUM),XCORDL(IMNUM),XCORDS(IMNUM),YCORDL(IMNUM),
     +     YCORDS(IMNUM)
      INTEGER*4 ILIST(IMLIM),JLIST(IMLIM),KLIST(IMLIM)
C     ..
C     .. Local Scalars ..
      REAL ARG,CHISQO,CHISQU,CONST,DISLIM,DISSQ,DMODEL,EDGE,FUPP,PI,
     +     PSFSQ,RAD,RADSQ,RANGE,RSAT,SIGSQ,SUM,T,TDEL,TEMP,X,XDEL,
     +     XINTMN,XINTTP,XMTHR,XYDIS,XYOLD,XYREF,Y,YDEL,YINTMN,ZINTMN
      INTEGER I,ICC,IEXTRA,IFLAG,IHH,II,ILL,IREM,IRLIM,ITER,J,K,
     +     LT,MINPIX,NSAT,NTER
C     ..
C     .. Local Arrays ..
      REAL*4 ERRC(IMNUM),ERRI(IMNUM),SINT(IMNUM),SXDEL(IMNUM),
     +       SYDEL(IMNUM), TMN(IMNUM),XDAMP(IMNUM),XMN(IMNUM),
     +       XMODEL(PIXLIM),YDAMP(IMNUM),YMN(IMNUM)
      INTEGER*4 IREF(IMNUM)
C     ..
C     .. Common blocks ..
      COMMON /OV/ILIST,JLIST,KLIST,PHRESH,IPIX,PARM,NBIT,KONST,OFFSET,
     +     IANAL
      COMMON /PM/PARM1,PARM2,PARMN,PARSQ,CHANGE,PARRAD,IUPP,SKYCOR,IB
      COMMON /PMN/PARMN1,PARMN2,PARMNN,Q,IMODEL
      COMMON /ST/XCORDS,YCORDS,XCORDL,YCORDL

C     ..
      PI = 4.0*ATAN(1.0)
      LT = 6
      ILL = IMS
      IHH = IMS + JCOUNT - 1
      IF (JCOUNT.LE.PIXLIM) THEN

C     *** save starting coordinates
         DO 10 I = 1,NBIT
            XCORDL(I) = PARM(4,I)
            YCORDL(I) = PARM(2,I)
            XCORDS(I) = PARM(4,I)
            YCORDS(I) = PARM(2,I)
 10      CONTINUE
         SIGSQ = SIGMA**2
         PSFSQ = PSF**2
         CONST = 4.0*PI*SIGSQ*PSFSQ

C     *** limit for edge effects
         EDGE = PSF

C     *** saturation limit
         FUPP = FLOAT(IUPP)

C     *** minimum intensity for theoretical bit
         XINTMN = SIGMA/PARMN

C     *** minimum intensity during refinement
         YINTMN = PI*PSF**2*SIGMA*0.5

C     *** after
         ZINTMN = PHRESH/PARMN
         ZINTMN = AMAX1(ZINTMN,2.0*YINTMN)

C     *** minimum pixel size for extra images
         MINPIX = IPIX
         MINPIX = MAX0(4,MINPIX)
         IEXTRA = 0
         IREM = 0

C     *** look for saturated pixels
         NSAT = 0
         DO 20 I = ILL,IHH
            IF (KLIST(I).GE.IUPP) NSAT = NSAT + 1
 20      CONTINUE

C     *** limit for rejecting spurious images
         DISLIM = PSF
         RSAT = SQRT(FLOAT(NSAT)/PI)
         DISSQ = (DISLIM+RSAT)**2

C     *** limit for introducing new ones
         RANGE = DISSQ/16.0

C     *** model fitting threshold
         XMTHR = AMAX1(1.0,0.5*SIGMA)

C     *** now do refinement of position and intensity
         CHISQO = 1.0e10
         NTER = 0
         SKYCOR = 0.0
 30      CONTINUE
         NTER = NTER + 1
         IFLAG = 0

C     *** if already removed images this cycle skip this bit
         IF (IREM.EQ.0) THEN

C     *** if any images are within 1.0*psf use larger only
            DO 40 I = 1,NBIT
               IF (PARM(1,I).GE.0.5) THEN
                  DO 50 J = 1,NBIT
                     IF (J.NE.I) THEN
                        IF (PARM(1,J).GE.0.5) THEN
                           XYDIS = (PARM(4,I)-PARM(4,J))**2 +
     +                          (PARM(2,I)-PARM(2,J))**2
                           XYOLD = (XCORDL(I)-XCORDL(J))**2 +
     +                          (YCORDL(I)-YCORDL(J))**2
                           IF (XYDIS.LE.DISSQ) THEN
                              IF (PARM(1,I).GT.PARM(1,J)) THEN

C     must have moved 1/4 psf sigma, if nearer than 1/4 psf throw out
C     anyway, must be moving toward image
                                 IF (XYDIS.GE.RANGE) THEN
                                    XYREF = (PARM(4,J)-
     +                                   XCORDS(J))**2 +
     +                                   (PARM(2,J)-
     +                                   YCORDS(J))**2
                                    IF (XYREF.LT.RANGE .OR.
     +                                   XYDIS.GT.XYOLD)
     +                                   GOTO 50
                                 ENDIF
                                 PARM(1,J) = 0.0
                                 IFLAG = 1
                              ELSE
                                 IF (XYDIS.GE.RANGE) THEN
                                    XYREF = (PARM(4,I)-
     +                                   XCORDS(I))**2 +
     +                                   (PARM(2,I)-
     +                                   YCORDS(I))**2
                                    IF (XYREF.LT.RANGE .OR.
     +                                   XYDIS.GT.XYOLD)
     +                                   GOTO 50
                                 ENDIF
                                 PARM(1,I) = 0.0
                                 IFLAG = 1
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
 50               CONTINUE
               ENDIF
 40         CONTINUE
         ENDIF
         IF (IFLAG.EQ.1) NTER = NTER - 1

C     *** approx theoretical parameter errors
         DO 60 I = 1,NBIT
            SXDEL(I) = 0.0
            SYDEL(I) = 0.0
            XDAMP(I) = 1.0
            YDAMP(I) = 1.0
            XINTTP = AMAX1(XINTMN,PARM(1,I))
            ERRI(I) = SQRT(XINTTP* (1.0+0.5*CONST/XINTTP))
            ERRC(I) = SQRT(PSFSQ* (1.0+CONST/XINTTP)/ (2.0*XINTTP))
            IF (NTER.GT.1) ERRC(I) = 0.5*ERRC(I)

C     *** at least 1/100 th of a pixel
            ERRC(I) = AMAX1(0.01,ERRC(I))
 60      CONTINUE
         ITER = 0
 70      CONTINUE
         ITER = ITER + 1

C     *** compute model function for all pixels
         DO 80 I = ILL,IHH
            SUM = 0.0
            DO 90 K = 1,NBIT
               IF (PARM(1,K).GE.0.5) THEN
                  X = PARM(4,K) - ILIST(I)
                  Y = PARM(2,K) - JLIST(I)

C     *** replaced FUNC between here
                  RADSQ = X*X + Y*Y
                  RAD = SQRT(RADSQ)
                  IF (RAD.LT.PARRAD) THEN
                     ARG = PARM1*RADSQ
                     T = PARMN*Q/ (1.0+PARMN2*RADSQ)
                     IF (ARG.GT.-16.0) T = T +
     +                    (1.0-Q)*PARMN*EXP(ARG)
                  ELSE
                     ARG = CHANGE + (PARRAD-RAD)*PARM2
                     T = PARMN*Q/ (1.0+PARMN2*RADSQ)
                     IF (ARG.GT.-16.0) T = T +
     +                    (1.0-Q)*PARMN*EXP(ARG)
                  ENDIF

C     *** and here
                  SUM = SUM + PARM(1,K)*T
               ENDIF
 90         CONTINUE
            XMODEL(I-ILL+1) = SUM
 80      CONTINUE

C     *** global convergence statistic
         IF (ITER.EQ.1) THEN
            DO 100 K = 1,NBIT
               IREF(K) = 1
 100        CONTINUE
            CHISQU = 0.0
            ICC = 0
            DO 110 I = ILL,IHH

C     *** miss out saturated pixels
               IF (KLIST(I).LT.IUPP) THEN
                  ICC = ICC + 1
                  CHISQU = CHISQU + (XMODEL(I-ILL+1)-KLIST(I)+
     +                 SKYCOR)**2
               ENDIF
 110        CONTINUE
            CHISQU = CHISQU/ (SIGSQ*FLOAT(ICC))

C     if(chisqu.gt.0.99*chisqo)goto 780
            CHISQO = CHISQU
         ENDIF

C     *** now do x and y positions
         DO 120 I = 1,NBIT

C     *** save old coords
            XCORDL(I) = PARM(4,I)
            YCORDL(I) = PARM(2,I)
            XMN(I) = 0.0
            TMN(I) = 0.0
            YMN(I) = 0.0
 120     CONTINUE
         DO 130 I = ILL,IHH
            IF (XMODEL(I-ILL+1).GE.XMTHR) THEN
               DO 140 K = 1,NBIT
                  IF (IREF(K).NE.0) THEN
                     IF (PARM(1,K).GE.0.5) THEN
                        X = PARM(4,K) - ILIST(I)
                        Y = PARM(2,K) - JLIST(I)
                        RADSQ = X*X + Y*Y
                        ARG = PARM1*RADSQ
                        IF (ARG.LT.-16.0) THEN
                           DMODEL = 0.0
                        ELSE
                           DMODEL = EXP(ARG)
                        ENDIF
                        TEMP = KLIST(I)*DMODEL/XMODEL(I-ILL+1)
                        XMN(K) = XMN(K) + TEMP*ILIST(I)
                        YMN(K) = YMN(K) + TEMP*JLIST(I)
                        TMN(K) = TMN(K) + TEMP
                     ENDIF
                  ENDIF
 140           CONTINUE
            ENDIF
 130     CONTINUE

C     *** check on errors and update
         IFLAG = 0
         DO 150 K = 1,NBIT
            IF (TMN(K).NE.0.0) THEN
               IREF(K) = 1
               IF (PARM(1,K).GE.0.5) THEN
                  XMN(K) = XMN(K)/TMN(K)
                  YMN(K) = YMN(K)/TMN(K)
                  XDEL = XMN(K) - PARM(4,K)
                  YDEL = YMN(K) - PARM(2,K)
                  IF (XDEL.GT.1.0) XDEL = 1.0
                  IF (XDEL.LT.-1.0) XDEL = -1.0
                  IF (YDEL.GT.1.0) YDEL = 1.0
                  IF (YDEL.LT.-1.0) YDEL = -1.0
                  IF (ABS(XDEL)+ABS(YDEL).LT.
     +                 0.5*ERRC(K)) IREF(K) = 0

C     *** test to stop oscillation
                  IF (XDEL*SXDEL(K).LT.0.0) XDAMP(K) = XDAMP(K) -
     +                 0.25
                  IF (YDEL*SYDEL(K).LT.0.0) YDAMP(K) = YDAMP(K) -
     +                 0.25
                  XDEL = XDEL*XDAMP(K)
                  YDEL = YDEL*YDAMP(K)
                  PARM(4,K) = PARM(4,K) + XDEL
                  PARM(2,K) = PARM(2,K) + YDEL
                  IF (ABS(XDEL).GT.ERRC(K) .OR.
     +                 ABS(YDEL).GT.ERRC(K)) IFLAG = 1
                  SXDEL(K) = XDEL
                  SYDEL(K) = YDEL
               ENDIF
            ENDIF
 150     CONTINUE

C     *** convergence test for coordinates
         IF (IFLAG.EQ.1 .AND. ITER.LT.5) GOTO 70

C     *** check if can remove any images
         IF (IEXTRA.GT.0 .AND. NTER.GT.1) THEN
            IREM = 0
            IRLIM = MAX0(1,NINT(0.1*NBIT))
            DO 160 I = 1,NBIT
               IF (PARM(1,I).GE.0.5) THEN
                  DO 170 J = 1,NBIT
                     IF (I.NE.J) THEN
                        IF (PARM(1,J).GE.0.5) THEN
                           XYDIS = (PARM(4,I)-PARM(4,J))**2 +
     +                          (PARM(2,I)-PARM(2,J))**2
                           XYOLD = (XCORDL(I)-XCORDL(J))**2 +
     +                          (YCORDL(I)-YCORDL(J))**2
                           IF (XYDIS.LE.PSFSQ .AND.
     +                          XYDIS.LE.XYOLD) THEN
                              IF (PARM(1,I).GT.PARM(1,J)) THEN
                                 PARM(1,J) = 0.0
                              ELSE
                                 PARM(1,I) = 0.0
                              ENDIF
                              NTER = NTER - 1
                              IREM = IREM + 1

C     *** don't remove more than 10% at a time
                              IF (IREM.EQ.IRLIM) GOTO 180
                           ENDIF
                        ENDIF
                     ENDIF
 170              CONTINUE
               ENDIF
 160        CONTINUE
         ENDIF
 180     II = 0
         DO 190 I = 1,NBIT
            IF (PARM(1,I).GT.0.5) THEN
               II = II + 1
               IF (II.NE.I) THEN
                  XCORDS(II) = XCORDS(I)
                  YCORDS(II) = YCORDS(I)
                  XCORDL(II) = XCORDL(I)
                  YCORDL(II) = YCORDL(I)
                  DO 200 K = 1,16
                     PARM(K,II) = PARM(K,I)
 200              CONTINUE
               ENDIF
            ENDIF
 190     CONTINUE
         NBIT = II

C     *** now do intensities
         DO 210 I = 1,NBIT
            SINT(I) = PARM(1,I)
 210     CONTINUE
         CALL PHOPT24(IMS,JCOUNT)

C     *** check for convergence
         IFLAG = 0
         DO 220 I = 1,NBIT
            PARM(1,I) = AMAX1(0.0,PARM(1,I))
            IF (SINT(I).LT.YINTMN) THEN
               PARM(1,I) = 0.0
            ELSE
               TDEL = PARM(1,I) - SINT(I)
               IF (ABS(TDEL).GT.ERRI(I)) IFLAG = 1
            ENDIF
 220     CONTINUE

C     write(lt,7500) nter,parm(1,i),tdel,erri(i),sint(i)
         IF (IFLAG.EQ.1 .AND. NTER.LT.10) THEN
            GOTO 30

*     check if any features left in diff map if doing full surface
*     modelling
         ELSEIF (IEXTRA.EQ.0 .AND. IFSM.EQ.1) THEN
            CALL FEATUR4(XMODEL,IMS,JCOUNT,MINPIX,SIGMA,RANGE,IEXTRA,
     +           EDGE)
            IF (IEXTRA.GT.0) THEN
               NTER = 0
               SKYCOR = 0.0
               GOTO 30
            ELSE
               GOTO 230
            ENDIF
         ENDIF
C     *** finish for blend
 230     CONTINUE
         AVCHI = AVCHI + FLOAT(JCOUNT)*CHISQU
         NUMCHI = NUMCHI + JCOUNT
C     *** see how many images and fill in parm array
         II = 0
         DO 240 K = 1,NBIT
            IF (PARM(1,K).GT.ZINTMN) THEN
               II = II + 1
               IF (II.NE.K) THEN
                  DO 250 I = 1,16
                     PARM(I,II) = PARM(I,K)
 250              CONTINUE
               ENDIF
            ENDIF
 240     CONTINUE
         NBIT = II
         DO 260 K = 1,NBIT
            PARM(5,K) = 0.5*PSF
            PARM(6,K) = 0.0
            PARM(7,K) = 0.5*PSF
            PARM(8,K) = PARM(1,K)*PARMN
            PARM(9,K) = PI*PSFSQ*ALOG(PARM(8,K)/PHRESH)
            PARM(9,K) = AMAX1(1.0,PARM(9,K))
            PARM(10,K) = -1.0
            PARM(11,K) = -1.0
            PARM(12,K) = -1.0
            PARM(13,K) = -1.0
            PARM(14,K) = -1.0
            PARM(15,K) = -1.0
            PARM(16,K) = -1.0
 260     CONTINUE
      ENDIF

      END
