      SUBROUTINE PHOPT4(MAP,PARM,NBIT,REC,XPEAK,R,SIGSQ)
C     *** PHOPT  does multiple profile fitting to determine intensities

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters
C     ..
C     .. Scalar Arguments ..
      REAL R,SIGSQ,XPEAK
      INTEGER NBIT
C     ..
C     .. Array Arguments ..
      REAL*4 PARM(16,IMNUM),REC(16)
      INTEGER*4 MAP(*)
C     ..
C     .. Scalars in Common ..
      REAL C1,C2,CHANGE,CN,PARMN1,PARMN2,PARMNN,PARRAD,PARSQ,Q,SKYCOR
      INTEGER IB,IMODEL,ISTART,ISTOP,IUPP,IXH,IXL,NWORD
C     ..
C     .. Local Scalars ..
      REAL A,ARG,ARG1,B,C,CLIM,CLIMSQ,CTHETA,ECC,ECCOLD,FUPP,PA,PB,PC,
     +     PI,PIO2,PT1,PT2,PT3,RAD,RADSQ,RATIO,RNEW,RSQ,SFAC,SRR,STHETA,
     +     STRECH,SXX,SXY,SYY,T,THETA,TJ,TK,XCOR,XI,XJ,XK,XNISO,XX,YCOR,
     +     YI,YJ,YK,YLIML,YLIMU
      INTEGER I,II,IRECH,IRECL,IYLIML,IYLIMU,J,JL,JU,K,KK,LT,NYOUT
C     ..
C     .. Local Arrays ..
      REAL*8 AA(IMNUM+1,IMNUM+1),BB(IMNUM+1)
      BYTE CC(IMNUM,IMNUM)
C     ..
C     .. Common blocks ..
      COMMON /FIL/ISTART,ISTOP,NWORD,IXL,IXH
      COMMON /PM/C1,C2,CN,PARSQ,CHANGE,PARRAD,IUPP,SKYCOR,IB
      COMMON /PMN/PARMN1,PARMN2,PARMNN,Q,IMODEL
C     ..
      PI = 4.0*ATAN(1.0)
      PIO2 = PI/2.0
      LT = 6
      FUPP = IUPP
      NYOUT = IXH - IXL + 1
C     *** set up flag array matrix
      CC(1,1) = 1
      IF (NBIT.GT.1) THEN
         DO 10 I = 1,NBIT
            XI = PARM(4,I)
            YI = PARM(2,I)
            JL = I + 1
            CC(I,I) = 1
            DO 20 J = JL,NBIT
               IF ((XI-PARM(4,J))**2+ (YI-PARM(2,J))**2.GT.
     +              PARSQ) THEN
                  CC(J,I) = 0
                  CC(I,J) = 0
               ELSE
                  CC(J,I) = 1
                  CC(I,J) = 1
               ENDIF

 20         CONTINUE
 10      CONTINUE
      ENDIF
C     *** get image parms
      XNISO = REC(1)
      XCOR = REC(4)
      YCOR = REC(2)
      SXX = REC(5)
      SXY = REC(6)
      SYY = REC(7)
      SRR = AMAX1(0.5,SXX+SYY)
      ECC = SQRT((SXX-SYY)**2+4.0*SXY**2)/SRR
      ECC = AMIN1(0.9,ECC)
      XX = 0.5* (1.0+ECC)*SRR - SYY
      IF (SXY.EQ.0.0) THETA = 0.0
      IF (XX.EQ.0.0) THEN
         THETA = PIO2
      ELSE
         THETA = ATAN(SXY/XX)
      ENDIF

      CTHETA = COS(THETA)
      STHETA = SIN(THETA)
C     *** ecc modified by noise effect
      ECCOLD = ECC
C     *** 50 approx 16*pi
      ECC = SQRT(AMAX1((SXX-SYY)**2-50.0*SIGSQ*SRR**3/ (XNISO**2)+
     +     4.0*SXY**2,0.0))/SRR
C     *** set initial aperture to be isophotal area
      A = SQRT(SRR* (1.0+ECC))
      B = SQRT(SRR* (1.0-ECC))
      STRECH = SQRT(REC(9)/ (PI*A*B))
C     *** no. of isophotal radii to extend
      SFAC = 2.5/SQRT(ALOG(R))
      SFAC = AMAX1(2.0,SFAC)
      SFAC = AMIN1(3.0,SFAC)
      A = SFAC*A*STRECH
      B = SFAC*B*STRECH
C     *** find max elliptical radius of input points
C     *** and alter a,b if necessary
      IF (NBIT.GT.1) THEN
         RATIO = B/A
         RNEW = 0.0
         DO 30 I = 1,NBIT
            CALL CHECKP(PARM(4,I),PARM(2,I),XCOR,YCOR,RATIO,THETA,RSQ)
            RNEW = AMAX1(RNEW, (SQRT(RSQ)+1.0)/A)
 30      CONTINUE
         IF (RNEW.GT.1.0) THEN
            A = A*RNEW
            B = B*RNEW
         ENDIF
      ENDIF
C     *** clear accumulators
      DO 40 I = 1,NBIT
         BB(I) = 0.d0
         DO 50 J = 1,NBIT
            AA(J,I) = 0.d0
 50      CONTINUE
 40   CONTINUE
C     *** generate images boundaries
      CLIMSQ = (A*CTHETA)**2 + (B*STHETA)**2
      CLIMSQ = AMAX1(1.0,CLIMSQ)
      CLIM = SQRT(CLIMSQ)
      PT1 = SIN(2.0*THETA)* (B**2-A**2)
      PT2 = (B*CTHETA)**2 + (A*STHETA)**2
      PT3 = (A*B)**2
      IRECL = MAX0(ISTART,INT(XCOR-CLIM))
      IRECH = MIN0(ISTOP,INT(XCOR+CLIM+1.0))
      DO 60 II = IRECL,IRECH
         KK = (II-ISTART)*NYOUT
         C = II - XCOR
         PA = CLIMSQ
         PB = PT1*C
         PC = PT2*C**2 - PT3
         ARG1 = PB**2 - 4.0*PA*PC
         ARG1 = SQRT(AMAX1(ARG1,0.0))
         YLIML = (-PB-ARG1)/ (2.0*PA)
         YLIMU = (-PB+ARG1)/ (2.0*PA)
         IYLIML = MAX0(IXL,INT(YCOR+YLIML))
         IYLIMU = MIN0(IXH,INT(YCOR+YLIMU+1.0))
         DO 70 I = IYLIML,IYLIMU
            T = MAP(KK+I-IXL+1) - XPEAK
            IF (T.LT.FUPP) THEN
               DO 80 J = 1,NBIT
                  XJ = II - PARM(4,J)
                  YJ = I - PARM(2,J)
C     *** replace FUNC directly between here
                  RADSQ = XJ*XJ + YJ*YJ
                  RAD = SQRT(RADSQ)
                  IF (RAD.LT.PARRAD) THEN
                     ARG = C1*RADSQ
                     TJ = CN*Q/ (1.0+PARMN2*RADSQ)
                     IF (ARG.GT.-16.0) TJ = TJ +
     +                    (1.0-Q)*CN*EXP(ARG)
                  ELSE
                     ARG = CHANGE + (PARRAD-RAD)*C2
                     TJ = CN*Q/ (1.0+PARMN2*RADSQ)
                     IF (ARG.GT.-16.0) TJ = TJ +
     +                    (1.0-Q)*CN*EXP(ARG)
                  ENDIF
C     *** and here
                  BB(J) = BB(J) + TJ*T
                  DO 90 K = J,NBIT
                     IF (CC(K,J).NE.0) THEN
                        IF (K.EQ.J) THEN
                           TK = TJ

                        ELSE

                           XK = II - PARM(4,K)
                           YK = I - PARM(2,K)
C     *** replaced FUNC directly between here
                           RADSQ = XK*XK + YK*YK
                           RAD = SQRT(RADSQ)
                           IF (RAD.LT.PARRAD) THEN
                              ARG = C1*RADSQ
                              TK = CN*Q/ (1.0+PARMN2*RADSQ)
                              IF (ARG.GT.-16.0) TK = TK +
     +                             (1.0-Q)*CN*EXP(ARG)

                           ELSE

                              ARG = CHANGE + (PARRAD-RAD)*C2
                              TK = CN*Q/ (1.0+PARMN2*RADSQ)
                              IF (ARG.GT.-16.0) TK = TK +
     +                             (1.0-Q)*CN*EXP(ARG)
                           ENDIF
C     *** and here
                        ENDIF

                        AA(K,J) = AA(K,J) + TK*TJ
                     ENDIF
 90               CONTINUE
 80            CONTINUE
            ENDIF

 70      CONTINUE
 60   CONTINUE
C     *** pad out rest of matrix
      IF (NBIT.GT.1) THEN
         DO 100 J = 2,NBIT
            JU = J - 1
            DO 110 K = 1,JU
               AA(K,J) = AA(J,K)
 110        CONTINUE
 100     CONTINUE
      ENDIF
C     *** solve for profile intensities
      CALL DCHOLE4(AA,BB,NBIT)
      DO 120 I = 1,NBIT
         PARM(1,I) = BB(I)
 120  CONTINUE

      END

