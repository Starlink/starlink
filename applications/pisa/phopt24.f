      SUBROUTINE PHOPT24(IMS,JCOUNT)
C     *** PHOPT2  does multiple profile fitting to determine intensities
C     ***         using only pixels within isophote

C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.

C     ..Parameters..
      INCLUDE 'PSA1_PAR'        ! PISA parameters

C     .. Scalar Arguments ..
      INTEGER IMS,JCOUNT
C     ..
C     .. Scalars in Common ..
      REAL C1,C2,CHANGE,CN,CONST,OFFSET,PARMN1,PARMN2,PARMNN,PARRAD,
     +     PARSQ,Q,SKYCOR,THRESH
      INTEGER IANAL,IB,IMODEL,IPIX,ISTART,ISTOP,IUPP,IXH,IXL,NBIT,NWORD
C     ..
C     .. Arrays in Common ..
      REAL*4 PARM(16,IMNUM)
      INTEGER*4 ILIST(IMLIM),JLIST(IMLIM),KLIST(IMLIM)
C     ..
C     .. Local Scalars ..
      REAL ARG,RAD,RADSQ,T,TJ,TK,XI,XJ,XK,YI,YJ,YK
      INTEGER I,II,J,JH,JJL,JL,JU,K,LT,NN,NPARM
C     ..
C     .. Local Arrays ..
      REAL*8 AA(IMNUM+1,IMNUM+1),BB(IMNUM+1)
      BYTE CC(IMNUM,IMNUM)
C     ..
C     .. External Subroutines ..
      EXTERNAL DCHOLE4
C     ..
C     .. Intrinsic Functions ..
      INTRINSIC EXP,SQRT
C     ..
C     .. Common blocks ..
      COMMON /FIL/ISTART,ISTOP,NWORD,IXL,IXH
      COMMON /OV/ILIST,JLIST,KLIST,THRESH,IPIX,PARM,NBIT,CONST,OFFSET,
     +     IANAL
      COMMON /PM/C1,C2,CN,PARSQ,CHANGE,PARRAD,IUPP,SKYCOR,IB
      COMMON /PMN/PARMN1,PARMN2,PARMNN,Q,IMODEL
C     ..
      LT = 6
      JL = IMS
      JH = IMS + JCOUNT - 1
      NPARM = NBIT + 1
C     *** set up flag array matrix
      CC(1,1) = 1
      IF (NBIT.GT.1) THEN
         DO 10 I = 1,NBIT
            XI = PARM(4,I)
            YI = PARM(2,I)
            JJL = I + 1
            CC(I,I) = 1
            DO 20 J = JJL,NBIT
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
C     *** clear accumulators
      DO 30 I = 1,NPARM
         BB(I) = 0.d0
         DO 40 J = 1,NPARM
            AA(J,I) = 0.d0
 40      CONTINUE
 30   CONTINUE
C     *** main pixel loop
      DO 50 NN = JL,JH
         IF (KLIST(NN).LT.IUPP) THEN
            II = ILIST(NN)
            I = JLIST(NN)
            T = KLIST(NN)
            AA(NPARM,NPARM) = AA(NPARM,NPARM) + 1.0
            BB(NPARM) = BB(NPARM) + T
            DO 60 J = 1,NBIT
               XJ = II - PARM(4,J)
               YJ = I - PARM(2,J)
C     *** replaced FUNC between here
               RADSQ = XJ*XJ + YJ*YJ
               RAD = SQRT(RADSQ)
               IF (RAD.LT.PARRAD) THEN
                  ARG = C1*RADSQ
                  TJ = CN*Q/ (1.0+PARMN2*RADSQ)
                  IF (ARG.GT.-16.0) TJ = TJ + (1.0-Q)*CN*EXP(ARG)

               ELSE

                  ARG = CHANGE + (PARRAD-RAD)*C2
                  TJ = CN*Q/ (1.0+PARMN2*RADSQ)
                  IF (ARG.GT.-16.0) TJ = TJ + (1.0-Q)*CN*EXP(ARG)
               ENDIF
C     *** and here
               BB(J) = BB(J) + TJ*T
               AA(J,NPARM) = AA(J,NPARM) + TJ
               DO 70 K = J,NBIT
                  IF (CC(K,J).NE.0) THEN
                     IF (K.EQ.J) THEN
                        TK = TJ

                     ELSE

                        XK = II - PARM(4,K)
                        YK = I - PARM(2,K)
C     *** replace FUNC between here
                        RADSQ = XK*XK + YK*YK
                        RAD = SQRT(RADSQ)
                        IF (RAD.LT.PARRAD) THEN
                           ARG = C1*RADSQ
                           TK = CN*Q/ (1.0+PARMN2*RADSQ)
                           IF (ARG.GT.-16.0) TK = TK +
     +                          (1.0-Q)*CN*EXP(ARG)

                        ELSE

                           ARG = CHANGE + (PARRAD-RAD)*C2
                           TK = CN*Q/ (1.0+PARMN2*RADSQ)
                           IF (ARG.GT.-16.0) TK = TK +
     +                          (1.0-Q)*CN*EXP(ARG)
                        ENDIF
C     *** and here
                     ENDIF

                     AA(K,J) = AA(K,J) + TK*TJ
                  ENDIF

 70            CONTINUE
 60         CONTINUE
         ENDIF

 50   CONTINUE
C     *** pad out rest of matrix
      IF (NBIT.GT.1) THEN
         DO 80 J = 2,NBIT
            JU = J - 1
            DO 90 K = 1,JU
               AA(K,J) = AA(J,K)
 90         CONTINUE
 80      CONTINUE
      ENDIF

      DO 100 J = 1,NBIT
         AA(NPARM,J) = AA(J,NPARM)
 100  CONTINUE
C     *** solve for profile intensities
      IF (IB.EQ.1) THEN
         CALL DCHOLE4(AA,BB,NPARM)
         SKYCOR = BB(NPARM)

      ELSE

         CALL DCHOLE4(AA,BB,NBIT)
         SKYCOR = 0.0
      ENDIF

      DO 110 I = 1,NBIT
         PARM(1,I) = BB(I)
 110  CONTINUE

      END

