      SUBROUTINE EXTEND4(MAP,PARM,XPEAK,R,SIGSQ,ICIRC,RCIRC)

C     EXTEND  does aperture integration a la Kron except using
C     matched ellipses.

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK. INTEGER*2 references changed to
C        INTEGER*4.

C     .. Scalar Arguments ..
      REAL R,RCIRC,SIGSQ,XPEAK
      INTEGER ICIRC
C     ..
C     .. Array Arguments ..
      REAL*4 PARM(16)
      INTEGER*4 MAP(*)
C     ..
C     .. Scalars in Common ..
      INTEGER ISTART,ISTOP,IXH,IXL,NWORD
C     ..
C     .. Local Scalars ..
      REAL A,ARG,ARG1,B,C,CLIM,CLIMSQ,CTHETA,ECC,ECCOLD,ELLRAD,PA,PB,PC,
     +     PI,PIO2,PT1,PT2,PT3,RADEG,RT1,RT2,SFAC,SRR,STHETA,STRECH,SXX,
     +     SXY,SYY,T,T1,TEMP,THETA,X,XCOR,XLIM1,XLIM2,XLIMIT,XMAX,XNEW,
     +     XNISO,XX,Y,YCOR,YLIML,YLIMU,YNEW
      INTEGER I,II,IR,IRECH,IRECL,IUPD,IYLIML,IYLIMU,J,KK,LT,NYOUT
C     ..
C     .. Local Arrays ..
      REAL*4 ACCUM(10),POLYCF(4),XDAT(10),XXXX(10)
C     ..
C     .. Common blocks ..
      COMMON /FIL/ISTART,ISTOP,NWORD,IXL,IXH
C     ..
      PI = 4.0*ATAN(1.0)
      PIO2 = PI/2.0
      RADEG = 180.0/PI
      LT = 6
      NYOUT = IXH - IXL + 1
C     *** get image parms
      XNISO = PARM(1)
      XCOR = PARM(4)
      YCOR = PARM(2)
      SXX = PARM(5)
      SXY = PARM(6)
      SYY = PARM(7)
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
      ECC = AMIN1(0.9,ECC)
C     *** set initial aperture to be isophotal area
      A = SQRT(SRR* (1.0+ECC))
      B = SQRT(SRR* (1.0-ECC))
      STRECH = SQRT(PARM(9)/ (PI*A*B))
C     *** no. of isophotal radii to extend
      SFAC = 3.0/SQRT(ALOG(R))
      SFAC = AMAX1(2.0,SFAC)
      SFAC = AMIN1(5.0,SFAC)
      A = SFAC*A*STRECH
      B = SFAC*B*STRECH
      IF (ICIRC.EQ.1) THEN
         A = RCIRC
         B = RCIRC
      ENDIF
C     *** clear accumulators
      TEMP = RADEG*THETA
      DO 10 I = 1,10
         ACCUM(I) = 0.0
 10   CONTINUE
C     *** generate images boundaries
      CLIMSQ = (A*CTHETA)**2 + (B*STHETA)**2
      CLIMSQ = AMAX1(1.0,CLIMSQ)
      CLIM = SQRT(CLIMSQ)
      PT1 = SIN(2.0*THETA)* (B**2-A**2)
      PT2 = (B*CTHETA)**2 + (A*STHETA)**2
      PT3 = (A*B)**2
      IRECL = MAX0(ISTART,INT(XCOR-CLIM))
      IRECH = MIN0(ISTOP,INT(XCOR+CLIM+1.0))
      DO 20 II = IRECL,IRECH
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
         X = C
         DO 30 I = IYLIML,IYLIMU
            T = MAP(KK+I-IXL+1) - XPEAK
            Y = I - YCOR
C     *** find ellipse radius of pt
            XNEW = X*CTHETA + Y*STHETA
            YNEW = -X*STHETA + Y*CTHETA
            ELLRAD = 2.0*SQRT((XNEW/A)**2+ (YNEW/B)**2)
            IUPD = INT((2.0-ELLRAD)*10.0) + 1
            IUPD = MAX0(1,IUPD)
            IUPD = MIN0(10,IUPD)
            DO 40 J = 1,IUPD
               ACCUM(11-J) = ACCUM(11-J) + T
 40         CONTINUE
 30      CONTINUE
 20   CONTINUE
C     *** now find limiting intensity
      IF (ICIRC.EQ.1) THEN
         PARM(1) = ACCUM(10)

      ELSE

         IF (PARM(1).LT.0.0) THEN
            DO 50 I = 1,10
               ACCUM(I) = -ACCUM(I)
 50         CONTINUE
         ENDIF
         CALL MEDIAN4(ACCUM,10,3)
         XMAX = 0.0
         XLIM1 = -1.0
         XLIM2 = -1.0
         DO 60 I = 1,10
            XXXX(I) = I
            XMAX = AMAX1(XMAX,ACCUM(I))
            XDAT(I) = ACCUM(I)
 60      CONTINUE
         CALL POLYNM(XDAT,XXXX,10,POLYCF,4,0)
         PA = POLYCF(2)
         PB = POLYCF(3)*2.0
         PC = POLYCF(4)*3.0
         ARG = SQRT(AMAX1(0.0,PB**2-4.0*PA*PC))
         IF (PC.NE.0.0) THEN
            RT1 = (-PB+ARG)/ (2.0*PC)
            RT2 = (-PB-ARG)/ (2.0*PC)
            IF (RT1.LT.10.0 .AND. RT1.GT.1.0) THEN
               IR = INT(RT1)
               T1 = RT1 - IR
               XLIM1 = (1.0-T1)*ACCUM(IR) + T1*ACCUM(IR+1)
            ENDIF

            IF (RT2.LT.10.0 .AND. RT2.GT.1.0) THEN
               IR = INT(RT2)
               T1 = RT2 - IR
               XLIM2 = (1.0-T1)*ACCUM(IR) + T1*ACCUM(IR+1)
            ENDIF

         ENDIF

         XLIMIT = AMAX1(XLIM1,XLIM2)
         IF (XLIMIT.LT.0.0) XLIMIT = XMAX
C     *** update buffers
         IF (PARM(1).LT.0.0) XLIMIT = -XLIMIT
         PARM(1) = XLIMIT
      ENDIF
      END

