      SUBROUTINE INIT3D (EYE,NU,NV,NW,ST1,LX,NY,IS2,IU,S)
      DIMENSION       EYE(3)     ,ST1(NV,NW,2)           ,IS2(LX,NY) ,
     1                S(4)
C
        SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
      CALL Q8QST4('GRAPHX','ISOSRFHR','INIT3D','VERSION  1')
C
C SET UP TRANSFORMATION ROUTINE FOR THIS LINE OF SIGHT.
C
      GO TO 101
C
C     ENTRY PERSPC(X,Y,Z,XT,YT,ZT)
C
      ENTRY PERSPC
      GO TO 122
  101 U = NU
      V = NV
      W = NW
      AX = U*.5
      AY = V*.5
      AZ = W*.5
      EX = EYE(1)
      EY = EYE(2)
      EZ = EYE(3)
      DX = AX-EX
      DY = AY-EY
      DZ = AZ-EZ
      D = SQRT(DX*DX+DY*DY+DZ*DZ)
      COSAL = DX/D
      COSBE = DY/D
      COSGA = DZ/D
      AL = ACOS(COSAL)
      BE = ACOS(COSBE)
      GA = ACOS(COSGA)
      SINGA = SIN(GA)
C
C TEST FOR CLOSE TO VERTICAL
C
      IF (SINGA .LT. 0.0001) GO TO 102
      R = 1./SINGA
      JUMP = 1
      GO TO 103
  102 SINBE = SIN(BE)
      R = 1./SINBE
      JUMP = 2
  103 X = 1.
      Y = 1.
      Z = W
      ASSIGN 104 TO IBAK
      GO TO 123
  104 YTS = YT
      X = U
      Y = V
      Z = 1.
      ASSIGN 105 TO IBAK
      GO TO 123
  105 YB = YT
      Y = 1.
      ASSIGN 106 TO IBAK
      GO TO 123
  106 XL = XT
      X = 1.
      Y = V
      ASSIGN 107 TO IBAK
      GO TO 123
  107 XR = XT
C
C MAKE SCREEN SQUARE
C
      DIF = (XR-XL-YTS+YB)*.5
      IF (DIF) 108,110,109
  108 XL = XL+DIF
      XR = XR-DIF
      GO TO 110
  109 YB = YB-DIF
      YTS = YTS+DIF
  110 REWIND IU
      C1 = .9*(S(2)-S(1))/(XR-XL)
      C2 = .05*(S(2)-S(1))+S(1)
      C3 = .9*(S(4)-S(3))/(YTS-YB)
      C4 = .05*(S(4)-S(3))+S(3)
      COSALR = COSAL*R*C1
      QF1 = (EX*COSAL+EY*COSBE+EZ*COSGA)/D
      RC3 = R*C3
      COSALD = COSAL/D
      COSBED = COSBE/D
      COSGAD = COSGA/D
C
C TRANSFORM GRID POINTS INTO 2-SPACE
C
      GO TO (111,115),JUMP
  111 COSBER = COSBE*R*C1
      X1F1 = (EX-AX)*COSBER+(AY-EY)*COSALR-C1*XL+C2
      X1F2 = EY*COSALR-EX*COSBER
      Y1F1 = (EZ-AZ)*RC3-C3*YB+C4
      DO 114 I=1,NU
         X = NU+1-I
         QI1 = X*COSALD-QF1
         XF1 = X*COSBER+X1F2
         DO 113 J=1,NV
            Y = J
            QI2 = Y*COSBED+QI1
            YF1 = XF1-Y*COSALR
            DO 112 K=1,NW
               Z = K
               QI3 = Z*COSGAD+QI2
               ST1(J,K,1) = X1F1+YF1/QI3
               ST1(J,K,2) = Y1F1+RC3*(Z-EZ)/QI3
  112       CONTINUE
  113    CONTINUE
         WRITE (IU) ST1
  114 CONTINUE
      GO TO 119
  115 COSGAR = COSGA*R*C1
      X2F1 = (EZ-AZ)*COSALR+(AX-EX)*COSGAR-C1*XL+C2
      X2F2 = EX*COSGAR-EZ*COSALR
      Y2F1 = (EY-AY)*RC3-C3*YB+C4
      DO 118 I=1,NU
         X = NU+1-I
         QI1 = X*COSALD-QF1
         XF1 = X2F2-X*COSGAR
         DO 117 J=1,NV
            Y = J
            QI2 = Y*COSBED+QI1
            YF1 = (Y-EY)*RC3
            DO 116 K=1,NW
               Z = K
               QI3 = Z*COSGAD+QI2
               ST1(J,K,1) = X2F1+(XF1+Z*COSALR)/QI3
               ST1(J,K,2) = Y2F1+YF1/QI3
  116       CONTINUE
  117    CONTINUE
         WRITE (IU) ST1
  118 CONTINUE
  119 REWIND IU
C
C ZERO OUT SCREEN MODEL
C
      DO 121 J=1,NY
         DO 120 I=1,LX
            IS2(I,J) = 0
  120    CONTINUE
  121 CONTINUE
      RETURN
C
C RETAIN OLD CODE FOR TRANSFORMING OUTER POINTS
C
  122 ASSIGN 127 TO IBAK
  123 Q = D/((X-EX)*COSAL+(Y-EY)*COSBE+(Z-EZ)*COSGA)
      GO TO (124,125),JUMP
  124 XT = ((EX+Q*(X-EX)-AX)*COSBE-(EY+Q*(Y-EY)-AY)*COSAL)*R
      YT = (EZ+Q*(Z-EZ)-AZ)*R
      GO TO 126
  125 XT = ((EZ+Q*(Z-EZ)-AZ)*COSAL-(EX+Q*(X-EX)-AX)*COSGA)*R
      YT = (EY+Q*(Y-EY)-AY)*R
  126 GO TO IBAK,(104,105,106,107,127)
  127 RETURN
      END
