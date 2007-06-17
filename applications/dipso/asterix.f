      SUBROUTINE ASTERIX(XC,YC,N,ANGLE,EXPAND,XYRATIO,XS,YS,NS)
      REAL XS(NS), YS(NS)
      PARAMETER (PI=3.14159265)

      DTHETA = 2 * PI / N
      THETA = (3*PI + DTHETA)/2 + ANGLE
      FRACT=0.01
      XA = EXPAND*FRACT*COS(THETA) + XC
      YA = EXPAND*FRACT*SIN(THETA)*XYRATIO + YC
      NS=0
      DO  J = 1,N
         NS=NS+1
         XS(NS)=XA
         YS(NS)=YA
         NS=NS+1
         XS(NS)=XC
         YS(NS)=YC
         THETA = THETA + DTHETA
         XB = EXPAND*FRACT*COS(THETA) + XC
         YB = EXPAND*FRACT*SIN(THETA)*XYRATIO + YC
         XA = XB
         YA = YB
      ENDDO
      END
