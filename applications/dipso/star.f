      SUBROUTINE STAR(XC,YC,N,ANGLE,EXPAND,XYRATIO,XS,YS,NS)
      REAL XS(NS), YS(NS)
      PARAMETER (PI=3.14159265)

      DTHETA = 2 * PI / N
      THETA = (3*PI + DTHETA)/2 + ANGLE
      FRACT=0.01
      XA = EXPAND*FRACT*COS(THETA) + XC
      YA = EXPAND*FRACT*SIN(THETA)*XYRATIO + YC
      NS=1
      XS(NS)=XA
      YS(NS)=YA
      STELLAR = .25
      DO  J = 1,N
         THETA = THETA + DTHETA
         XB = EXPAND*FRACT*COS(THETA) + XC
         YB = EXPAND*FRACT*SIN(THETA)* XYRATIO + YC
         XMID = STELLAR*(XA + XB - 2*XC) + XC
         YMID = STELLAR*(YA + YB - 2*YC)*XYRATIO + YC
         NS=NS+1
         XS(NS)=XMID
         YS(NS)=YMID
         NS=NS+1
         XS(NS)=XB
         YS(NS)=YB
         XA = XB
         YA = YB
      ENDDO
      END
