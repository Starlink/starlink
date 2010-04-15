      SUBROUTINE CHECKP(X,Y,X0,Y0,RATIO,THETA,RADSQ)
C     CHECKP  calculates equivalent radius (along semi-major axis)
C     ellipse Ratio is b/a

C  Changes:
C     17-JUN-1995: P.W.Draper
C        Code transformed by TOOLPACK.

C     .. Scalar Arguments ..
      REAL RADSQ,RATIO,THETA,X,X0,Y,Y0
C     ..
C     .. Local Scalars ..
      REAL XNEW,XX,YNEW,YY
C     ..

      XX = X - X0
      YY = Y - Y0
      XNEW = XX*COS(THETA) + YY*SIN(THETA)
      YNEW = -XX*SIN(THETA) + YY*COS(THETA)
      RADSQ = XNEW**2 + (YNEW/RATIO)**2

      END

