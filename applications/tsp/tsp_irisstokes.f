
      SUBROUTINE TSP_IRISSTOKES2(NX,NY,I1,I2,X1,Y1,WIDTH,HEIGHT,
     :   XSEP,YSEP,INTEN,STOKES,IX,OX,IY,OY)
*+
*
*   Reduce IRIS spectropolarimetry using the Scaling method
*
*   (>)  NX      (Integer)  First dimension of input arrays
*   (>)  NY      (Integer)  Second dimension of input arrays
*   (>)  I1      (Real array(NX,NY) First input array
*   (>)  I2      (Real array(NX,NY) Second input array
*   (>)  X1      (Integer)  Start of block in X
*   (>)  Y1      (Integer)  Start of block in Y
*   (>)  WIDTH   (Integer)  Width of each spectrum
*   (>)  HEIGHT  (Integer)  Height of each spectrum
*   (>)  XSEP    (Real)  OE separation in X
*   (>)  YSEP    (Real)  OE separation in Y
*   (<)  INTEN   (Real array(WIDTH,HEIGHT))  Output intensity
*   (<)  STOKES  (Real array(WIDTH,HEIGHT))  Output Stokes data
*   (>)  IX      (Real array(NX))  Input X axis array
*   (<)  OX      (Real array(WIDTH))  Output X axis array
*   (>)  IY      (Real array(NY))  Input Y axis array
*   (<)  OY      (Real array(HEIGHT))  Output Y axis array
*
*
*   Jeremy Bailey   3/5/1993
*
*   Modified:
*     1997-May-2, JAB
*        Fix bug in scaling method scale O2 rather than E1
*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,X1,Y1,WIDTH,HEIGHT
      REAL XSEP,YSEP
      REAL I1(NX,NY),I2(NX,NY)
      REAL INTEN(WIDTH,HEIGHT),STOKES(WIDTH,HEIGHT)
      REAL IX(NX),OX(WIDTH),IY(NY),OY(HEIGHT)

*  Local variables
      INTEGER I,J
      REAL RAT,E1,E2,O1,O2
      INTEGER JX,JY
      REAL RX,RY,T,U

*  Copy X axis to output
      DO I=1,WIDTH
         OX(I)=IX(I+X1-1)
      ENDDO

*  Copy Y axis to output
      DO J=1,HEIGHT
         OY(J)=IY(J+Y1-1)
      ENDDO


      DO I=1,WIDTH
        DO J=1,HEIGHT
          E1 = I1(X1+I-1,Y1+J-1)
          E2 = I2(X1+I-1,Y1+J-1)
          RX = REAL(X1+I-1)+XSEP
          RY = REAL(Y1+J-1)+YSEP
          JX = INT(RX)
          JY = INT(RY)
          T = RX - REAL(JX)
          U = RY - REAL(JY)
          O1 = (1.0-T)*(1.0-U)*I1(JX,JY) +
     :          T*(1.0-U)*I1(JX+1,JY) + T*U*I1(JX+1,JY+1)
     :           + (1.0-T)*U*I1(JX,JY+1)
          O2 = (1.0-T)*(1.0-U)*I2(JX,JY) +
     :          T*(1.0-U)*I2(JX+1,JY) + T*U*I2(JX+1,JY+1)
     :           + (1.0-T)*U*I2(JX,JY+1)
*  Determine ratio of E and O data
          IF ((O1+O2) .NE. 0.0) THEN
             RAT = (E1+E2)/(O1+O2)
          ELSE
             RAT = 1.0
          ENDIF

*  Scale up the O data to match that of the E data
          O1 = O1*RAT
          O2 = O2*RAT


*  Output intensity
          INTEN(I,J) = O1+O2+E1+E2

*  Determine Stokes parameter
          STOKES(I,J) = O1-E1-O2+E2
        ENDDO

      ENDDO
      END


      SUBROUTINE TSP_IRISSTOKES(NX,NY,I1,I2,X1,Y1,WIDTH,HEIGHT,
     :   XSEP,YSEP,INTEN,STOKES,IX,OX,IY,OY)
*+
*
*   Reduce IRIS spectropolarimetry using the RATIO method
*
*   (>)  NX      (Integer)  First dimension of input arrays
*   (>)  NY      (Integer)  Second dimension of input arrays
*   (>)  I1      (Real array(NX,NY) First input array
*   (>)  I2      (Real array(NX,NY) Second input array
*   (>)  X1      (Integer)  Start of block in X
*   (>)  Y1      (Integer)  Start of block in Y
*   (>)  WIDTH   (Integer)  Width of each spectrum
*   (>)  HEIGHT  (Integer)  Height of each spectrum
*   (>)  XSEP    (Real)     OE separation in X
*   (>)  YSEP    (Real)     OE separation in Y
*   (<)  INTEN   (Real array(WIDTH,HEIGHT))  Output intensity
*   (<)  STOKES  (Real array(WIDTH,HEIGHT))  Output Stokes data
*   (>)  IX      (Real array(NX))  Input X axis array
*   (<)  OX      (Real array(WIDTH))  Output X axis array
*   (>)  IY      (Real array(NY))  Input Y axis array
*   (<)  OY      (Real array(HEIGHT))  Output Y axis array
*
*
*   Jeremy Bailey   3/5/1993
*
*   Modified:
*+


*+

      IMPLICIT NONE

      INCLUDE 'PRM_PAR'

*  Parameters
      INTEGER NX,NY,X1,Y1,WIDTH,HEIGHT
      REAL XSEP,YSEP
      REAL I1(NX,NY),I2(NX,NY)
      REAL INTEN(WIDTH,HEIGHT),STOKES(WIDTH,HEIGHT)
      REAL IX(NX),OX(WIDTH),IY(NY),OY(HEIGHT)

*  Local variables
      INTEGER I,J
      REAL RAT,E1,E2,O1,O2
      REAL RX,RY,T,U
      INTEGER JX,JY
      INTEGER STATUS

      STATUS=0

*  Copy X axis to output
      DO I=1,WIDTH
         OX(I)=IX(I+X1-1)
      ENDDO

*  Copy Y axis to output
      DO J=1,HEIGHT
         OY(J)=IY(J+Y1-1)
      ENDDO

      DO I=1,WIDTH
        DO J=1,HEIGHT
          E1 = I1(X1+I-1,Y1+J-1)
          E2 = I2(X1+I-1,Y1+J-1)
          RX = REAL(X1+I-1)+XSEP
          RY = REAL(Y1+J-1)+YSEP
          JX = INT(RX)
          JY = INT(RY)
          T = RX - REAL(JX)
          U = RY - REAL(JY)
          O1 = (1.0-T)*(1.0-U)*I1(JX,JY) +
     :          T*(1.0-U)*I1(JX+1,JY) + T*U*I1(JX+1,JY+1)
     :           + (1.0-T)*U*I1(JX,JY+1)
          O2 = (1.0-T)*(1.0-U)*I2(JX,JY) +
     :          T*(1.0-U)*I2(JX+1,JY) + T*U*I2(JX+1,JY+1)
     :           + (1.0-T)*U*I2(JX,JY+1)
*   Output intensity
          INTEN(I,J) = E1+E2+O1+O2

*   Determine ratio
          RAT = O1/E1
          RAT = RAT/(O2/E2)

*   Check that it is positive and we can take square root of it
          IF (RAT .LE. 0.0) THEN
              CALL MSG_SETI('CHAN1',I+X1-1)
              CALL MSG_SETI('CHAN2',J+Y1-1)
              CALL MSG_OUT(' ',
     :             'Negative square root in pixel ^CHAN1 ^CHAN2',
     :             STATUS)
              RAT = 0.0
          ELSE
              RAT = SQRT(RAT)
          ENDIF

*  Determine output Stokes parameter
          STOKES(I,J) = INTEN(I,J)*(RAT-1)/(RAT+1)

        ENDDO
      ENDDO
      END

