*  History:
*     22 Nov 1993 (hme):
*        Remove TABs.
C------------------------------------------------------------------------------

      SUBROUTINE A13(DATJUL,A1,A2,A3)

C   CALCULATES BASIC EARTH ORBIT PARAMETERS
C
C     VARIABLES ARE:
C     T   : BESSELIAN CENTURIES
C     A1  : GEOMETRIC LONGITUDE OF SUN (L)
C     A2  : MEAN LONGITUDE OF PERIGEE (GAMMA)
C     A3  : MEAN ANOMALY (L-GAMMA) I.E., THETA OF ORBIT
C
C     RED IS A ROUTINE TO REDUCE ANGLES TO .LT. 360 DEG.
C
      REAL*8  DATJUL,CSAR,T,TSQ,TCB,B1,B2,B3,RED

      DATA CSAR/4.8481368111D-06/

      T=DATJUL/36525.0D0
      B1=((1.089D0*T+129602768.13D0)*T+1006908.04D0)*CSAR
      B2=(((0.012D0*T+1.63D0)*T+6189.03D0)*T+1012395.00D0)*CSAR
      B3=(((-.012D0*T-0.54D0)*T+129596579.1D0)*T+1290513.04D0)*CSAR
      A1=RED(B1)
      A2=RED(B2)
      A3=RED(B3)

      RETURN
      END

C------------------------------------------------------------------------------
