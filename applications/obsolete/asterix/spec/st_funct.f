*
*-------------------------------------------------------------------------------
*
        REAL*8 FUNCTION ST_FUNCT(FA1,FA2,X)
*
* Calculates that horrible function for the S-T Comptonization spectrum
*
        IMPLICIT NONE
*
        INTEGER I
        REAL*8 DI,FA1,FA2,FUNCT,SUM,SUMSAV,X
*
        FUNCT=1.0D0
        SUM=1.0D0
*
        DO I=1,10000
          DI=DFLOAT(I)
          FUNCT=FUNCT*X*(FA1+DI-1.0D0)/(DI*(FA2+DI-1.0D0))
          SUMSAV=SUM
          SUM=SUM+FUNCT
          IF (DABS(SUM-SUMSAV) .LT. 1.0D-8) GO TO 10
        END DO
*
 10     ST_FUNCT=SUM
*
        END
