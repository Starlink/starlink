C--------------------------------------------------------------

      SUBROUTINE SWAP2(X,Y)

C  Routine to swap two real*4 variables X and Y

      REAL*4 X,Y,TEMP

      TEMP=X
      X=Y
      Y=TEMP

      RETURN
      END


