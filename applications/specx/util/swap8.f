C--------------------------------------------------------------

      SUBROUTINE SWAP8 (X,Y)

C  Routine to swap two real*8 variables X and Y

      REAL*8 X, Y, TEMP

      TEMP = X
      X    = Y
      Y    = TEMP

      RETURN
      END


