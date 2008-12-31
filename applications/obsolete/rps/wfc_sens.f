*+WFC_SENS         Gives estimated sensitivity for given exposure
      REAL FUNCTION WFC_SENS(SIGMA, BCELL,TSEC,F,NMIN)
      IMPLICIT NONE
 
*  Calling Arguments
      REAL SIGMA
      REAL BCELL
      REAL TSEC			! In	Exposure, sec
      REAL F
      REAL NMIN

* _________________________ Executable Code ___________________________

      WFC_SENS = SQRT ( ( (SIGMA/F)**2 * BCELL + NMIN**2/TSEC ) / TSEC )

      END
