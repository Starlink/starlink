*+HRI_SENS         Gives estimated sensitivity for given exposure
      REAL FUNCTION HRI_SENS(SIGMA, BCELL,TSEC)
      IMPLICIT NONE
 
*  Calling Arguments
      REAL SIGMA
      REAL BCELL
      REAL TSEC			! In	Exposure, sec

*  Local variables
      REAL SS, SS2

* _________________________ Executable Code ___________________________

      SS = SIGMA ** 2
      SS2 = SS / 2.
      HRI_SENS = (SS2 + SQRT(SS2**2 + SS * BCELL * TSEC) ) / TSEC

      END
