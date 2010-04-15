	SUBROUTINE MAKEMASKSUB( DIMSX, DIMSY, ARRIN, ARROUT, IXST,
     :	                        IYST, IXEN, IYEN, SIGMALEVEL,
     :	                        MEAN_VALUE, SIGMA, NUMPIX, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  IXST,
     :	  IYST,
     :	  IXEN,
     :	  IYEN,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  NUMPIX,
     :	  BAD_NUMBER

	REAL
     :	  ARRIN( DIMSX, DIMSY),
     :	  ARROUT( DIMSX, DIMSY),
     :	  SIGMALEVEL,
     :	  SIGMA,
     :	  MEAN_VALUE,
     :	  ONE_SIGMA,
     :	  VARIANCE,
     :	  SUM,
     :	  SUMSQ,
     :	  XMAXIMUM

*      initialize the number of bad pixels found
	NUMPIX = 0
	VARIANCE = 0.0

*      define limits of maximum, minimum values
	XMAXIMUM = -1.0E20
	MEAN_VALUE = 0.0
	SUM = 0.0
	SUMSQ = 0.0
	ONE_SIGMA = 1.0
	BAD_NUMBER = 0

*      calculate mean value in sub-image
	DO J = IYST, IYEN
	  DO K = IXST, IXEN

	    IF( ABS( ARRIN( K, J)) .GT. 1.0E30) THEN
	      BAD_NUMBER = BAD_NUMBER + 1
	    ELSE
	      NUMPIX = NUMPIX + 1
	      SUM = SUM + ARRIN( K, J)
	      SUMSQ = SUMSQ + ARRIN( K, J)**2
	    END IF

	  END DO
	END DO

*      calculate maximum and minimum from sigma level
	IF( NUMPIX .GT. 0) THEN
	  MEAN_VALUE = SUM/NUMPIX
          VARIANCE = ( SUMSQ - 2*MEAN_VALUE*SUM +
     :	               NUMPIX*MEAN_VALUE**2)

          IF( NUMPIX .EQ. 1 ) THEN
            VARIANCE = VARIANCE/REAL( NUMPIX)
          ELSE
            VARIANCE = VARIANCE/( REAL( NUMPIX) - 1.0)
          END IF

	END IF

        ONE_SIGMA =  SQRT( ABS( VARIANCE))

	SIGMA = SIGMALEVEL*ONE_SIGMA

	XMAXIMUM = MEAN_VALUE + SIGMA

	NUMPIX = 0

*      scan through all the input pixels
	DO J = 1, DIMSY
	  DO K = 1, DIMSX

	    IF( ARRIN( K, J) .GT. XMAXIMUM) THEN
	      ARROUT( K, J) = 1
	    ELSE
	      NUMPIX = NUMPIX + 1
	      ARROUT( K, J) = 0
	    END IF

	  END DO
	END DO

	END
