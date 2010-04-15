	SUBROUTINE MAKEBADSUB( DIMSX, DIMSY, ARRIN, SIGMA, OUTFILE, STATUS)

	IMPLICIT NONE

*     HISTORY
*     14-JUL-1994  Changed LIB$ to FIO_ (SKL@JACH)
*      9-AUG-2004  Use FIO for open and close (TIMJ@JACH)

        INCLUDE 'FIO_PAR'
	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  STATUS,
     :	  LUN,
     :	  J,
     :	  K,
     :	  NUMPIX,
     :	  BAD_NUMBER
	REAL
     :	  ARRIN( DIMSX, DIMSY),
     :	  SIGMA,
     :	  MEAN_VALUE,
     :	  ONE_SIGMA,
     :	  VARIANCE,
     :	  SUM,
     :	  SUMSQ,
     :	  XMAXIMUM,
     :	  XMINIMUM

	CHARACTER*( *)
     :	  OUTFILE

*      initialize the number of bad pixels found
	NUMPIX = 0
	VARIANCE = 0.0

*      Open output file
	CALL FIO_OPEN( OUTFILE, 'WRITE','LIST',0, LUN, STATUS)

*      write header line to bad pixel file
	CALL FIO_WRITE( LUN, 'Bad pixel list from MAKEBAD', STATUS )

*      define limits of maximum, minimum values
	XMAXIMUM = -1.0E20
	XMINIMUM = 1.0E20
	MEAN_VALUE = 0.0
	SUM = 0.0
	SUMSQ = 0.0
	ONE_SIGMA = 1.0
	BAD_NUMBER = 0

*      calculate mean value in FULL image
	DO J = 1, DIMSY
	  DO K = 1, DIMSX

	    IF( ABS( ARRIN( K, J)) .GT. 1.0E30) THEN
	      BAD_NUMBER = BAD_NUMBER + 1
	    ELSE
	      SUM = SUM + ARRIN( K, J)
	      SUMSQ = SUMSQ + ARRIN( K, J)**2
	    END IF

	  END DO
	END DO

*      calculate maximum and minimum from sigma level
	IF( ( DIMSX*DIMSY) .GT. 0) THEN
	  MEAN_VALUE = SUM/( DIMSX*DIMSY)
          VARIANCE = ( SUMSQ - 2*MEAN_VALUE*SUM +
     :	             (DIMSX*DIMSY)*MEAN_VALUE**2)

          IF( (DIMSX*DIMSY) .EQ. 1 ) THEN
            VARIANCE = VARIANCE/REAL( DIMSX*DIMSY)
          ELSE
            VARIANCE = VARIANCE/( REAL( DIMSX*DIMSY) - 1.0)
          END IF
	END IF

        ONE_SIGMA =  SQRT( ABS( VARIANCE))

	XMAXIMUM = MEAN_VALUE + SIGMA*ONE_SIGMA
	XMINIMUM = MEAN_VALUE - SIGMA*ONE_SIGMA

*      tell user mean and sigma and maximum and minimum
	CALL MSG_SETR( 'MEA', MEAN_VALUE)
	CALL MSG_SETR( 'SIG', ONE_SIGMA)
	CALL MSG_OUT( 'MESS',
     :	  'Mean and one-sigma level = ^MEA +- ^SIG', STATUS)
	CALL MSG_SETR( 'XMX', XMAXIMUM)
	CALL MSG_SETR( 'XMN', XMINIMUM)
	CALL MSG_OUT( 'MESS',
     :	  'Maximum and Minimum levels for cut = ^XMX, ^XMN', STATUS)

*      scan through all the input pixels
	DO J = 1, DIMSY
	  DO K = 1, DIMSX

	    IF( ARRIN( K, J) .LT. XMINIMUM .OR.
     :	        ARRIN( K, J) .GT. XMAXIMUM) THEN
	      NUMPIX = NUMPIX + 1
	      WRITE( LUN, *) K, J
	    END IF

	  END DO
	END DO

*      tell user how many pixels defined as bad from sigma cut
	CALL MSG_SETR( 'VAL', SIGMA)
	CALL MSG_SETI( 'NUM', NUMPIX)
	CALL MSG_OUT( 'MESS',
     :	  'Number of pixels outside ^VAL -sigma cut = ^NUM', STATUS)

*      close output file and release lun
	CALL FIO_CLOSE( LUN, STATUS )

	END
