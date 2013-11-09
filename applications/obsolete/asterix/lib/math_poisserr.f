*+  MATH_POISSERR - Set up array of Poisson errors
      SUBROUTINE MATH_POISSERR( DATA, N, MINVAL, ERROR )
*
*    Description :
*
*      Creates an array of Poisson errors from the data. If any error value
*      is zero then it is replaced by MINVAL.
*
*    History :
*
*     5 Aug 88 : Original ( dja @ uk.ac.bham.sr.star )
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER	      N		  ! Number of DATA and ERROR values

      REAL	      MINVAL      ! Value with which to replace zero error
      REAL	      DATA( N )   ! The input data
*
*    Export :
*
      REAL	      ERROR( N )  ! The output errors
*
*    Local variables :
*
      INTEGER	      I		  ! Loop counter
*
*-
	DO I = 1 , N
	  ERROR( I ) = SQRT( DATA( I ) )
	  IF ( ERROR( I ) .EQ. 0.0 ) ERROR( I ) = MINVAL

	END DO

      END
