*+  MATH_POISSVAR - Set up array of Poisson variances
      SUBROUTINE MATH_POISSVAR( DATA, N, MINVAL,VARS )
*
*    Description :
*
*      Creates an array of Poisson variances from the data. If any value
*      is zero then it is replaced by MINVAL. From Poisson error routine.
*
*    History :
*
*    15 Aug 88 : Original ( dja @ uk.ac.bham.sr.star )
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
      REAL	      VARS( N )  ! The output variances
*
*    Local variables :
*
      INTEGER	      I		  ! Loop counter
*
*-
      DO I = 1 , N
         VARS(I) = ABS( DATA(I) )
         IF ( VARS( I ) .EQ. 0.0 ) VARS( I ) = MINVAL

      END DO

      END
