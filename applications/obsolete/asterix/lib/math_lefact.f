*+  MATH_LEFACT - Returns Logarithm to base e of factorial of integer N.
      REAL FUNCTION MATH_LEFACT( N )
*    Description :
*     <description of what the function does - for user info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     Clive G. Page (LTVAD::CGP)
*
*    History :
*
*     15 Aug 90 : Original (CGP)
*     18 Aug 92 : Moved to ASTERIX (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      INTEGER                   N               !
*
*    Function declarations :
*
*
*    Local constants :
*
      REAL			LRT2PI     	! ln(sqrt(2pi))
	PARAMETER		( LRT2PI=0.91893853 )
      INTEGER 			MAXSTORE
	PARAMETER		( MAXSTORE=100 )
*
*    Local variables :
*
      INTEGER 			I
*
*    Local data :
*
      REAL 			FLN(0:MAXSTORE)
      LOGICAL 			SETUP
      SAVE 			SETUP, FLN
      DATA 			SETUP/.FALSE./
*-

*    Compute table if first time through
      IF ( .NOT. SETUP ) THEN
	FLN(0) = 0.0
	DO I = 1, MAXSTORE
	  FLN(I) = FLN(I-1) + LOG(REAL(I))
	END DO
	SETUP = .TRUE.
      END IF

      IF ( N .LT. 0 ) THEN
	PRINT *, 'LEFACT ERROR: -ve arg for factorial'
	MATH_LEFACT = 0.0

      ELSE IF ( N .LE. MAXSTORE ) THEN
	MATH_LEFACT = FLN(N)

      ELSE

*      Compute from Stirling's approximation
	MATH_LEFACT = (N+0.5)*LOG(REAL(N)) - N + LRT2PI

      END IF

      END
