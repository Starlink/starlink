*+  MATH_EXPD - Double precision exponentiation with underflow trap
      DOUBLE PRECISION FUNCTION MATH_EXPD( ARG )
*
*    Description :
*
*     Performs double precision exponentiation with floating underflow
*     check.
*
*    Method :
*
*     On SUN machines too small an argument to the DEXP function generates
*     floating underflow exceptions. This routine alleviates this problem
*     by preventing use of the intrinsic function for very small values,
*     returning zero in this case.
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*      1 Mar 94 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'PRM_PAR'
*
*    Import :
*
      DOUBLE PRECISION		ARG			! Argument for DEXP()
*
*    Local variables :
*
      DOUBLE PRECISION		ARGLIM			! Arg limit for DEXP()
      LOGICAL                   FIRST			! First time through?
*
*    Local data :
*
      DATA			FIRST/.TRUE./
*-

*    First time through
      IF ( FIRST ) THEN
        ARGLIM = LOG(VAL__SMLD)
        FIRST = .FALSE.
      END IF

*    Would the value underflow if we didn't trap it?
      IF ( ARG .LT. ARGLIM ) THEN
        MATH_EXPD = 0.0
      ELSE
        MATH_EXPD = DEXP(ARG)
      END IF

      END
