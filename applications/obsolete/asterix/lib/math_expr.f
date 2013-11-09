*+  MATH_EXPR - Single precision exponentiation with underflow trap
      REAL FUNCTION MATH_EXPR( ARG )
*
*    Description :
*
*     Performs single precision exponentiation with floating underflow
*     check.
*
*    Method :
*
*     On DEC UNIX architectures the single precision EXP() function has a
*     bug whereby arguments smaller than a certain value cause a segmentation
*     fault. Additionally, on SUN machines a too small value generates
*     floating underflow exceptions. This routine alleviates both problems
*     by preventing use of th intrinsic function for very small values,
*     returning zero in both cases.
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
      REAL			ARG			! Argument for EXP()
*
*    Local variables :
*
      REAL			ARGLIM			! Arg limit for EXP()
      LOGICAL                   FIRST			! First time through?
*
*    Local data :
*
      DATA			FIRST/.TRUE./
*-

*    First time through
      IF ( FIRST ) THEN
        ARGLIM = LOG(VAL__SMLR)
        FIRST = .FALSE.
      END IF

*    Would the value underflow if we didn't trap it?
      IF ( ARG .LT. ARGLIM ) THEN
        MATH_EXPR = 0.0
      ELSE
        MATH_EXPR = EXP(ARG)
      END IF

      END
