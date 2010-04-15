      DOUBLE PRECISION FUNCTION MACH_PREC()
*+
* Name:
*    MACH_PREC

* Invocation:
*    (DOUBLE PRECISION) = MACH_PREC()

* Purpose:
*   Calculates Machine precision
*   replacment for NAG X02ajf

* Description:
*   Calculates machine precision ( 0.5*B^(1-T) ) using values
*   obtained from PDA_I1MACH  (10 = B, 14 = T)

* Arguments:
*    None
* Authors:
*    J.W.Palmer, Manchester 27-03-97
* History:
*-
      IMPLICIT NONE

      INTEGER PDA_I1MACH
      REAL B,T

*

      B = PDA_I1MACH(10)
      T = PDA_I1MACH(14)
      MACH_PREC = 0.5*B**(1-T)

      END










