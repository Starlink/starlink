      SUBROUTINE sla_DR2TF (NDP, ANGLE, SIGN, IHMSF)
*+
*     - - - - - -
*      D R 2 T F
*     - - - - - -
*
*  Convert an angle in radians to hours, minutes, seconds
*  (double precision)
*
*  Given:
*     NDP      i      number of decimal places of seconds
*     ANGLE    d      angle in radians
*
*  Returned:
*     SIGN     c      '+' or '-'
*     IHMSF    i(4)   hours, minutes, seconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of ANGLE, the format of DOUBLE PRECISION floating-point
*         numbers on the target machine, and the risk of overflowing
*         IHMSF(4).  For example, on the VAX, for ANGLE up to 2pi, the
*         available floating-point precision corresponds roughly to
*         NDP=12.  However, the practical limit is NDP=9, set by the
*         capacity of the 32-bit integer IHMSF(4).
*
*     3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where ANGLE is very nearly 2pi and rounds up to 24 hours,
*         by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
*
*  Called:  sla_DD2TF
*
*  P.T.Wallace   Starlink   19 March 1999
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

*  Turns to radians
      DOUBLE PRECISION T2R
      PARAMETER (T2R=6.283185307179586476925287D0)



*  Scale then use days to h,m,s routine
      CALL sla_DD2TF(NDP,ANGLE/T2R,SIGN,IHMSF)

      END
