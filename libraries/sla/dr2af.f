      SUBROUTINE sla_DR2AF (NDP, ANGLE, SIGN, IDMSF)
*+
*     - - - - - -
*      D R 2 A F
*     - - - - - -
*
*  Convert an angle in radians to degrees, arcminutes, arcseconds
*  (double precision)
*
*  Given:
*     NDP      i      number of decimal places of arcseconds
*     ANGLE    d      angle in radians
*
*  Returned:
*     SIGN     c      '+' or '-'
*     IDMSF    i(4)   degrees, arcminutes, arcseconds, fraction
*
*  Notes:
*
*     1)  NDP less than zero is interpreted as zero.
*
*     2)  The largest useful value for NDP is determined by the size
*         of ANGLE, the format of DOUBLE PRECISION floating-point
*         numbers on the target machine, and the risk of overflowing
*         IDMSF(4).  For example, on the VAX, for ANGLE up to 2pi, the
*         available floating-point precision corresponds roughly to
*         NDP=12.  However, the practical limit is NDP=9, set by the
*         capacity of the 32-bit integer IDMSF(4).
*
*     3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*         does not, it is up to the caller to test for and handle the
*         case where ANGLE is very nearly 2pi and rounds up to 360 deg,
*         by testing for IDMSF(1)=360 and setting IDMSF(1-4) to zero.
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
      INTEGER IDMSF(4)

*  Hours to degrees * radians to turns
      DOUBLE PRECISION F
      PARAMETER (F=15D0/6.283185307179586476925287D0)



*  Scale then use days to h,m,s routine
      CALL sla_DD2TF(NDP,ANGLE*F,SIGN,IDMSF)

      END
