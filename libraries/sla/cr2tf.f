      SUBROUTINE sla_CR2TF (NDP, ANGLE, SIGN, IHMSF)
*+
*     - - - - - -
*      C R 2 T F
*     - - - - - -
*
*  Convert an angle in radians into hours, minutes, seconds
*  (single precision)
*
*  Given:
*     NDP       int      number of decimal places of seconds
*     ANGLE     real     angle in radians
*
*  Returned:
*     SIGN      char     '+' or '-'
*     IHMSF     int(4)   hours, minutes, seconds, fraction
*
*  Notes:
*
*  1)  NDP less than zero is interpreted as zero.
*
*  2)  The largest useful value for NDP is determined by the size of
*      ANGLE, the format of REAL floating-point numbers on the target
*      machine, and the risk of overflowing IHMSF(4).  For example,
*      on the VAX, for ANGLE up to 2pi, the available floating-point
*      precision corresponds roughly to NDP=3.  This is well below
*      the ultimate limit of NDP=9 set by the capacity of the 32-bit
*      integer IHMSF(4).
*
*  3)  The absolute value of ANGLE may exceed 2pi.  In cases where it
*      does not, it is up to the caller to test for and handle the
*      case where ANGLE is very nearly 2pi and rounds up to 24 hours,
*      by testing for IHMSF(1)=24 and setting IHMSF(1-4) to zero.
*
*  Called:  sla_CD2TF
*
*  P.T.Wallace   Starlink   18 March 1999
*
*  Copyright (C) 1999 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      INTEGER NDP
      REAL ANGLE
      CHARACTER SIGN*(*)
      INTEGER IHMSF(4)

*  Turns to radians
      REAL T2R
      PARAMETER (T2R=6.283185307179586476925287)



*  Scale then use days to h,m,s routine
      CALL sla_CD2TF(NDP,ANGLE/T2R,SIGN,IHMSF)

      END
