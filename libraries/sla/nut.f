      SUBROUTINE sla_NUT (DATE, RMATN)
*+
*     - - - -
*      N U T
*     - - - -
*
*  Form the matrix of nutation for a given date - Shirai & Fukushima
*  2001 theory (double precision)
*
*  Reference:
*     Shirai, T. & Fukushima, T., Astron.J. 121, 3270-3283 (2001).
*
*  Given:
*     DATE    d          TDB (loosely ET) as Modified Julian Date
*                                           (=JD-2400000.5)
*  Returned:
*     RMATN   d(3,3)     nutation matrix
*
*  The matrix is in the sense   V(true)  =  RMATN * V(mean)
*
*  Called:   sla_NUTC, sla_DEULER
*
*  P.T.Wallace   Starlink   17 September 2001
*
*  Copyright (C) 2001 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE,RMATN(3,3)

      DOUBLE PRECISION DPSI,DEPS,EPS0



*  Nutation components and mean obliquity
      CALL sla_NUTC(DATE,DPSI,DEPS,EPS0)

*  Rotation matrix
      CALL sla_DEULER('XZX',EPS0,-DPSI,-(EPS0+DEPS),RMATN)

      END
