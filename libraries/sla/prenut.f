      SUBROUTINE sla_PRENUT (EPOCH, DATE, RMATPN)
*+
*     - - - - - - -
*      P R E N U T
*     - - - - - - -
*
*  Form the matrix of precession and nutation (IAU1976/1980/FK5)
*  (double precision)
*
*  Given:
*     EPOCH   dp         Julian Epoch for mean coordinates
*     DATE    dp         Modified Julian Date (JD-2400000.5)
*                        for true coordinates
*
*  Returned:
*     RMATPN  dp(3,3)    combined precession/nutation matrix
*
*  Called:  sla_PREC, sla_EPJ, sla_NUT, sla_DMXM
*
*  Notes:
*
*  1)  The epoch and date are TDB (loosely ET).
*
*  2)  The matrix is in the sense   V(true)  =  RMATPN * V(mean)
*
*  P.T.Wallace   Starlink   8 May 2000
*
*  Copyright (C) 2000 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION EPOCH,DATE,RMATPN(3,3)

      DOUBLE PRECISION RMATP(3,3),RMATN(3,3),sla_EPJ



*  Precession
      CALL sla_PREC(EPOCH,sla_EPJ(DATE),RMATP)

*  Nutation
      CALL sla_NUT(DATE,RMATN)

*  Combine the matrices:  PN = N x P
      CALL sla_DMXM(RMATN,RMATP,RMATPN)

      END
