      REAL FUNCTION sla_VDV (VA, VB)
*+
*     - - - -
*      V D V
*     - - - -
*
*  Scalar product of two 3-vectors  (single precision)
*
*  Given:
*      VA      real(3)     first vector
*      VB      real(3)     second vector
*
*  The result is the scalar product VA.VB (single precision)
*
*  P.T.Wallace   Starlink   November 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL VA(3),VB(3)


      sla_VDV=VA(1)*VB(1)+VA(2)*VB(2)+VA(3)*VB(3)

      END
