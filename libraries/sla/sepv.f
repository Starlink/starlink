      REAL FUNCTION sla_SEPV (V1, V2)
*+
*     - - - - -
*      S E P V
*     - - - - -
*
*  Angle between two vectors.
*
*  (single precision)
*
*  Given:
*     V1      r(3)    first vector
*     V2      r(3)    second vector
*
*  The result is the angle, in radians, between the two vectors.  It
*  is always positive.
*
*  Notes:
*
*  1  There is no requirement for the vectors to be unit length.
*
*  2  If either vector is null, zero is returned.
*
*  3  The simplest formulation would use dot product alone.  However,
*     this would reduce the accuracy for angles near zero and pi.  The
*     algorithm uses both cross product and dot product, which maintains
*     accuracy for all sizes of angle.
*
*  Called:  sla_DSEPV
*
*  Last revision:   7 May 2000
*
*  Copyright P.T.Wallace.  All rights reserved.
*-

      IMPLICIT NONE

      REAL V1(3),V2(3)

      INTEGER I
      DOUBLE PRECISION DV1(3),DV2(3)
      DOUBLE PRECISION sla_DSEPV



*  Use double precision version.
      DO I=1,3
         DV1(I) = DBLE(V1(I))
         DV2(I) = DBLE(V2(I))
      END DO
      sla_SEPV = REAL(sla_DSEPV(DV1,DV2))

      END
