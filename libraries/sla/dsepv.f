      DOUBLE PRECISION FUNCTION sla_DSEPV (V1, V2)
*+
*     - - - - - -
*      D S E P V
*     - - - - - -
*
*  Angle between two vectors.
*
*  (double precision)
*
*  Given:
*     V1      d(3)    first vector
*     V2      d(3)    second vector
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
*  Called:  sla_DVXV, sla_DVN, sla_DVDV
*
*  Last revision:   7 May 2000
*
*  Copyright P.T.Wallace.  All rights reserved.
*-

      IMPLICIT NONE

      DOUBLE PRECISION V1(3),V2(3)

      DOUBLE PRECISION V1XV2(3),WV(3),S,C
      DOUBLE PRECISION sla_DVDV



*  Modulus of cross product = sine multiplied by the two moduli.
      CALL sla_DVXV(V1,V2,V1XV2)
      CALL sla_DVN(V1XV2,WV,S)

*  Dot product = cosine multiplied by the two moduli.
      C = sla_DVDV(V1,V2)

*  Angle between the vectors.
      IF (S.NE.0D0) THEN
         sla_DSEPV = ATAN2(S,C)
      ELSE
         sla_DSEPV = 0D0
      END IF

      END
