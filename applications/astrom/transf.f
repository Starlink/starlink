      SUBROUTINE TRANSF (COEFFS,XMEAS,YMEAS,PCRA,PCDEC,DISTOR,
     :                   X,Y,RA,DEC)
*
*     - - - - - - -
*      T R A N S F
*     _ _ _ _ _ _ _
*
*  Apply model to measured x,y
*
*  Given:
*     COEFFS  d(6)    plate constants
*     XMEAS    d      measured X
*     YMEAS    d      measured Y
*     PCRA     d      plate centre RA
*     PCDEC    d      plate centre Dec
*     DISTOR   d      pincushion/barrel distortion coefficient
*
*  Returned:
*     X        d      predicted X
*     Y        d      predicted Y
*     RA       d      calculated RA
*     DEC      d      calculated Dec
*
*+

      IMPLICIT NONE

      DOUBLE PRECISION COEFFS(6),XMEAS,YMEAS,PCRA,PCDEC,DISTOR,
     :                 X,Y,RA,DEC



*  Measured X,Y to predicted X,Y
      CALL sla_XY2XY(XMEAS,YMEAS,COEFFS,X,Y)

*  Expected X,Y to RA,Dec
      CALL RDCAL(X,Y,PCRA,PCDEC,DISTOR,RA,DEC)

      END
