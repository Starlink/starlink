      SUBROUTINE RDCAL (X,Y,PCRA,PCDEC,DISTOR,RA,DEC)
*+
*
*     - - - - - -
*      R D C A L
*     - - - - - -
*
*  Adjust expected plate coordinates X,Y according to
*  telescope type, then calculate RA,Dec
*
*  Given:
*     X        d       expected plate coordinate
*     Y        d       expected plate coordinate
*     PCRA     d       plate centre RA
*     PCDEC    d       plate centre Dec
*     DISTOR   d       pincushion/barrel distortion coefficient
*
*  Returned:
*     RA       d       calculated RA
*     DEC      d       calculated Dec
*+

      IMPLICIT NONE

      DOUBLE PRECISION X,Y,PCRA,PCDEC,DISTOR,RA,DEC

      DOUBLE PRECISION XI,ETA



*  Get tangent plane coordinates
      XI=X
      ETA=Y

*  Adjust geometry
      CALL sla_UNPCD(DISTOR,XI,ETA)

*  Compute RA,Dec
      CALL sla_DTP2S(XI,ETA,PCRA,PCDEC,RA,DEC)

      END
