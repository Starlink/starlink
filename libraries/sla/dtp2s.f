      SUBROUTINE sla_DTP2S (XI, ETA, RAZ, DECZ, RA, DEC)
*+
*     - - - - - -
*      D T P 2 S
*     - - - - - -
*
*  Transform tangent plane coordinates into spherical
*  (double precision)
*
*  Given:
*     XI,ETA      dp   tangent plane rectangular coordinates
*     RAZ,DECZ    dp   spherical coordinates of tangent point
*
*  Returned:
*     RA,DEC      dp   spherical coordinates (0-2pi,+/-pi/2)
*
*  Called:        sla_DRANRM
*
*  P.T.Wallace   Starlink   24 July 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION XI,ETA,RAZ,DECZ,RA,DEC

      DOUBLE PRECISION sla_DRANRM

      DOUBLE PRECISION SDECZ,CDECZ,DENOM



      SDECZ=SIN(DECZ)
      CDECZ=COS(DECZ)

      DENOM=CDECZ-ETA*SDECZ

      RA=sla_DRANRM(ATAN2(XI,DENOM)+RAZ)
      DEC=ATAN2(SDECZ+ETA*CDECZ,SQRT(XI*XI+DENOM*DENOM))

      END
