      SUBROUTINE sla_TP2S (XI, ETA, RAZ, DECZ, RA, DEC)
*+
*     - - - - -
*      T P 2 S
*     - - - - -
*
*  Transform tangent plane coordinates into spherical
*  (single precision)
*
*  Given:
*     XI,ETA      real  tangent plane rectangular coordinates
*     RAZ,DECZ    real  spherical coordinates of tangent point
*
*  Returned:
*     RA,DEC      real  spherical coordinates (0-2pi,+/-pi/2)
*
*  Called:        sla_RANORM
*
*  P.T.Wallace   Starlink   24 July 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL XI,ETA,RAZ,DECZ,RA,DEC

      REAL sla_RANORM

      REAL SDECZ,CDECZ,DENOM



      SDECZ=SIN(DECZ)
      CDECZ=COS(DECZ)

      DENOM=CDECZ-ETA*SDECZ

      RA=sla_RANORM(ATAN2(XI,DENOM)+RAZ)
      DEC=ATAN2(SDECZ+ETA*CDECZ,SQRT(XI*XI+DENOM*DENOM))

      END
