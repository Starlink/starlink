      SUBROUTINE sla__ATMS (RT, TT, DNT, GAMAL, R, DN, RDNDR)
*+
*     - - - - -
*      A T M S
*     - - - - -
*
*  Internal routine used by REFRO
*
*  Refractive index and derivative with respect to height for the
*  stratosphere.
*
*  Given:
*    RT      d    height of tropopause from centre of the Earth (metre)
*    TT      d    temperature at the tropopause (deg K)
*    DNT     d    refractive index at the tropopause
*    GAMAL   d    constant of the atmospheric model = G*MD/R
*    R       d    current distance from the centre of the Earth (metre)
*
*  Returned:
*    DN      d    refractive index at R
*    RDNDR   d    R * rate the refractive index is changing at R
*
*  P.T.Wallace   Starlink   14 July 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION RT,TT,DNT,GAMAL,R,DN,RDNDR

      DOUBLE PRECISION B,W


      B = GAMAL/TT
      W = (DNT-1D0)*EXP(-B*(R-RT))
      DN = 1D0+W
      RDNDR = -R*B*W

      END
