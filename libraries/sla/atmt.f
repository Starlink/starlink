      SUBROUTINE sla__ATMT (R0, T0, ALPHA, GAMM2, DELM2,
     :                      C1, C2, C3, C4, C5, C6, R, T, DN, RDNDR)
*+
*     - - - - -
*      A T M T
*     - - - - -
*
*  Internal routine used by REFRO
*
*  Refractive index and derivative with respect to height for the
*  troposphere.
*
*  Given:
*    R0      d    height of observer from centre of the Earth (metre)
*    T0      d    temperature at the observer (deg K)
*    ALPHA   d    alpha          )
*    GAMM2   d    gamma minus 2  ) see HMNAO paper
*    DELM2   d    delta minus 2  )
*    C1      d    useful term  )
*    C2      d    useful term  )
*    C3      d    useful term  ) see source
*    C4      d    useful term  ) of sla_REFRO
*    C5      d    useful term  )
*    C6      d    useful term  )
*    R       d    current distance from the centre of the Earth (metre)
*
*  Returned:
*    T       d    temperature at R (deg K)
*    DN      d    refractive index at R
*    RDNDR   d    R * rate the refractive index is changing at R
*
*  Note that in the optical case C5 and C6 are zero.
*
*  P.T.Wallace   Starlink   30 May 1997
*
*  Copyright (C) 1997 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION R0,T0,ALPHA,GAMM2,DELM2,C1,C2,C3,C4,C5,C6,
     :                 R,T,DN,RDNDR

      DOUBLE PRECISION TT0,TT0GM2,TT0DM2


      T = MAX(MIN(T0-ALPHA*(R-R0),320D0),100D0)
      TT0 = T/T0
      TT0GM2 = TT0**GAMM2
      TT0DM2 = TT0**DELM2
      DN = 1D0+(C1*TT0GM2-(C2-C5/T)*TT0DM2)*TT0
      RDNDR = R*(-C3*TT0GM2+(C4-C6/TT0)*TT0DM2)

      END
