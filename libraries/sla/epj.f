      DOUBLE PRECISION FUNCTION sla_EPJ (DATE)
*+
*     - - - -
*      E P J
*     - - - -
*
*  Conversion of Modified Julian Date to Julian Epoch (double precision)
*
*  Given:
*     DATE     dp       Modified Julian Date (JD - 2400000.5)
*
*  The result is the Julian Epoch.
*
*  Reference:
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*
*  P.T.Wallace   Starlink   February 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION DATE


      sla_EPJ = 2000D0 + (DATE-51544.5D0)/365.25D0

      END
