      DOUBLE PRECISION FUNCTION sla_EPJ2D (EPJ)
*+
*     - - - - - -
*      E P J 2 D
*     - - - - - -
*
*  Conversion of Julian Epoch to Modified Julian Date (double precision)
*
*  Given:
*     EPJ      dp       Julian Epoch
*
*  The result is the Modified Julian Date (JD - 2400000.5).
*
*  Reference:
*     Lieske,J.H., 1979. Astron.Astrophys.,73,282.
*
*  P.T.Wallace   Starlink   February 1984
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION EPJ


      sla_EPJ2D = 51544.5D0 + (EPJ-2000D0)*365.25D0

      END
