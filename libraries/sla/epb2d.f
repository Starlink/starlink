      DOUBLE PRECISION FUNCTION sla_EPB2D (EPB)
*+
*     - - - - - -
*      E P B 2 D
*     - - - - - -
*
*  Conversion of Besselian Epoch to Modified Julian Date
*  (double precision)
*
*  Given:
*     EPB      dp       Besselian Epoch
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

      DOUBLE PRECISION EPB


      sla_EPB2D = 15019.81352D0 + (EPB-1900D0)*365.242198781D0

      END
