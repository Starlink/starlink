      SUBROUTINE sgs_1HSTAT (JSTAT)
*+
*
*   - - - - - -
*    H S T A T      (internal routine)
*   - - - - - -
*
*   Handle incoming status.
*
*   If SGS not in 'inherited status' mode, set JSTAT to zero;
*   otherwise do nothing.
*
*   Given:
*      JSTAT     i      incoming status
*
*   Returned:
*      JSTAT     i      status
*
*   Read from COMMON:
*      JSOPT     i      status handling mode
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER JSTAT

      INCLUDE 'sgscom'



      IF (JSOPT.EQ.0) JSTAT=0

      END
