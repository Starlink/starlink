      SUBROUTINE sgs_ICURZ (IZONID)
*+
*   - - - - - -
*    I C U R Z
*   - - - - - -
*
*   Return zone identifier for current zone.
*
*   Returned:
*      IZONID     i     zone identifier
*
*   Read from COMMON:
*      ISZID      i     current zone ID
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IZONID

      INCLUDE 'sgscom'




      IZONID=ISZID

      END
