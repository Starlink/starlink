      SUBROUTINE sgs_IPEN (NPEN)
*+
*   - - - - -
*    I P E N
*   - - - - -
*
*   Inquire current SGS pen number.
*
*   Returned:
*      NPEN     i    SGS pen number
*
*   Read from COMMON:
*      IPEN     i    SGS pen number
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER NPEN

      INCLUDE 'sgscom'




      NPEN = IPEN

      END
