      SUBROUTINE sgs_ICURW (IWKID)
*+
*   - - - - - -
*    I C U R W
*   - - - - - -
*
*   Inquire GKS workstation ID for currently selected zone.
*
*   Returned:
*      IWKID    i     workstation ID
*
*   Read from COMMON:
*      IZWT     i()   zone table - SGS workstation ID
*      IWTID    i()   workstation table - GKS workstation ID
*      ISZID    i     current zone ID
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INTEGER IWKID

      INCLUDE 'sgscom'




      IWKID=IWTID(ABS(IZTW(ISZID)))

      END
