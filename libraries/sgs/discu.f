      SUBROUTINE sgs_DISCU
*+
*   - - - - - -
*    D I S C U
*   - - - - - -
*
*   Disable sampling of cursor on the current workstation.
*
*   Read from COMMON:
*      IZTW     i()   zone table - SGS workstation ID
*      IWTID    i()   workstation table - GKS workstation ID
*      ISZID    i     current zone ID
*
*   Constants from GKS_PAR:
*      GREQU    i     request mode
*
*   Externals:
*      GSLCM, sgs_1ILCMO
*
*   P.T.Wallace, D.L.Terrett   Starlink   14 September 1991
*-

      IMPLICIT NONE

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER MODE,IESW,JSTAT



*   Inquire current echo switch setting
      CALL sgs_1ILCMO(MODE,IESW,JSTAT)

*   Set to request mode
      IF (JSTAT.EQ.0) CALL GSLCM(IWTID(ABS(IZTW(ISZID))),1,GREQU,IESW)

      END
