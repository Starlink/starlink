      SUBROUTINE sgs_ENSCU
*+
*   - - - - - -
*    E N S C U
*   - - - - - -
*
*   Enable sampling of cursor on the current workstation.
*
*   Read from COMMON:
*      IZTW     i()   zone table - SGS workstation ID
*      IWTID    i()   workstation table - GKS workstation ID
*      ISZID    i     current zone ID
*
*   Constants from GKS_PAR:
*      GSAMPL   i     sample mode
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



*   Inquire echo switch setting
      CALL sgs_1ILCMO(MODE,IESW,JSTAT)                       

*   Enable for sample
      IF (JSTAT.EQ.0) CALL GSLCM(IWTID(ABS(IZTW(ISZID))),1,GSAMPL,IESW)

      END
