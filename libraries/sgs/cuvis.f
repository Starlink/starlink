      SUBROUTINE sgs_CUVIS (ON)
*+
*   - - - - - -
*    C U V I S
*   - - - - - -
*
*   Set the visibility of the cursor on the current SGS device.
*
*   Given:
*      ON       l     the desired visibilty
*
*   Read from COMMON:
*      IZTW     i()   zone table - SGS workstation ID
*      IWTID    i()   workstation table - GKS workstation ID
*      ISZID    i     current zone ID
*
*   Constants from GKS_PAR:
*      GECHO    i     echo on
*      GNECHO   i     echo off
*
*   Externals:
*      sgs_1ILCMO, GSLCM
*
*   P.T.Wallace, D.L.Terrett   Starlink   7 September 1991
*-

      IMPLICIT NONE

      LOGICAL ON

      INCLUDE 'sgscom'

      INCLUDE 'GKS_PAR'


      INTEGER MODE,IESW,JSTAT



*   Inquire current locator mode
      CALL sgs_1ILCMO(MODE,IESW,JSTAT)
      IF (JSTAT.NE.0) GO TO 999

*   Set echo switch
      IF (ON) THEN
         IESW = GECHO
      ELSE
         IESW = GNECHO
      END IF
      CALL GSLCM(IWTID(ABS(IZTW(ISZID))),1,MODE,IESW)

  999 CONTINUE

      END
