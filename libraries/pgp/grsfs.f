      SUBROUTINE GRSFS(IS)
*+
*     - - - - - - -
*       G R F L S     (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Set the current fill area style.
*
*   Given
*      IS       i     Line style
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   Written to COMMON
*      GRFAST   i()   Current fill style
*
*   D.L.Terrett  Starlink  Mar 1991
*+
      IMPLICIT NONE

      INTEGER IS

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'

      INCLUDE 'GKS_PAR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSFS - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (IS.LT.1.OR.IS.GT.2) THEN
            CALL MSG_SETI('IS', IS)
            CALL ERR_REP('GRILFS',
     :    'GRSFS - ^IS is not a valid fill style; style 1 selected',
     :      GRILLS)
            GRFAST(GRCIDE) = 1
         ELSE
            GRFAST(GRCIDE) = IS
         END IF
      END IF


*  Set the appropriate GKS fill area interior style

      GOTO (10,20) GRFAST(GRCIDE)
   10 CONTINUE
      CALL GSFAIS(GSOLID)
      GO TO 100
   20 CONTINUE
      CALL GSFAIS(GHOLLO)
 100  CONTINUE

      END
