      SUBROUTINE GRSLS(IS)
*+
*     - - - - - - -
*       G R S L S     (GKS emulation of GRPCKG)
*     - - - - - - -
*
*   Set the current line style.
*
*   Given
*      IS       i     Line style
*
*   Read from COMMON
*      GRCIDE   i     Current device id
*
*   Written to COMMON
*      GRSTYL   i()   Current line style
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER IS

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRSLS - No PGPLOT device open',
     :   GRNODO)
      ELSE
         IF (IS.LE.0.OR.IS.GT.5) THEN
            CALL MSG_SETI('IS', IS)
            CALL ERR_REP('GRILLS',
     :    'GRSLS - ^LS is not a valid line style; style 1 selected',
     :      GRILLS)
            GRSTYL(GRCIDE) = 1
         ELSE
            GRSTYL(GRCIDE) = IS
         END IF
      END IF

      END
