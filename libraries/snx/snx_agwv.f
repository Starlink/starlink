
      SUBROUTINE snx_AGWV

*+
*
*  - - - - -
*   A G W V
*  - - - - -
*
*  Set up NCAR AUTOGRAPH graph window to match the
*  current GKS viewport.
*
*  Called:  GQCNTN, GQNT, AGSETP
*
*  P T Wallace   Starlink   April 1986
*
*+

      IMPLICIT NONE

      REAL WIND(4),VIEWP(4)
      INTEGER J

      INTEGER NCT



*  Inquire the current GKS transformation number
      CALL GQCNTN(J,NCT)

*  Find out what part of the NDC square is available
      CALL GQNT(NCT,J,WIND,VIEWP)

*  Establish the AUTOGRAPH coordinate system
      CALL AGSETP('GRAPH.',VIEWP,4)

      END
