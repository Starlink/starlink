      SUBROUTINE CROPER

* Modifies the GKS workstation transformation so that the workstation
* viewport is the same size as the user's viewport (in NDC). In
* practice this means that the workstation viewport matches the current
* SGS zone. This can be used to crop postscript files so that they are
* only as big are the graphics they contain (instead of being A4 sized).

      IMPLICIT NONE
      INTEGER ERRIND, IWKID, TUS
      REAL NWIN(4), NVP(4), RWIN(4), CWIN(4), RVP(4), CVP(4),
     :     MX, MY, CX, CY

      CALL SGS_ICURW(IWKID)
      CALL GQNT( 1, ERRIND, NWIN, NVP )
      CALL GQWKT( IWKID, ERRIND, TUS, RWIN, CWIN, RVP, CVP )

      MX = ( RVP(2) - RVP(1) )/( RWIN(2) - RWIN(1) )
      CX = RVP(1) - MX*RWIN(1)
      MY = ( RVP(4) - RVP(3) )/( RWIN(4) - RWIN(3) )
      CY = RVP(3) - MY*RWIN(3)

      RVP( 1 ) = MX*NVP(1) + CX
      RVP( 2 ) = MX*NVP(2) + CX
      RVP( 3 ) = MY*NVP(3) + CY
      RVP( 4 ) = MY*NVP(4) + CY

      CALL GSWKWN( IWKID, NVP(1), NVP(2), NVP(3), NVP(4) )
      CALL GSWKVP( IWKID, RVP(1), RVP(2), RVP(3), RVP(4) )

      END
