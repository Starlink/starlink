      SUBROUTINE GRFLU0
*+
*
*     - - - - - - - -
*       G R F L U 0    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Flush line polyline buffer
*
*   Read from COMMON
*      GRBPT     i      Buffer pointer
*      GRXBUF    r()    Line buffer X
*      GRYBUF    r()    Line buffer Y
*      GRCIDE    i      Current device
*      GRVPVI    l()    Viewport visible
*
*   Written to COMMON
*      GRBPT     i      Buffer pointer
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INCLUDE 'grecom.inc'


*     Flush line buffer
      IF (GRBPT.GE.2) THEN
         IF (GRVPVI(GRCIDE)) CALL GPL(GRBPT,GRXBUF,GRYBUF)
         GRBPT = 0
      END IF

      END
