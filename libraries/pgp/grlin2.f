      SUBROUTINE GRLIN2(X1,Y1,X2,Y2)
*+
*   - - - - - - - -
*     G R L I N 2      (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   Draw a stroke from (X1,Y1) to (X2,Y2) in absolute coordinates
*
*   Given
*      X0       r       Start position x coordinate
*      Y0       r       Start position y coordinate
*      X1       r       Start position x coordinate
*      Y1       r       Start position y coordinate
*
*   Read from COMMON
*      GRXBUF    r()     x line buffer
*      GRYBUF    r()     y line buffer
*      GRBPT     i       Buffer pointer
*      GRCIDE    i       Current device
*      GRVPVI    l()     Viewport visible
*
*   Written to COMMON
*      GRXBUF    r()     x line buffer
*      GRYBUF    r()     y line buffer
*      GRBPT     i       Buffer pointer
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'


      REAL X1,X2,Y1,Y2

*  Some points already in the buffer
      IF (GRBPT.GT.0) THEN

*     Start point is where the last stroke finished so just add the
*     new end point if there is room.
         IF (X1.EQ.GRXBUF(GRBPT) .AND. Y1.EQ.GRYBUF(GRBPT). AND.
     :       GRBPT.LT.GRBSIZ) THEN
            GRBPT = GRBPT + 1
            GRXBUF(GRBPT) = X2
            GRYBUF(GRBPT) = Y2
         ELSE

*      Otherwise empty the buffer first
            IF (GRBPT.GE.2.AND.GRVPVI(GRCIDE))
     1         CALL GPL(GRBPT,GRXBUF,GRYBUF)
            GRXBUF(1) = X1
            GRYBUF(1) = Y1
            GRXBUF(2) = X2
            GRYBUF(2) = Y2
            GRBPT = 2
         ENDIF
      ELSE

*    Buffer was empty - just insert the stroke
         GRXBUF(1) = X1
         GRYBUF(1) = Y1
         GRXBUF(2) = X2
         GRYBUF(2) = Y2
         GRBPT = 2
      ENDIF

      END
