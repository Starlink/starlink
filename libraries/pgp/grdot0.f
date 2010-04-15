      SUBROUTINE GRDOT0 (X,Y)
*+
*    - - - - - - - -
*      G R D O T 0     (GKS emulation of GRPCKG)
*    - - - - - - - -
*
*   Draw a single dot (pixel) at a specified location.
*
*   Given
*      X        r     Absolute x position of dot
*      Y        r     Absolute y position of dot
*
*   Read from COMMON
*      GRCIDE   i    Current device identifier
*      GRIWDT   i()  Line width
*      GRVPVI   l()  Viewport visible
*
*   Written to COMMON
*      GRXPRE   r    Current x position
*      GRYPRE   r    Current y position
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE
      INCLUDE 'grecom.inc'


      REAL     X, Y

*   (X,Y) is the new current position
      GRXPRE(GRCIDE) = X
      GRYPRE(GRCIDE) = Y

      IF (GRVPVI(GRCIDE)) THEN

*     If a "thick pen" is to be simulated, use the line-drawing routines
         IF (GRWIDT(GRCIDE).GT.1) THEN
            CALL GRLIN3(X,Y,X,Y)
         ELSE

*        Use GKS polymarker
            CALL GPM(1,X,Y)
         END IF
      END IF

      END
