      SUBROUTINE GRLIN0 (XP,YP)
*+
*   - - - - - - - -
*     G R L I N 0       (GKS emulation of GRPCKG)
*   - - - - - - - -
*
*   draw a line from the current position to a specified position,
*   which becomes the new current position. This routine takes care of
*   viewport boundary, dashed and thick lines.
*
*   Given
*      XP        r     Absolute x coordinate of end point
*      YP        r     Absolute y coordinate of end point
*
*   Read from COMMON
*      GRCIDE    i     Current device id
*      GRXPRE    r()   Current x point
*      GRYPRE    r()   Current y point
*      GRSTYL    i()   Line style
*      GRWIDT    i()   Line width
*
*   Written to COMMON
*      GRXPRE    r()   Current x point
*      GRYPRE    r()   Current y point
*
*   D.L.Terrett  Starlink  Aug 1987  (After T.J.Pearson)
*+
*
      IMPLICIT NONE
      INCLUDE 'grecom.inc'


      REAL     XP,YP, X0,Y0

*   End-points of line are (X0,Y0), (XP,YP).
      X0 = GRXPRE(GRCIDE)
      Y0 = GRYPRE(GRCIDE)
      GRXPRE(GRCIDE) = XP
      GRYPRE(GRCIDE) = YP

*    Draw the line in the appropriate style.
      IF (GRSTYL(GRCIDE).GT.1) THEN

*   Dashed line
          CALL GRLIN1(X0,Y0,XP,YP)

      ELSE IF (GRWIDT(GRCIDE).GT.1) THEN

*   thick line
          CALL GRLIN3(X0,Y0,XP,YP)
      ELSE

*  normal line
          CALL GRLIN2(X0,Y0,XP,YP)

      END IF

      END
