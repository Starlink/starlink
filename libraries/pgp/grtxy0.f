      SUBROUTINE GRTXY0(ABSXY,X,Y,XT,YT)
*+
*     - - - - - - - -
*       G R T X Y 0    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Convert a position to an absolute position
*
*   Given
*      ABSXY    l     Absolute or scaled position
*      X        r     X coordinate
*      Y        r     Y coordinate
*      X        r     X absolute coordinate
*      Y        r     Y absolute coordinate
*
*   Read from COMMON
*      GRCIDE   i     Current device
*      GRXSCL   r()   X scale
*      GRYSCL   r()   Y scale
*      GRXORG   r()   X origin
*      GRYORG   r()   Y origin
*
*   D.L.Terrett  Starlink  Jul 1987
*+
      IMPLICIT NONE
      REAL X,Y,XT,YT
      LOGICAL ABSXY

      INCLUDE 'grecom.inc'


      IF (ABSXY) THEN
         XT = X
         YT = Y
      ELSE
         XT = GRXSCL(GRCIDE) * X + GRXORG(GRCIDE)
         YT = GRYSCL(GRCIDE) * Y + GRYORG(GRCIDE)
      END IF

      END
