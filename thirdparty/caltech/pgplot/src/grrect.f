
C*GRRECT -- fill a rectangle
C+
      SUBROUTINE GRRECT (X0,Y0,X1,Y1)
      REAL X0, Y0, X1, Y1
C
C GRPCKG: Fill a rectangle with solid color.  The rectangle
C is defined by the (x,y) world coordinates of its lower left and upper 
C right corners; the edges are parallel to the coordinate axes.
C
C Arguments:
C
C X0, Y0 (input, real): world coordinates of one corner of the 
C       rectangle.
C X1, Y1 (input, real): world coordinates of the opposite corner of the 
C       rectangle.
C--
C 23-Mar-1988 - [TJP].
C 18-Jan-1991 - Code moved from GRRECT to GRREC0 so that it can also be
C               used by GRPXRE
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    XLL, YLL, XUR, YUR
      REAL    XMIN, YMIN, XMAX, YMAX
C
      IF (GRCIDE.LT.1) RETURN
C
C Convert to device coordinates and clip.
C
      CALL GRTXY0(.FALSE.,X0,Y0,XLL,YLL)
      CALL GRTXY0(.FALSE.,X1,Y1,XUR,YUR)
      XMIN = MIN(XLL,XUR)
      XMAX = MAX(XLL,XUR)
      YMIN = MIN(YLL,YUR)
      YMAX = MAX(YLL,YUR)
C
C Do the real work
C
      CALL GRREC0(XMIN,YMIN,XMAX,YMAX)
      END
