C PGBOX1 -- support routine for PGBOX
C
      SUBROUTINE PGBOX1 (XA, XB, XD, I1, I2)
      REAL XA, XB, XD
      INTEGER I1, I2
C
C This routine is used to determine where to draw the tick marks on
C an axis. The input arguments XA and XB are the world-coordinate
C end points of the axis; XD is the tick interval. PGBOX1 returns
C two integers, I1 and I2, such that the required tick marks are
C to be placed at world-coordinates (I*XD), for I=I1,...,I2.
C Normally I2 is greater than or equal to I1, but if there are no
C values of I such that I*XD lies in the inclusive range (XA, XB),
C then I2 will be 1 less than I1.
C
C Arguments:
C  XA, XB (input)  : world-coordinate end points of the axis. XA must
C                    not be equal to XB; otherwise, there are no
C                    restrictions.
C  XD     (input)  : world-coordinate tick interval. XD may be positive
C                    or negative, but may not be zero.
C  I1, I2 (output) : tick marks should be drawn at world
C                    coordinates I*XD for I in the inclusive range
C                    I1...I2 (see above).
C
C 14-Jan-1986 - new routine [TJP].
C 13-Dec-1990 - remove rror check [TJP].
C-----------------------------------------------------------------------
      REAL XLO, XHI
C
      XLO = MIN(XA/XD, XB/XD)
      XHI = MAX(XA/XD, XB/XD)
      I1 = NINT(XLO)
      IF (I1.LT.XLO) I1 = I1+1
      I2 = NINT(XHI)
      IF (I2.GT.XHI) I2 = I2-1
      END
