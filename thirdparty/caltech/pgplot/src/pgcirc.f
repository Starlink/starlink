C*PGCIRC -- draw a circle, using fill-area attributes
C%void cpgcirc(float xcent, float ycent, float radius);
C+
      SUBROUTINE PGCIRC (XCENT, YCENT, RADIUS)
      REAL XCENT, YCENT, RADIUS
C
C Draw a circle. The action of this routine depends
C on the setting of the Fill-Area Style attribute. If Fill-Area Style
C is SOLID (the default), the interior of the circle is solid-filled
C using the current Color Index. If Fill-Area Style is HOLLOW, the
C outline of the circle is drawn using the current line attributes
C (color index, line-style, and line-width).
C
C Arguments:
C  XCENT  (input)  : world x-coordinate of the center of the circle.
C  YCENT  (input)  : world y-coordinate of the center of the circle.
C  RADIUS (input)  : radius of circle (world coordinates).
C--
C 26-Nov-1992 - [TJP].
C 20-Sep-1994 - adjust number of points according to size [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER MAXPTS
      PARAMETER (MAXPTS=72)
C
      INTEGER NPTS,I,RADPIX
      REAL ANGLE
      REAL X(MAXPTS),Y(MAXPTS)
C
      RADPIX = NINT(RADIUS*MAX(PGXSCL(PGID), PGYSCL(PGID)))
      NPTS = MAX(8, MIN(MAXPTS, RADPIX))
      DO 10 I=1,NPTS
         ANGLE = I*360.0/REAL(NPTS)/57.3
         X(I) = XCENT + RADIUS*COS(ANGLE)
         Y(I) = YCENT + RADIUS*SIN(ANGLE)
   10 CONTINUE
      CALL PGPOLY (NPTS,X,Y)
C     write (*,*) 'PGCIRC', NPTS
C-----------------------------------------------------------------------
      END
