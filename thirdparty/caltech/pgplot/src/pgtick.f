C*PGTICK -- draw a single tick mark on an axis
C%void cpgtick(float x1, float y1, float x2, float y2, float v, \
C% float tikl, float tikr, float disp, float orient, const char *str);
C+
      SUBROUTINE PGTICK (X1, Y1, X2, Y2, V, TIKL, TIKR, DISP, 
     :                   ORIENT, STR)
      REAL X1, Y1, X2, Y2, V, TIKL, TIKR, DISP, ORIENT
      CHARACTER*(*) STR
C
C Draw and label single tick mark on a graph axis. The tick mark is
C a short line perpendicular to the direction of the axis (which is not
C drawn by this routine). The optional text label is drawn with its
C baseline parallel to the axis and reading in the same direction as
C the axis (from point 1 to point 2). Current line and text attributes
C are used.
C
C Arguments:
C  X1, Y1 (input)  : world coordinates of one endpoint of the axis.
C  X2, Y2 (input)  : world coordinates of the other endpoint of the axis.
C  V      (input)  : draw the tick mark at fraction V (0<=V<=1) along
C                    the line from (X1,Y1) to (X2,Y2).
C  TIKL   (input)  : length of tick mark drawn to left of axis
C                    (as seen looking from first endpoint to second), in
C                    units of the character height.
C  TIKR   (input)  : length of major tick marks drawn to right of axis,
C                    in units of the character height.
C  DISP   (input)  : displacement of label text to
C                    right of axis, in units of the character height.
C  ORIENT (input)  : orientation of label text, in degrees; angle between
C                    baseline of text and direction of axis (0-360°).
C  STR    (input)  : text of label (may be blank).
C--
C 25-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL X, Y, XV1, XV2, YV1, YV2, XW1, XW2, YW1, YW2
      REAL XPMM, YPMM, LENMM, ANGLE, XCH, YCH
      REAL TIKX, TIKY, FJUST, D, OR
C
C Check arguments.
C
      IF (X1.EQ.X2 .AND. Y1.EQ.Y2) RETURN
C
C Get current character height (mm) [note: XCH = YCH].
C
      CALL PGQCS(2, XCH, YCH)
C
C Get x and y scales (units per mm).
C
      CALL PGQVP(2, XV1, XV2, YV1, YV2)
      CALL PGQWIN(XW1, XW2, YW1, YW2)
      XPMM  = (XW2-XW1)/(XV2-XV1)
      YPMM  = (YW2-YW1)/(YV2-YV1)
C
C Length of axis in mm.
C
      LENMM = SQRT(((X2-X1)/XPMM)**2 + ((Y2-Y1)/YPMM)**2)
C
C Angle of axis to horizontal (device coordinates).
C
      ANGLE = ATAN2((Y2-Y1)/YPMM, (X2-X1)/XPMM)*57.29577951
C
C (x,y) displacement for 1 character height perpendicular to axis.
C
      TIKX = (Y1-Y2)*XCH*XPMM/(LENMM*YPMM)
      TIKY = (X2-X1)*XCH*YPMM/(LENMM*XPMM)
C
C Draw the tick mark at point (X,Y) on the axis.
C
      X = X1 + V*(X2-X1)
      Y = Y1 + V*(Y2-Y1)
      CALL PGMOVE(X - TIKR*TIKX, Y - TIKR*TIKY)
      CALL PGDRAW(X + TIKL*TIKX, Y + TIKL*TIKY)
C
C Label the tick mark.
C
      D = DISP
      IF (STR.EQ.' ') RETURN
      OR = MOD(ORIENT, 360.0)
      IF (OR.LT.0.0) OR=OR+360.0
      IF (OR.GT.45.0 .AND. OR.LE.135.0) THEN
         FJUST = 0.0
         IF (D.LT.0.0) FJUST = 1.0
      ELSE IF (OR.GT.135.0 .AND. OR.LE.225.0) THEN
         FJUST = 0.5
         IF (D.LT.0.0) D = D-1.0
      ELSE IF (OR.GT.225.0 .AND. OR.LE.315.0) THEN
         ANGLE = ANGLE+90.0
         FJUST = 1.0
         IF (D.LT.0.0) FJUST = 0.0
      ELSE
         FJUST = 0.5
         IF (D.GT.0.0) D = D+1.0
      END IF            
      CALL PGPTXT(X-D*TIKX, Y-D*TIKY, ANGLE-OR, FJUST, STR)
      END
