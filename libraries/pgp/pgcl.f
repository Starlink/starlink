C
      SUBROUTINE PGCL (K, X, Y, Z)
      INTEGER K
      REAL X, Y, Z
C
C PGPLOT (internal routine): Label one contour segment (for use by
C PGCONX).
C
C Arguments:
C
C K (input, integer): if K=0, move the pen to (X,Y); if K=1, draw
C       a line from the current position to (X,Y); otherwise
C       do nothing.
C X (input, real): X world-coordinate of end point.
C Y (input, real): Y world-coordinate of end point.
C Z (input, real): the value of the contour level, not used by PGCL.
C--
C  5-May-1994 - new routine [TJP]
C  7-Mar-1995 - correct error in angle; do not draw labels outside
C               window [TJP].
C 28-Aug-1995 - check arguments of atan2 [TJP].
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      REAL     XX, YY, XC, YC, XV1, XV2, YV1, YV2, XL, XR, YB, YT
      REAL     XN, YN
      REAL     ANGLE, XO, YO, XP, YP, DINDX, DINDY, XBOX(4), YBOX(4)
      INTEGER  I, TB
      SAVE     I
      DATA     I /0/
C
C     -- transform to world coordinates
      XX = TRANS(1) + TRANS(2)*X + TRANS(3)*Y
      YY = TRANS(4) + TRANS(5)*X + TRANS(6)*Y
C
      IF (K.EQ.0) THEN
C        -- start of contour: reset segment counter
         I = 0
      ELSE
C        -- increment segment counter and check whether this
C           segment should be labelled
         I = MOD(I+1,PGCINT)
         IF (I.EQ.PGCMIN) THEN
C           -- find center of this segment (XC, YC)
            CALL PGQPOS(XP, YP)
            XC = (XX+XP)*0.5
            YC = (YY+YP)*0.5
C            -- find slope of this segment (ANGLE)
            CALL PGQVP(1, XV1, XV2, YV1, YV2)
            CALL PGQWIN(XL, XR, YB, YT)
            ANGLE = 0.0
            IF (XR.NE.XL .AND. YT.NE.YB) THEN
               DINDX = (XV2 - XV1) / (XR - XL)
               DINDY = (YV2 - YV1) / (YT - YB)
               IF (YY-YP.NE.0.0 .OR. XX-XP.NE.0.0)
     :           ANGLE = 57.3*ATAN2((YY-YP)*DINDY, (XX-XP)*DINDX)
            END IF
C           -- check whether point is in window
            XN = (XC-XL)/(XR-XL)
            YN = (YC-YB)/(YT-YB)
            IF (XN.GE.0.0 .AND. XN.LE.1.0 .AND.
     :          YN.GE.0.0 .AND. YN.LE.1.0) THEN
C              -- save current text background and set to erase
               CALL PGQTBG(TB)
               CALL PGSTBG(0)
C              -- find bounding box of label
               CALL PGQTXT(XC, YC, ANGLE, 0.5, PGCLAB, XBOX, YBOX)
               XO = 0.5*(XBOX(1)+XBOX(3))
               YO = 0.5*(YBOX(1)+YBOX(3))
C              -- plot label with bounding box centered at (XC, YC)
               CALL PGPTXT(2.0*XC-XO, 2.0*YC-YO, ANGLE, 0.5, PGCLAB)
C              -- restore text background
               CALL PGSTBG(TB)
            END IF
         END IF
      END IF
      CALL PGMOVE(XX,YY)
      END
