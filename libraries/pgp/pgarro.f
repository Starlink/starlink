C*PGARRO -- draw an arrow
C%void cpgarro(float x1, float y1, float x2, float y2);
C+
      SUBROUTINE PGARRO (X1, Y1, X2, Y2)
      REAL X1, Y1, X2, Y2
C
C Draw an arrow from the point with world-coordinates (X1,Y1) to 
C (X2,Y2). The size of the arrowhead at (X2,Y2) is determined by 
C the current character size set by routine PGSCH. The default size 
C is 1/40th of the smaller of the width or height of the view surface.
C The appearance of the arrowhead (shape and solid or open) is
C controlled by routine PGSAH.
C
C Arguments:
C  X1, Y1 (input)  : world coordinates of the tail of the arrow.
C  X2, Y2 (input)  : world coordinates of the head of the arrow.
C--
C  7-Feb-92 Keith Horne @ STScI / TJP.
C 13-Oct-92 - use arrowhead attributes; scale (TJP).
C-----------------------------------------------------------------------
      INTEGER AHFS, FS
      REAL DX, DY, XV1, XV2, YV1, YV2, XL, XR, YB, YT, DINDX, DINDY
      REAL XINCH, YINCH, RINCH, CA, SA, SO, CO, YP, XP, YM, XM, DHX, DHY
      REAL PX(4), PY(4)
      REAL AHANGL, AHVENT, SEMANG, CH, DH, XS1, XS2, YS1, YS2
C
      CALL PGBBUF
      CALL PGQAH(AHFS, AHANGL, AHVENT)
      CALL PGQFS(FS)
      CALL PGSFS(AHFS)
      DX = X2 - X1
      DY = Y2 - Y1
      CALL PGQCH(CH)
      CALL PGQVSZ(1, XS1, XS2, YS1, YS2)
C     -- length of arrowhead: 1 40th of the smaller of the height or
C        width of the view surface, scaled by character height.
      DH = CH*MIN(ABS(XS2-XS1),ABS(YS2-YS1))/40.0
      CALL PGMOVE(X2, Y2)
C     -- Is there to be an arrowhead ?
      IF (DH.GT.0.) THEN
          IF (DX.NE.0. .OR. DY.NE.0.) THEN
C             -- Get x and y scales
              CALL PGQVP(1, XV1, XV2, YV1, YV2)
              CALL PGQWIN(XL, XR, YB, YT)
              IF (XR.NE.XL .AND. YT.NE.YB) THEN
                  DINDX = (XV2 - XV1) / (XR - XL)
                  DINDY = (YV2 - YV1) / (YT - YB)
                  DHX = DH / DINDX
                  DHY = DH / DINDY
C                 -- Unit vector in direction of the arrow
                  XINCH = DX * DINDX
                  YINCH = DY * DINDY
                  RINCH = SQRT(XINCH*XINCH + YINCH*YINCH)
                  CA = XINCH / RINCH
                  SA = YINCH / RINCH
C                 -- Semiangle in radians
                  SEMANG = AHANGL/2.0/57.296
                  SO = SIN(SEMANG)
                  CO = -COS(SEMANG)
C                 -- Vector back along one edge of the arrow
                  XP = DHX * (CA*CO - SA*SO)
                  YP = DHY * (SA*CO + CA*SO)
C                 -- Vector back along other edge of the arrow
                  XM = DHX * (CA*CO + SA*SO)
                  YM = DHY * (SA*CO - CA*SO)
C                 -- Draw the arrowhead
                  PX(1) = X2
                  PY(1) = Y2
                  PX(2) = X2 + XP
                  PY(2) = Y2 + YP
                  PX(3) = X2 + 0.5*(XP+XM)*(1.0-AHVENT)
                  PY(3) = Y2 + 0.5*(YP+YM)*(1.0-AHVENT)
                  PX(4) = X2 + XM
                  PY(4) = Y2 + YM
                  CALL PGPOLY(4, PX, PY)
                  CALL PGMOVE(PX(3), PY(3))
              END IF
          END IF
      END IF
      CALL PGDRAW(X1, Y1)
      CALL PGMOVE(X2,Y2)
      CALL PGSFS(FS)
      CALL PGEBUF
      RETURN
      END
