C*PGSCRL -- scroll window
C%void cpgscrl(float dx, float dy);
C+
      SUBROUTINE PGSCRL (DX, DY)
      REAL DX, DY
C
C This routine moves the window in world-coordinate space while
C leaving the viewport unchanged. On devices that have the
C capability, the pixels within the viewport are scrolled
C horizontally, vertically or both in such a way that graphics
C previously drawn in the window are shifted so that their world
C coordinates are unchanged.
C
C If the old window coordinate range was (X1, X2, Y1, Y2), the new
C coordinate range will be approximately (X1+DX, X2+DX, Y1+DY, Y2+DY).
C The size and scale of the window are unchanged.
C
C Thee window can only be shifted by a whole number of pixels
C (device coordinates). If DX and DY do not correspond to integral
C numbers of pixels, the shift will be slightly different from that
C requested. The new window-coordinate range, and hence the exact
C amount of the shift, can be determined by calling PGQWIN after this
C routine.
C
C Pixels that are moved out of the viewport by this operation are
C lost completely; they cannot be recovered by scrolling back.
C Pixels that are ``scrolled into'' the viewport are filled with
C the background color (color index 0).
C
C If the absolute value of DX is bigger than the width of the window,
C or the aboslute value of DY is bigger than the height of the window,
C the effect will be the same as zeroing all the pixels in the
C viewport.
C
C Not all devices have the capability to support this routine.
C It is only available on some interactive devices that have discrete
C pixels. To determine whether the current device has scroll capability,
C call PGQINF.
C
C Arguments:
C  DX     (input)  : distance (in world coordinates) to shift the
C                    window horizontally (positive shifts window to the
C                    right and scrolls to the left).
C  DY     (input)  : distance (in world coordinates) to shift the
C                    window vertically (positive shifts window up and
C                    scrolls down).
C--
C 25-Feb-97: new routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL X1, X2, Y1, Y2, DDX, DDY
      INTEGER NDX, NDY
C
      IF (PGNOTO('PGSCRL')) RETURN
C
C Shift must be a whole number of pixels.
C
      NDX = NINT(DX*PGXSCL(PGID))
      NDY = NINT(DY*PGYSCL(PGID))
C
      IF (NDX.NE.0 .OR. NDY.NE.0) THEN
         CALL PGBBUF
         DDX = NDX/PGXSCL(PGID)
         DDY = NDY/PGYSCL(PGID)
C
C        -- Set new world-ccordinate window.
C
         X1 = PGXBLC(PGID)
         X2 = PGXTRC(PGID)
         Y1 = PGYBLC(PGID)
         Y2 = PGYTRC(PGID)
         PGXBLC(PGID) = X1+DDX
         PGXTRC(PGID) = X2+DDX
         PGYBLC(PGID) = Y1+DDY
         PGYTRC(PGID) = Y2+DDY
         CALL PGVW
C
C        -- Do hardware scroll.
C
         CALL GRSCRL(NDX, NDY)
         CALL PGEBUF
      END IF
      END
