C*PGPAP -- change the size of the view surface 
C%void cpgpap(float width, float aspect);
C+
      SUBROUTINE PGPAP (WIDTH, ASPECT)
      REAL WIDTH, ASPECT
C
C This routine changes the size of the view surface ("paper size") to a
C specified width and aspect ratio (height/width), in so far as this is
C possible on the specific device. It is always possible to obtain a
C view surface smaller than the default size; on some devices (e.g.,
C printers that print on roll or fan-feed paper) it is possible to 
C obtain a view surface larger than the default.
C 
C This routine should be called either immediately after PGBEG or
C immediately before PGPAGE. The new size applies to all subsequent
C images until the next call to PGPAP.
C
C Arguments:
C  WIDTH  (input)  : the requested width of the view surface in inches;
C                    if WIDTH=0.0, PGPAP will obtain the largest view
C                    surface available consistent with argument ASPECT.
C                    (1 inch = 25.4 mm.)
C  ASPECT (input)  : the aspect ratio (height/width) of the view
C                    surface; e.g., ASPECT=1.0 gives a square view
C                    surface, ASPECT=0.618 gives a horizontal
C                    rectangle, ASPECT=1.618 gives a vertical rectangle.
C--
C (22-Apr-1983; bug fixed 7-Jun-1988)
C  6-Oct-1990 Modified to work correctly on interactive devices.
C 13-Dec-1990 Make errors non-fatal [TJP].
C 14-Sep-1994 Fix bug to do with drivers changing view surface size.
C-----------------------------------------------------------------------
      INCLUDE  'pgplot.inc'
      LOGICAL  PGNOTO
      REAL     HDEF, HMAX, HREQ, WDEF, WMAX, WREQ
      REAL     XSMAX, YSMAX, XSZ, YSZ
C
      IF (PGNOTO('PGPAP'))  RETURN
      IF (WIDTH.LT.0.0 .OR. ASPECT.LE.0.0) THEN
          CALL GRWARN('PGPAP ignored: invalid arguments')
          RETURN
      END IF
C
      PGPFIX(PGID) = .TRUE.
C     -- Find default size WDEF, HDEF and maximum size WMAX, HMAX
C        of view surface (inches)
      CALL GRSIZE(PGID,XSZ,YSZ,XSMAX,YSMAX,
     1            PGXPIN(PGID),PGYPIN(PGID))
      WDEF = XSZ/PGXPIN(PGID)
      HDEF = YSZ/PGYPIN(PGID)
      WMAX = XSMAX/PGXPIN(PGID)
      HMAX = YSMAX/PGYPIN(PGID)
C     -- Find desired size WREQ, HREQ of view surface (inches)
      IF (WIDTH.NE.0.0) THEN
          WREQ = WIDTH
          HREQ = WIDTH*ASPECT
      ELSE
          WREQ = WDEF
          HREQ = WDEF*ASPECT
          IF (HREQ.GT.HDEF) THEN
              WREQ = HDEF/ASPECT
              HREQ = HDEF
          END IF
      END IF
C     -- Scale the requested view surface to fit the maximum
C        dimensions
      IF (WMAX.GT.0.0 .AND. WREQ.GT.WMAX) THEN
          WREQ = WMAX
          HREQ = WMAX*ASPECT
      END IF
      IF (HMAX.GT.0.0 .AND. HREQ.GT.HMAX) THEN
          WREQ = HMAX/ASPECT
          HREQ = HMAX
      END IF
C     -- Establish the new view surface dimensions
      XSZ = WREQ*PGXPIN(PGID)
      YSZ = HREQ*PGYPIN(PGID)
      CALL GRSETS(PGID,XSZ,YSZ)
      PGXSZ(PGID) = XSZ/PGNX(PGID)
      PGYSZ(PGID) = YSZ/PGNY(PGID)
      PGNXC(PGID) = PGNX(PGID)
      PGNYC(PGID) = PGNY(PGID)
      CALL PGSCH(1.0)
      CALL PGVSTD
      END
