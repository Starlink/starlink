C*GRAREA -- define a clipping window
C+
      SUBROUTINE GRAREA (IDENT,X0,Y0,XSIZE,YSIZE)
C
C GRPCKG: Define a rectangular window in the current plotting area. All
C graphics (except characters written with GRCHAR) will be blanked
C outside this window.  The default window is the full plotting area
C defined by default or by GRSETS.
C
C Arguments:
C
C IDENT (input, integer): the plot identifier, returned by GROPEN.
C X0, Y0 (input, real): the lower left corner of the window, in absolute
C       device coordinates.
C XSIZE, YSIZE (input, real): width and height of the window in absolute
C       coordinates; if either is negative, the window will be reset to
C       the full plotting area.
C--
C  1-Feb-1983 - [TJP].
C 25-Nov-1994 - use floating-point [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER IDENT
      REAL X0, Y0, XSIZE, YSIZE
C
      CALL GRSLCT(IDENT)
C
      IF ((XSIZE.LE.0.0) .OR. (YSIZE.LE.0.0)) THEN
          GRXMIN(IDENT) = 0
          GRXMAX(IDENT) = GRXMXA(IDENT)
          GRYMIN(IDENT) = 0
          GRYMAX(IDENT) = GRYMXA(IDENT)
      ELSE
          GRXMIN(IDENT) = MAX(X0,0.0)
          GRYMIN(IDENT) = MAX(Y0,0.0)
          GRXMAX(IDENT) = MIN(XSIZE+X0,REAL(GRXMXA(IDENT)))
          GRYMAX(IDENT) = MIN(YSIZE+Y0,REAL(GRYMXA(IDENT)))
      END IF
C
      END
