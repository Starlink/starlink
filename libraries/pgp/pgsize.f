C
      SUBROUTINE PGSIZE (WIDTH, HEIGHT, SHIFTX, SHIFTY, DUMMY)
C
C PGPLOT (obsolete routine; use PGVSIZ in preference): Change the
C size and position of the viewport.
C
C Arguments:
C
C WIDTH (input, real) : width of viewport in inches.
C HEIGHT (input, real) : height of viewport in inches.
C SHIFTX (input, real) : horizontal offset of bottom left corner
C       from blc of page or panel, in inches.
C SHIFTY (input, real) : vertical offset of bottom left corner
C       from blc of page or panel, in inches.
C DUMMY (input, real) : reserved for future use (must be 0.0).
C--
C 13-Dec-1990  Make errors non-fatal [TJP].
C-----------------------------------------------------------------------
      REAL     WIDTH,HEIGHT,SHIFTX,SHIFTY,DUMMY
C
      IF (WIDTH.LE.0.0 .OR. HEIGHT.LE.0.0 .OR. DUMMY.NE.0.0) THEN
          CALL GRWARN('PGSIZE ignored: invalid arguments')
          RETURN
      END IF
C
      CALL PGVSIZ(SHIFTX, SHIFTX+WIDTH, SHIFTY, SHIFTY+HEIGHT)
      END
