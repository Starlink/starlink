C*PGLAB -- write labels for x-axis, y-axis, and top of plot
C%void cpglab(const char *xlbl, const char *ylbl, const char *toplbl);
C+
      SUBROUTINE PGLAB (XLBL, YLBL, TOPLBL)
      CHARACTER*(*) XLBL, YLBL, TOPLBL
C
C Write labels outside the viewport. This routine is a simple
C interface to PGMTXT, which should be used if PGLAB is inadequate.
C
C Arguments:
C  XLBL   (input) : a label for the x-axis (centered below the
C                   viewport).
C  YLBL   (input) : a label for the y-axis (centered to the left
C                   of the viewport, drawn vertically).
C  TOPLBL (input) : a label for the entire plot (centered above the
C                   viewport).
C--
C 11-May-1990 - remove unnecessary include - TJP.
C-----------------------------------------------------------------------
      CALL PGBBUF
      CALL PGMTXT('T', 2.0, 0.5, 0.5, TOPLBL)
      CALL PGMTXT('B', 3.2, 0.5, 0.5, XLBL)
      CALL PGMTXT('L', 2.2, 0.5, 0.5, YLBL)
      CALL PGEBUF
      END
