C*PGSCI -- set color index
C%void cpgsci(int ci);
C+
      SUBROUTINE PGSCI (CI)
      INTEGER  CI
C
C Set the Color Index for subsequent plotting, if the output device
C permits this. The default color index is 1, usually white on a black
C background for video displays or black on a white background for
C printer plots. The color index is an integer in the range 0 to a
C device-dependent maximum. Color index 0 corresponds to the background
C color; lines may be "erased" by overwriting them with color index 0
C (if the device permits this).
C
C If the requested color index is not available on the selected device,
C color index 1 will be substituted.
C
C The assignment of colors to color indices can be changed with
C subroutine PGSCR (set color representation).  Color indices 0-15
C have predefined color representations (see the PGPLOT manual), but
C these may be changed with PGSCR.  Color indices above 15  have no
C predefined representations: if these indices are used, PGSCR must
C be called to define the representation.
C
C Argument:
C  CI     (input)  : the color index to be used for subsequent plotting
C                    on the current device (in range 0-max). If the
C                    index exceeds the device-dependent maximum, the
C                    default color index (1) is used.
C--
C 26-Sep-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCI')) RETURN
      CALL GRSCI(CI)
      END
