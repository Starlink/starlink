C*PGSCR -- set color representation
C%void cpgscr(int ci, float cr, float cg, float cb);
C+
      SUBROUTINE PGSCR (CI, CR, CG, CB)
      INTEGER CI
      REAL    CR, CG, CB
C
C Set color representation: i.e., define the color to be
C associated with a color index.  Ignored for devices which do not
C support variable color or intensity.  Color indices 0-15
C have predefined color representations (see the PGPLOT manual), but
C these may be changed with PGSCR.  Color indices 16-maximum have no
C predefined representations: if these indices are used, PGSCR must
C be called to define the representation. On monochrome output
C devices (e.g. VT125 terminals with monochrome monitors), the
C monochrome intensity is computed from the specified Red, Green, Blue
C intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television
C systems, NTSC encoding.  Note that most devices do not have an
C infinite range of colors or monochrome intensities available;
C the nearest available color is used.  Examples: for black,
C set CR=CG=CB=0.0; for white, set CR=CG=CB=1.0; for medium gray,
C set CR=CG=CB=0.5; for medium yellow, set CR=CG=0.5, CB=0.0.
C
C Argument:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  CR     (input)  : red, green, and blue intensities,
C  CG     (input)    in range 0.0 to 1.0.
C  CB     (input)
C--
C 5-Nov-1985 - new routine [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSCR')) RETURN
      CALL GRSCR(CI,CR,CG,CB)
      END
