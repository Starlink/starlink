C*PGSHLS -- set color representation using HLS system
C%void cpgshls(int ci, float ch, float cl, float cs);
C+
      SUBROUTINE PGSHLS (CI, CH, CL, CS)
      INTEGER CI
      REAL    CH, CL, CS
C
C Set color representation: i.e., define the color to be
C associated with a color index.  This routine is equivalent to
C PGSCR, but the color is defined in the Hue-Lightness-Saturation
C model instead of the Red-Green-Blue model. Hue is represented
C by an angle in degrees, with red at 120, green at 240,
C and blue at 0 (or 360). Lightness ranges from 0.0 to 1.0, with black
C at lightness 0.0 and white at lightness 1.0. Saturation ranges from
C 0.0 (gray) to 1.0 (pure color). Hue is irrelevant when saturation
C is 0.0.
C
C Examples:           H     L     S        R     G     B
C     black          any   0.0   0.0      0.0   0.0   0.0
C     white          any   1.0   0.0      1.0   1.0   1.0
C     medium gray    any   0.5   0.0      0.5   0.5   0.5
C     red            120   0.5   1.0      1.0   0.0   0.0
C     yellow         180   0.5   1.0      1.0   1.0   0.0
C     pink           120   0.7   0.8      0.94  0.46  0.46
C
C Reference: SIGGRAPH Status Report of the Graphic Standards Planning
C Committee, Computer Graphics, Vol.13, No.3, Association for
C Computing Machinery, New York, NY, 1979. See also: J. D. Foley et al,
C ``Computer Graphics: Principles and Practice'', second edition,
C Addison-Wesley, 1990, section 13.3.5.
C
C Argument:
C  CI     (input)  : the color index to be defined, in the range 0-max.
C                    If the color index greater than the device
C                    maximum is specified, the call is ignored. Color
C                    index 0 applies to the background color.
C  CH     (input)  : hue, in range 0.0 to 360.0.
C  CL     (input)  : lightness, in range 0.0 to 1.0.
C  CS     (input)  : saturation, in range 0.0 to 1.0.
C--
C 9-May-1988 - new routine [TJP].
C-----------------------------------------------------------------------
      REAL CR, CG, CB
      CALL GRXRGB (CH,CL,CS,CR,CG,CB)
      CALL GRSCR(CI,CR,CG,CB)
      END
