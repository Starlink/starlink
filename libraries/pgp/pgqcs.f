C*PGQCS  -- inquire character height in a variety of units
C%void cpgqcs(int units, float *xch, float *ych);
C+
      SUBROUTINE PGQCS(UNITS, XCH, YCH)
      INTEGER UNITS
      REAL XCH, YCH
C
C Return the current PGPLOT character height in a variety of units.
C This routine provides facilities that are not available via PGQCH.
C Use PGQCS if the character height is required in units other than
C those used in PGSCH.
C
C The PGPLOT "character height" is a dimension that scales with the
C size of the view surface and with the scale-factor specified with
C routine PGSCH. The default value is 1/40th of the height or width
C of the view surface (whichever is less); this value is then
C multiplied by the scale-factor supplied with PGSCH. Note that it
C is a nominal height only; the actual character size depends on the
C font and is usually somewhat smaller.
C
C Arguments:
C  UNITS  (input)  : Used to specify the units of the output value:
C                    UNITS = 0 : normalized device coordinates
C                    UNITS = 1 : inches
C                    UNITS = 2 : millimeters
C                    UNITS = 3 : pixels
C                    UNITS = 4 : world coordinates
C                    Other values give an error message, and are
C                    treated as 0.
C  XCH    (output) : The character height for text written with a
C                    vertical baseline.
C  YCH    (output) : The character height for text written with
C                    a horizontal baseline (the usual case).
C
C The character height is returned in both XCH and YCH.
C
C If UNITS=1 or UNITS=2, XCH and YCH both receive the same value.
C
C If UNITS=3, XCH receives the height in horizontal pixel units, and YCH
C receives the height in vertical pixel units; on devices for which the
C pixels are not square, XCH and YCH will be different.
C
C If UNITS=4, XCH receives the height in horizontal world coordinates
C (as used for the x-axis), and YCH receives the height in vertical
C world coordinates (as used for the y-axis). Unless special care has
C been taken to achive equal world-coordinate scales on both axes, the
C values of XCH and YCH will be different.
C
C If UNITS=0, XCH receives the character height as a fraction of the
C horizontal dimension of the view surface, and YCH receives the
C character height as a fraction of the vertical dimension of the view
C surface.
C--
C 15-Oct-1992 - new routine [MCS].
C  4-Dec-1992 - added more explanation [TJP].
C  5-Sep-1995 - add UNITS=4; correct error for non-square pixels [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL PGNOTO
      REAL RATIO
C                                        Conversion factor inches -> mm
      REAL INTOMM
      PARAMETER (INTOMM=25.4)
C-----------------------------------------------------------------------
      IF (PGNOTO('PGQCS')) RETURN
      RATIO = PGYPIN(PGID)/PGXPIN(PGID)
C
C Return the character height in the required units.
C
C                                        Inches.
      IF (UNITS.EQ.1) THEN
        XCH = PGYSP(PGID)/PGXPIN(PGID)
        YCH = XCH
C                                        Millimeters.
      ELSE IF (UNITS.EQ.2) THEN
        XCH = PGYSP(PGID)/PGXPIN(PGID) * INTOMM
        YCH = XCH
C                                        Pixels.
      ELSE IF (UNITS.EQ.3) THEN
        XCH = PGYSP(PGID)
        YCH = PGYSP(PGID)*RATIO
C                                        World coordinates.
      ELSE IF (UNITS.EQ.4) THEN
         XCH = PGYSP(PGID)/PGXSCL(PGID)
         YCH = PGYSP(PGID)*RATIO/PGYSCL(PGID)
C                                        Normalized device coords, or
C                                        unknown.
      ELSE
        XCH = PGYSP(PGID)/PGXSZ(PGID)
        YCH = PGYSP(PGID)*RATIO/PGYSZ(PGID)
        IF (UNITS.NE.0)
     :       CALL GRWARN('Invalid "UNITS" argument in PGQCS.')
      END IF
      END
