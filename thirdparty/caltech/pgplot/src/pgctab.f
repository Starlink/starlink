C*PGCTAB -- install the color table to be used by PGIMAG
C%void cpgctab(const float *l, const float *r, const float *g, \
C% const float *b, int nc, float contra, float bright);
C+
      SUBROUTINE PGCTAB(L, R, G, B, NC, CONTRA, BRIGHT)
      INTEGER NC
      REAL    L(NC), R(NC), G(NC), B(NC), CONTRA, BRIGHT
C
C Use the given color table to change the color representations of
C all color indexes marked for use by PGIMAG. To change which
C color indexes are thus marked, call PGSCIR before calling PGCTAB
C or PGIMAG. On devices that can change the color representations
C of previously plotted graphics, PGCTAB will also change the colors
C of existing graphics that were plotted with the marked color
C indexes. This feature can then be combined with PGBAND to
C interactively manipulate the displayed colors of data previously
C plotted with PGIMAG.
C
C Limitations:
C  1. Some devices do not propagate color representation changes
C     to previously drawn graphics.
C  2. Some devices ignore requests to change color representations.
C  3. The appearance of specific color representations on grey-scale
C     devices is device-dependent.
C
C Notes:
C  To reverse the sense of a color table, change the chosen contrast
C  and brightness to -CONTRA and 1-BRIGHT.
C
C  In the following, the term 'color table' refers to the input
C  L,R,G,B arrays, whereas 'color ramp' refers to the resulting
C  ramp of colors that would be seen with PGWEDG.
C
C Arguments:
C  L      (input)  : An array of NC normalized ramp-intensity levels
C                    corresponding to the RGB primary color intensities
C                    in R(),G(),B(). Colors on the ramp are linearly
C                    interpolated from neighbouring levels.
C                    Levels must be sorted in increasing order.
C                     0.0 places a color at the beginning of the ramp.
C                     1.0 places a color at the end of the ramp.
C                    Colors outside these limits are legal, but will
C                    not be visible if CONTRA=1.0 and BRIGHT=0.5.
C  R      (input)  : An array of NC normalized red intensities.
C  G      (input)  : An array of NC normalized green intensities.
C  B      (input)  : An array of NC normalized blue intensities.
C  NC     (input)  : The number of color table entries.
C  CONTRA (input)  : The contrast of the color ramp (normally 1.0).
C                    Negative values reverse the direction of the ramp.
C  BRIGHT (input)  : The brightness of the color ramp. This is normally
C                    0.5, but can sensibly hold any value between 0.0
C                    and 1.0. Values at or beyond the latter two
C                    extremes, saturate the color ramp with the colors
C                    of the respective end of the color table.
C--
C  17-Sep-1994 - New routine [MCS].
C  14-Apr-1997 - Modified to implement a more conventional
C                interpretation of contrast and brightness [MCS].
C-----------------------------------------------------------------------
      INTEGER MININD, MAXIND, CI
      INTEGER NTOTAL, NSPAN
      INTEGER BELOW, ABOVE
      LOGICAL FORWRD
      REAL CA, CB, CIFRAC, SPAN
      REAL LEVEL
      REAL LDIFF, LFRAC
      REAL RED, GREEN, BLUE
C
C Set the minimum absolute contrast - this prevents a divide by zero.
C
      REAL MINCTR
      PARAMETER (MINCTR = 1.0/256)
C
C No colormap entries?
C
      IF(NC .EQ. 0) RETURN
C
C Determine the range of color indexes to be used.
C
      CALL PGQCIR(MININD, MAXIND)
C
C Count the total number of color indexes to be processed.
C
      NTOTAL = MAXIND - MININD + 1
C
C No definable colors?
C
      IF(NTOTAL .LT. 1 .OR. MININD .LT. 0) RETURN
C
C Prevent a divide by zero later by ensuring that CONTRA >= ABS(MINCTR).
C
      IF(ABS(CONTRA) .LT. MINCTR) THEN
        CONTRA = SIGN(MINCTR, CONTRA)
      END IF
C
C Convert contrast to the normalized stretch of the
C color table across the available color index range.
C
      SPAN = 1.0 / ABS(CONTRA)
C
C Translate from brightness and contrast to the normalized color index
C coordinates, CA and CB, at which to place the start and end of the
C color table.
C
      IF(CONTRA .GE. 0.0) THEN
        CA = 1.0 - BRIGHT * (1.0 + SPAN)
        CB = CA + SPAN
      ELSE
        CA = BRIGHT * (1.0 + SPAN)
        CB = CA - SPAN
      END IF
C
C Determine the number of color indexes spanned by the color table.
C
      NSPAN = INT(SPAN * NTOTAL)
C
C Determine the direction in which the color table should be traversed.
C
      FORWRD = CA .LE. CB
C
C Initialize the indexes at which to start searching the color table.
C
C Set the start index for traversing the table from NC to 1.
C
      BELOW = NC
C
C Set the start index for traversing the table from 1 to NC.
C
      ABOVE = 1
C
C Buffer PGPLOT commands until the color map has been completely
C installed.
C
      CALL PGBBUF
C
C Linearly interpolate the color table RGB values onto each color index.
C
      DO 1 CI=MININD, MAXIND
C
C Turn the color index into a fraction of the range MININD..MAXIND.
C
        CIFRAC = REAL(CI-MININD) / REAL(MAXIND-MININD)
C
C Determine the color table position that corresponds to color index,
C CI.
C
        IF(NSPAN .GT. 0) THEN
          LEVEL = (CIFRAC-CA) / (CB-CA)
        ELSE
          IF(CIFRAC .LE. CA) THEN
            LEVEL = 0.0
          ELSE
            LEVEL = 1.0
          END IF
        END IF
C
C Search for the indexes of the two color table entries that straddle
C LEVEL. The search algorithm assumes that values in L() are
C arranged in increasing order. This allows us to search the color table
C from the point at which the last search left off, rather than having
C to search the whole color table each time.
C
        IF(FORWRD) THEN
 2        IF(ABOVE.LE.NC .AND. L(ABOVE).LT.LEVEL) THEN
            ABOVE = ABOVE + 1
            GOTO 2
          END IF
          BELOW = ABOVE - 1
        ELSE
 3        IF(BELOW.GE.1 .AND. L(BELOW).GT.LEVEL) THEN
            BELOW = BELOW - 1
            GOTO 3
          END IF
          ABOVE = BELOW + 1
        END IF
C
C If the indexes lie outside the table, substitute the index of the
C nearest edge of the table.
C
        IF(BELOW .LT. 1) THEN
          LEVEL = 0.0
          BELOW = 1
          ABOVE = 1
        ELSE IF(ABOVE .GT. NC) THEN
          LEVEL = 1.0
          BELOW = NC
          ABOVE = NC
        END IF
C
C Linearly interpolate the primary color intensities from color table
C entries, BELOW and ABOVE.
C
        LDIFF = L(ABOVE) - L(BELOW)
        IF(LDIFF .GT. MINCTR) THEN
          LFRAC = (LEVEL - L(BELOW)) / LDIFF
        ELSE
          LFRAC = 0.0
        END IF
        RED   = R(BELOW) + (R(ABOVE) - R(BELOW)) * LFRAC
        GREEN = G(BELOW) + (G(ABOVE) - G(BELOW)) * LFRAC
        BLUE  = B(BELOW) + (B(ABOVE) - B(BELOW)) * LFRAC
C
C Intensities are only defined between 0 and 1.
C
        IF(RED   .LT. 0.0)   RED = 0.0
        IF(RED   .GT. 1.0)   RED = 1.0
        IF(GREEN .LT. 0.0) GREEN = 0.0
        IF(GREEN .GT. 1.0) GREEN = 1.0
        IF(BLUE  .LT. 0.0)  BLUE = 0.0
        IF(BLUE  .GT. 1.0)  BLUE = 1.0
C
C Install the new color representation.
C
        CALL PGSCR(CI, RED, GREEN, BLUE)
 1    CONTINUE
C
C Reveal the changed color map.
C
      CALL PGEBUF
      RETURN
      END
