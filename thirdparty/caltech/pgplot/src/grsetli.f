C*GRSETLI -- *obsolete routine*
C+
      SUBROUTINE GRSETLI (IN)
C
C GRPCKG: Set the line intensity for subsequent plotting on the current
C device. *** OBSOLETE ROUTINE *** Intensity is now set with GRSCI
C and GRSCR. For compatibility, GRSETLI now sets color zero if its
C argument is 0, and resets the previous color if its argument is
C non-zero.
C
C Argument:
C
C IN (integer, input): the intensity to be used for subsequent
C       plotting on the current device (in range 0-3).
C--
C 11-Apr-1983 - [TJP].
C 12-Jul-1984 - modify to call GRSCI [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  IN, OLDCOL(GRIMAX)
      DATA     OLDCOL /GRIMAX*1/
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSETLI - no graphics device is active.')
      ELSE IF (IN.EQ.0) THEN
          OLDCOL(GRCIDE) = GRCCOL(GRCIDE)
          CALL GRSCI(0)
      ELSE
          CALL GRSCI(OLDCOL(GRCIDE))
      END IF
      END
