C*GRSLW -- set line width
C+
      SUBROUTINE GRSLW (IW)
      INTEGER IW
C
C GRPCKG: Set the line width for subsequent plotting on the current
C device. If the hardware does not support thick lines, they are
C simulated by tracing each line with multiple strokes offset in the
C direction perpendicular to the line. The line width is specified by
C the number of strokes to be used, which must be in the range 1-201.
C The actual line width obtained depends on the device resolution.
C If the hardware does support thick lines, the width of the line
C is approximately 0.005 inches times the value of argument IW.
C
C Argument:
C
C IW (integer, input): the number of strokes to be used for subsequent
C       plotting on the current device (in range 1-201).
C--
C  1-Feb-1983 [TJP].
C  3-Jun-1984 [TJP] - add GMFILE device.
C 28-Aug-1984 [TJP] - correct bug in GMFILE: redundant SET_LINEWIDTH
C                     commands were not being filtered out.
C 26-May-1987 [TJP] - add GREXEC support.
C 11-Jun-1987 [TJP] - remove built-in devices.
C 31-May-1989 [TJP] - increase maximum width from 21 to 201.
C  1-Sep-1994 [TJP] 
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I, ITHICK
      REAL    RBUF(1)
      INTEGER NBUF,LCHR
      CHARACTER*32 CHR
C
C Check that graphics is active.
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRSLW - no graphics device is active.')
          RETURN
      END IF
C
C Check that requested line-width is valid.
C
      I = IW
      IF (I.LT.1 .OR. I.GT.201) THEN
          CALL GRWARN('GRSLW - invalid line-width requested.')
          I = 1
      END IF
C
C Ignore the request if the linewidth is unchanged.
C
      IF (I.EQ.ABS(GRWIDT(GRCIDE))) RETURN
C
C Inquire if hardware supports thick lines.
C
      ITHICK = 0
      IF (GRGCAP(GRCIDE)(5:5).EQ.'T') ITHICK = 1
C
C For devices with hardware support of thick lines, send the
C appropriate commands to the device driver, and give the "current
C linewidth" parameter a negative value to suppress software linewidth
C emulation.
C
      IF (ITHICK.EQ.1 .AND. GRPLTD(GRCIDE)) THEN
          RBUF(1) = I
          CALL GREXEC(GRGTYP,22,RBUF,NBUF,CHR,LCHR)
      END IF
C
C Save the current linewidth.
C
      GRWIDT(GRCIDE) = I
      IF (ITHICK.EQ.1) GRWIDT(GRCIDE) = -I
C
      END
