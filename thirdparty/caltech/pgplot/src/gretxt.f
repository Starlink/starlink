
C*GRETXT -- erase text from graphics screen
C+
      SUBROUTINE GRETXT
C
C GRPCKG: Erase the text screen.  Some graphics devices have
C two superimposed view surfaces, of which one is used for graphics and
C the other for alphanumeric text.  This routine erases the text
C view surface without affecting the graphics view surface. It does
C nothing if there is no text view surface associated with the device.
C
C Arguments: none.
C--
C (1-Feb-1983)
C 16-Oct-1984 - add ID100 device [RSS/TJP].
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*1   CHR
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
C
      IF (GRCIDE.GE.1) THEN
          CALL GREXEC(GRGTYP,18,RBUF,NBUF,CHR,LCHR)
      END IF
C
      END
