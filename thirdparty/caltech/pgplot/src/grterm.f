
C*GRTERM -- flush buffer to output device
C+
      SUBROUTINE GRTERM
C
C GRPCKG: flush the buffer associated with the current plot. GRTERM
C should be called only when it is necessary to make sure that all the
C graphics created up to this point in the program are visible on the
C device, e.g., before beginning a dialog with the user. GRTERM has no
C effect on hardcopy devices.
C
C Arguments: none.
C--
C  6-Oct-1983
C 29-Jan-1985 - add HP2648 device [KS/TJP].
C 31-Dec-1985 - do not send CAN code to true Tek [TJP/PCP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 11-Jun-1987 - remove built-in devices [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER CHR
C
      IF (GRCIDE.GE.1) THEN
          CALL GREXEC(GRGTYP,16,RBUF,NBUF,CHR,LCHR)
      END IF
      END
