C*PGQNDT -- inquire number of available device types
C%void cpgqndt(int *n);
C+
      SUBROUTINE PGQNDT(N)
      INTEGER N
C
C Return the number of available device types. This routine is
C usually used in conjunction with PGQDT to get a list of the
C available device types.
C
C Arguments:
C  N      (output) : the number of available device types.
C--
C 17-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      INTEGER NBUF, LCHR
      REAL RBUF(2)
      CHARACTER CHR
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
C Find number of device types.
C
      CALL GREXEC(0, 0, RBUF, NBUF, CHR, LCHR)
      N = NINT(RBUF(1))
C
      END
