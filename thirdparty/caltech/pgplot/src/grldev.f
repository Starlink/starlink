C*GRLDEV -- list supported device types
C+
      SUBROUTINE GRLDEV
C
C Support routine for PGLDEV.
C
C Arguments: none
C--
C  5-Aug-1986 [AFT]
C 13-Dec-1990 Change warnings to messages [TJP].
C 18-Jan-1993 Display one per line [TJP].
C 13-Jan-1995 Change message [TJP].
C 10-Nov-1995 Ignore device types of zero length [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER I,NDEV,NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER*72 CHR
      CHARACTER*72 TEXT
C---
      CALL GRMSG('Device types available:')
C--- First obtain number of devices.
      CALL GREXEC(0,0,RBUF,NBUF,CHR,LCHR)
      NDEV=NINT(RBUF(1))
C
      DO 10 I=1,NDEV
         CALL GREXEC(I, 1,RBUF,NBUF,CHR,LCHR)
         IF (LCHR.GT.0) THEN
            TEXT(1:1) = '/'
            TEXT(2:) = CHR(:LCHR)
            CALL GRMSG(TEXT)
         END IF
 10   CONTINUE
C
      END
