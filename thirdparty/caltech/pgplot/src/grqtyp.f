C*GRQTYP -- inquire current device type
C+
      SUBROUTINE GRQTYP (TYPE,INTER)
      CHARACTER*(*) TYPE
      LOGICAL INTER
C
C GRPCKG: obtain the device type of the currently selected graphics
C device, and determine whether or not it is an interactive device.
C
C Arguments:
C
C TYPE (output, CHARACTER*(*)): receives the device type, as a
C       character string, eg 'PRINTRONIX', 'TRILOG', 'VERSATEC',
C       'TEK4010', 'TEK4014', 'GRINNELL', or 'VT125'.  The character
C       string should have a length of at least 8 to ensure that the
C       type is unique.
C INTER (output, LOGICAL): receives the value .TRUE. if the device is
C       interactive, .FALSE. otherwise.
C--
C (23-May-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C 18-Jan-1993 - return type only, not description [TJP].
C  1-Sep-1994 - get capabilities from common [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL    RBUF(6)
      INTEGER NBUF,LCHR
      CHARACTER*32 CHR
C
      IF (GRCIDE.LT.1) THEN
          CALL GRWARN('GRQTYP - no graphics device is active.')
          TYPE = 'NULL'
          INTER = .FALSE.
      ELSE
          CALL GREXEC(GRGTYP, 1,RBUF,NBUF,CHR,LCHR)
          LCHR = INDEX(CHR,' ')
          TYPE = CHR(:LCHR)
          INTER = (GRGCAP(GRCIDE)(1:1).EQ.'I')
      END IF
C
      END
