
C*GRTRML -- get name of user's terminal (UNIX)
C+
      SUBROUTINE GRTRML(STRING, L)
      CHARACTER*(*) STRING
      INTEGER L
C
C Return the device name of the user's terminal, if any. In Sun/Convex-UNIX,
C the name of the terminal is always /dev/tty.
C
C Arguments:
C  STRING : receives the terminal name, truncated or extended with
C           blanks as necessary.
C  L      : receives the number of characters in STRING, excluding
C           trailing blanks. If there is not attached terminal, 
C           zero is returned.
C--
C 19-Jan-1988
C-----------------------------------------------------------------------
      STRING = '/dev/tty'
      L = MIN(LEN(STRING),8)
      END
