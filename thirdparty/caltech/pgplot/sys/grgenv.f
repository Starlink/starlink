
C*GRGENV -- get value of PGPLOT environment parameter (Sun/Convex-UNIX)
C+
      SUBROUTINE GRGENV(NAME, VALUE, L)
      CHARACTER*(*) NAME, VALUE
      INTEGER L
C
C Return the value of a PGPLOT environment parameter. In Sun/Convex-UNIX,
C environment parameters are UNIX environment variables; e.g. parameter
C ENVOPT is environment variable PGPLOT_ENVOPT. Translation is not
C recursive and is case-sensitive.
C
C Arguments:
C  NAME   : (input) the name of the parameter to evaluate.
C  VALUE  : receives the value of the parameter, truncated or extended
C           with blanks as necessary. If the parameter is undefined,
C           a blank string is returned.
C  L      : receives the number of characters in VALUE, excluding
C           trailing blanks. If the parameter is undefined, zero is
C           returned.
C--
C 19-Jan-1988
C-----------------------------------------------------------------------
      INTEGER I, LIN
      CHARACTER*32 TEST
C
      TEST = 'PGPLOT_'//NAME
      LIN = INDEX(TEST, ' ')-1
      CALL GETENV(TEST(:LIN), VALUE)
      IF (VALUE.EQ.' ') THEN
          L = 0
      ELSE
          DO 10 I=LEN(VALUE),1,-1
              L = I
              IF (VALUE(I:I).NE.' ') GOTO 20
   10     CONTINUE
          L = 0
   20     CONTINUE
      END IF
      END
