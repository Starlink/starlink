C*GRWARN -- issue warning message to user
C+
      SUBROUTINE GRWARN (TEXT)
      CHARACTER*(*) TEXT
C
C Report a warning message on standard output, with prefix "%PGPLOT, ".
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C  8-Nov-1994 [TJP]
C-----------------------------------------------------------------------
      INTEGER   GRTRIM
C
      IF (TEXT.NE.' ') THEN
          WRITE (*, '(1X,2A)') '%PGPLOT, ', TEXT(1:GRTRIM(TEXT))
      END IF
      END
