C*GRMSG -- issue message to user
C+
      SUBROUTINE GRMSG (TEXT)
      CHARACTER*(*) TEXT
C
C Display a message on standard output.
C
C Argument:
C  TEXT (input): text of message to be printed (the string
C      may not be blank).
C--
C  8-Nov-1994 [TJP].
C-----------------------------------------------------------------------
      INTEGER   GRTRIM
C
      IF (TEXT.NE.' ') THEN
          WRITE (*, '(1X,A)') TEXT(1:GRTRIM(TEXT))
      END IF
      END
