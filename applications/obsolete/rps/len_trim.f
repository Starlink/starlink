*+LEN_TRIM  Returns effective string length ignoring trailing spaces.
      INTEGER FUNCTION LEN_TRIM(STRING)
      CHARACTER STRING*(*)	!input	String
*NOTE: returns ZERO if string is blank
*-Author	Clive Page	1991-JULY-19
      INTEGER K
*
      DO K = LEN(STRING), 1, -1
          IF(STRING(K:K) .NE. ' ') GO TO 200
      END DO
      K = 0
200   CONTINUE
      LEN_TRIM = K
      END
