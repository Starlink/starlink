	LOGICAL FUNCTION NY(LU)
        IMPLICIT NONE
        INTEGER LU
	LOGICAL INY
        CHARACTER*(1) ANSWER
C
10	READ (LU,13) ANSWER
13	FORMAT (A1)
C
        NY = .FALSE.
        IF ( ANSWER .EQ. 'n' .OR. ANSWER .EQ. 'N') THEN
          NY = .FALSE.
          GOTO 900
        ENDIF

        IF ( ANSWER .EQ. 'y' .OR. ANSWER .EQ. 'Y') THEN
          NY = .TRUE.
          GOTO 900
        ENDIF

	WRITE (6,23)
23	FORMAT (' A simple "N" or "Y" will suffice . . .')
	GOTO 10
C
900	CONTINUE
        RETURN
	END
