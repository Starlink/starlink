	INTEGER FUNCTION LNBLNK(STRING)

	CHARACTER *(*) STRING
        INTEGER K

        K = LEN(STRING)

*    Look for NULL and blank fill if necessary

        DO 10 I = 1,K
           IF ( ICHAR(STRING(I:I)) .EQ. 0) THEN
              STRING(I:K) = ' '
           ENDIF
   10   CONTINUE

*   Now clip trailing blanks

        DO  20  I = K,1,-1
           IF (STRING(I:I) .NE. ' ') THEN
		LNBLNK = I
                RETURN
           ENDIF
   20   CONTINUE

        LNBLNK = 0
        RETURN
        END
