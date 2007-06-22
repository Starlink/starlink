	FUNCTION NY(LU)
	BYTE INY
C
10	READ (LU,13) INY
13	FORMAT (A1)
C
	IF (INY.EQ.'N'.OR.INY.EQ.'Y') GOTO 900
C
        IF ( INY .EQ. 'n' ) THEN
          INY = 'N'
          GOTO 900
        ENDIF
 
        IF ( INY .EQ. 'y' ) THEN
          INY = 'Y'
          GOTO 900
        ENDIF
 
	WRITE (6,23)
23	FORMAT (' A simple "N" or "Y" will suffice . . .')
	GOTO 10
C
900	NY = INY
	RETURN
	END
