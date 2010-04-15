**==UPDATE.FOR
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE UPDATE(STRING,TEXT,LLENT,MODE,OK,REAL,STATUS)
!
!
       IMPLICIT NONE
       INCLUDE 'SAE_PAR'
       INTEGER STATUS
!
!
       CHARACTER*80 STRING, TEXT, TEMP
       INTEGER LLENS, LLENT, LLENST
       INTEGER SLEN
       INTEGER IX
       INTEGER MODE
       REAL REAL, NUVAL
       LOGICAL OK
!
       IF( STATUS .NE. SAI__OK ) RETURN
       OK = .TRUE.
!
!
       LLENST = SLEN(STRING)
       IF (STRING(LLENST:LLENST).EQ.'X') LLENST = LLENST - 1
       LLENST = LLENST + 3
  100  CONTINUE
       TEMP = ' '
       NUVAL = 0.0
       TEMP = ' '
       IF (MODE.EQ.0) THEN
          CALL RDSTR( 'NEBCONT', STRING(:20), TEXT(:LLENT), TEMP,
     :                STATUS )
          IF( STATUS .NE. SAI__OK ) THEN
             OK = .FALSE.
             RETURN
          END IF

       ELSE
          STRING(20:20) = ':'
          WRITE (*,'(A,A)') STRING(1:LLENST), TEXT(1:LLENT)
          STRING = STRING(1:LLENST-3)
          RETURN
       ENDIF

       STRING = STRING(1:LLENST-3)
       CALL SSTRIP(TEMP)
       CALL DTOUPP(TEMP)
       IF (TEMP(1:1).EQ.'!') THEN
          WRITE (*,'(''   NEBCONT:  user abort'')')
          OK = .FALSE.
          RETURN
       ELSE
          NUVAL = 0.0
          CALL DECODE('UPDATE',TEMP,0,1,NUVAL,' ',OK)
          IF (.NOT.OK) THEN
             WRITE (*,
     :'(''   NEBCONT:  error reading parameter;'',       ''  please re-e
     :nter'')')
             OK = .TRUE.
             GOTO 100
          ENDIF
       ENDIF
!
       IF (NUVAL.NE.0.0) REAL = NUVAL
!
!
       RETURN
       END
