*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
       SUBROUTINE RECORD(COMND,RESTRING,OK)

       IMPLICIT NONE

       CHARACTER*(*) COMND
       CHARACTER*(*) RESTRING
       LOGICAL OK

       CHARACTER*200 TSTRING
       CHARACTER*20 QUOSTR
       INTEGER MCLOSE, NCLOSE, NTEMP, SLEN

       CALL SSTRIP(COMND)
       IF (COMND(1:6).EQ.'RECORD') THEN
          COMND(1:) = COMND(7:)
          CALL SSTRIP(COMND)
          IF (COMND(1:1).EQ.' ' .OR. COMND(1:1).EQ.',') THEN
             RESTRING = ' '
             GOTO 100
          ELSEIF (COMND(1:1).NE.'"') THEN
             WRITE (*,'(''   RECORD:  no opening quotes'')')
             OK = .FALSE.
             GOTO 100
          ELSE
             QUOSTR = ' '
             DO 20 WHILE (COMND(1:1).EQ.'"')
                QUOSTR(1:) = '"'//QUOSTR
                COMND(1:) = COMND(2:)
   20        CONTINUE
             NCLOSE = INDEX(COMND,QUOSTR(1:SLEN(QUOSTR)))
             MCLOSE = NCLOSE + SLEN(QUOSTR)
             DO 40 WHILE (COMND(MCLOSE:MCLOSE).EQ.'"')
                NCLOSE = NCLOSE + 1
                MCLOSE = MCLOSE + 1
   40        CONTINUE
             TSTRING = COMND(1:NCLOSE-1)
             CALL DTOUPP(TSTRING)
             IF (NCLOSE.EQ.0) THEN
                WRITE (*,'(''   RECORD:  unmatched delimiters'')')
                OK = .FALSE.
                GOTO 100
             ENDIF
             NTEMP = INDEX(TSTRING(1:NCLOSE),'RECORD')
             IF (NTEMP.NE.0) THEN
                WRITE (*,
     :          '(''   RECORD:  string may not contain RECORD'',
     :          '' command'')')
                OK = .FALSE.
                GOTO 100
             ENDIF
             NTEMP = INDEX(TSTRING(1:NCLOSE),'REPLAY')
             IF (NTEMP.NE.0) THEN
                WRITE (*,
     :          '(''   RECORD:  string may not contain'',
     :          '' REPLAY command'')')
                OK = .FALSE.
                GOTO 100
             ENDIF

             RESTRING = COMND(1:NCLOSE-1)
             COMND(1:) = COMND(NCLOSE:)
             DO 60 WHILE (COMND(1:1).EQ.'"')
                COMND(1:) = COMND(2:)
   60        CONTINUE

          ENDIF
       ENDIF

  100  CONTINUE

       END


