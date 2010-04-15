!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!
!
!   SUBROUTINE GETCOM
!
!      PARSE COMMAND CHARACTER STRING TO GET COMMAND
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE GETCOM (COMMND,SUBCMD,PARAMS,DELIM,SUBCHK)
!
       CHARACTER*(*) COMMND, SUBCMD, PARAMS
       INTEGER DELIM, DPARSE, LTOKEN, DUMMY
       LOGICAL SUBCHK
!
       PARAMS = ' '
       SUBCHK = .TRUE.
!
        CALL SSTRIP(COMMND)
!
        IF (COMMND(1:1).EQ.'@') THEN
           IF (.NOT.SUBCHK) THEN
              WRITE (*,'(''   Unmatched quotes in character'',
     :        '' argument to command file'')')
           ENDIF
           CALL GETCOM2 (COMMND,SUBCMD,PARAMS,SUBCHK)
           RETURN
        ENDIF
!
        I = INDEX(COMMND,',')
        IF (I.GT.0) THEN
            I = INDEX(COMMND(1:I),'"')
        ELSE
            I = INDEX(COMMND,'"')
        ENDIF
        IF (I.GT.0) THEN
           J = INDEX(COMMND(I+1:),'"')
           IF (J.LE.0) THEN
              DO J = LEN(COMMND)-1, I, -1
                 IF (COMMND(J:J).NE.' ') THEN
                    COMMND(J+1:J+1) = '"'
                    GO TO 10
                 ENDIF
              ENDDO
              COMMND(LEN(COMMND):) = '"'
   10         CONTINUE
              SUBCHK = .FALSE.
           ENDIF
        ENDIF

        LTOKEN=DPARSE(COMMND,PARAMS,',',DELIM)
        I=INDEX(PARAMS,'"')
        IF ( I.NE.0 ) THEN
          PARAMS=PARAMS(1:I-1)//PARAMS(I+1:)
          I=INDEX(PARAMS,'"')
          IF ( I.NE.0 ) THEN
            PARAMS=PARAMS(1:I-1)//PARAMS(I+1:)
          ELSE
            PARAMS(LTOKEN:LTOKEN)=','
            I=DPARSE(COMMND,PARAMS(LTOKEN+1:),'"',DELIM)
            LTOKEN=LTOKEN+I
            DUMMY=DPARSE(COMMND,PARAMS(LTOKEN+1:),',',DELIM)
          END IF
        END IF
        DUMMY=DPARSE(PARAMS,SUBCMD,' ',I)
        CALL DTOUPP(SUBCMD)
!
       RETURN
       END

