*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
*
*
*   SUBROUTINE GETCOM2
*
*   PARSE COMMAND LINE TO GET COMMAND AND PARAMETERS
*   WHERE COMMAND IS "@" (COMMAND FILE INVOCATION)
*
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
       SUBROUTINE GETCOM2 (COMMND,SUBCMD,PARAMS,SUBCHK)
*
       CHARACTER*(*) COMMND, SUBCMD, PARAMS
       LOGICAL SUBCHK
       INTEGER SLEN, CMDLEN
       LOGICAL ALLBLANK
*
       PARAMS = ' '
       SUBCHK = .TRUE.
*
       CMDLEN = LEN( COMMND )
       CALL SSTRIP(COMMND)

       DO I = 1, CMDLEN-1
          COMMND( I : I ) = COMMND( I+1 : I+1 )
       END DO
       COMMND( CMDLEN : ) = ' '

       CALL SSTRIP(COMMND)

       DO I = CMDLEN-1, 1, -1
          COMMND( I+1 : I+1 ) = COMMND( I : I )
       END DO
       COMMND( 1 : 1 ) = '@'

*   Deal with case of no parameters

       I = INDEX(COMMND,' ')
       J = INDEX(COMMND,',')
       ALLBLANK = .TRUE.
       IF (J.EQ.0) THEN
          ALLBLANK = .FALSE.
       ELSE
          DO 5 K = I+1, J-1
             IF (COMMND(K:K).NE.' ') ALLBLANK = .FALSE.
    5     CONTINUE
       ENDIF
       IF (ALLBLANK) THEN
          SUBCMD = COMMND(1:J-1)
!!!          CALL DTOUPP(SUBCMD)
          COMMND(1:) = COMMND(J+1:)
          GOTO 30
       ENDIF

*  Parameters present

       I = INDEX(COMMND,' ')
       SUBCMD = COMMND(1:I-1)
!!!       CALL DTOUPP(SUBCMD)

       COMMND(1:) = COMMND(I+1:)

       K = 1
   10  CONTINUE

       I = INDEX(COMMND(K:),',')
       J = INDEX(COMMND(K:),'"')

*   No commas;  remaining string must be entirely command argument

       IF (I.EQ.0) THEN
          PARAMS = COMMND(1:SLEN(COMMND))
          COMMND = ' '
*   Check for matching single quotes
          IF (J.NE.0) THEN
             NQUOTES = 0
             L = 1
             M = SLEN(PARAMS)
             DO WHILE (L.LT.M)
                IF (PARAMS(L:L).EQ.'"') THEN
                   L = L+1
                   IF (PARAMS(L:L).NE.'"') NQUOTES = NQUOTES+1
                ENDIF
                L = L + 1
             ENDDO
             IF (MOD(NQUOTES,2).NE.0) THEN
                SUBCHK = .FALSE.
             ENDIF
          ENDIF
          GOTO 30
       ENDIF

       IF (J.EQ.0 .OR. J.GT.I) THEN
*   Argument string contains no single quotes
          I = I + K - 1
          PARAMS = COMMND(1:I-1)
          COMMND(1:) = COMMND(I+1:)
          GOTO 30
       ENDIF

*   Argument string contains opening double quote;  find closing d.q.

       K = J + K
   20  CONTINUE

       K = INDEX(COMMND(K:),'"') + K - 1
       IF (K.EQ.0) THEN
*   No closing quote found
          PARAMS = COMMND(1:SLEN(COMMND))//'"'
          COMMND = ' '
          SUBCHK = .FALSE.
          GOTO 30
       ELSEIF (COMMND(K+1:K+1).EQ.'"') THEN
*   Pair of double quotes; ignore
          K = K+2
          GOTO 20
       ELSE
*   Closing single quote found;  but parameter string may contain more arguments
          K = K + 1
          GO TO 10
       ENDIF

   30  CONTINUE

       END
