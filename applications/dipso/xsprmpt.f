       SUBROUTINE XSPRMPT
     : (SUBSTR, COMMND, PROMPT)

       IMPLICIT NONE

       CHARACTER*(*) SUBSTR, COMMND, PROMPT
       LOGICAL BEEP
       CHARACTER*1 BLEEP
       COMMON /BLEEP/ BLEEP
       COMMON /BEEP/ BEEP

       INTEGER JNDEX
       INTEGER SLEN
         CALL SSTRIP(SUBSTR)
         JNDEX = INDEX(SUBSTR,' ')
         JNDEX = MAX(JNDEX,1)
         SUBSTR(1:) = SUBSTR(JNDEX:)
         IF (SUBSTR.NE.' ') THEN
            WRITE (*,
     :      '(''   '',A,'':  '',A,''- superfluous parameters'',
     :      '' ignored'',A)')
     :      COMMND(1:SLEN(COMMND)), PROMPT(1:SLEN(PROMPT)), BLEEP
         ENDIF

         RETURN
         END
