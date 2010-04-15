!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      SUBROUTINE XDCODE(COMMND,STRING,MINPAR,MAXPAR,PARAS,PROMPT,OK)
C
C SUBROutine to decode character string STRING into a set of real values
C PARAS with MINPAR being the minimum no. allowed and MAXPAR the maximum no.
C If insufficient parameters are found they are prompted for up to MINPAR.
C Excess parameters are ignored.  If any parameter is unreadable OK is set
C to .FALSE., otherwise it is returned as .TRUE.
C Any messages are prefixed with character string COMMND as an identifier.
C
      INCLUDE 'SAE_PAR'
      CHARACTER COMMND*(*), STRING*(*), PROMPT*(*)
      INTEGER MINPAR, MAXPAR
      INTEGER SLEN
      REAL PARAS(1)
      LOGICAL BEEP
      CHARACTER*1 BLEEP
      COMMON /BEEP/ BEEP
      COMMON /BLEEP/ BLEEP
      LOGICAL OK

*  External References:
      INTEGER DPARSE
C
C Declare local variables
C
      INTEGER DELIM, I, FIRST, LAST
      INTEGER JJ
      CHARACTER SUBSTR*80
      REAL VALUE
      OK=.TRUE.
C
C Decode first MINPAR parameters enquiring for value if not enough paras.
C
      LAST=0
      DO 1 I=1,MINPAR
        FIRST=LAST+1
        LAST=INDEX(PROMPT(FIRST:),' ')+LAST
        CALL SSTRIP(STRING)
        JJ = DPARSE(STRING,SUBSTR,' ',DELIM)
C       IF ( SUBSTR.EQ.' ' ) THEN
        DO WHILE (SUBSTR.EQ.' ')

          STATUS = SAI__OK
          CALL RDSTR( COMMND, PROMPT( FIRST : LAST ), ' ',
     :                SUBSTR, STATUS )
          IF( STATUS .NE. SAI__OK ) THEN
             CALL ERR_FLUSH( STATUS )
             OK = .FALSE.
             RETURN
          END IF

        ENDDO
C       END IF
        CALL SSTRIP (SUBSTR)
        CALL TOREAL(SUBSTR(1:INDEX(SUBSTR,' ')),VALUE,OK)
        CALL XSPRMPT(SUBSTR,COMMND,PROMPT(FIRST:LAST))
        IF ( .NOT.OK ) THEN
          WRITE (*,
     :    '(''   '',A,'':  parameter no.'',I2,'' unreadable'',A)')
     :    COMMND(1:SLEN(COMMND)),I,BLEEP
          GOTO 99999
        END IF
        PARAS(I)=VALUE
    1 CONTINUE
C
C Decode any remaining parameters
C
      I = MINPAR
10000 CONTINUE
       I = I+1
      IF ( I.LE.MAXPAR .AND. STRING.NE.' ' ) THEN
        CALL SSTRIP(STRING)
        JJ = DPARSE(STRING,SUBSTR,' ',DELIM)
        CALL TOREAL(SUBSTR,VALUE,OK)
        IF ( .NOT.OK ) THEN
          WRITE (*,
     :    '(''   '',A,'':  parameter no.'',I2,'' unreadable'',A)')
     :     COMMND(1:SLEN(COMMND)),I,BLEEP
          GOTO 99999
        END IF
        PARAS(I)=VALUE
        GOTO 10000
      END IF
C
C Check for superfluous parameters
C
      IF ( STRING.NE.' ' ) THEN
        IF
     :  (COMMND.NE.'SNIP' .AND.
     :  COMMND.NE.'CREGS' .AND.
     :  COMMND.NE.'PM') THEN
           WRITE (*,
     :     '(''   '',A,'':   superfluous parameters ignored'',A)')
     :     COMMND(1:SLEN(COMMND)),BLEEP
        ENDIF
      END IF
99999 CONTINUE
      END
