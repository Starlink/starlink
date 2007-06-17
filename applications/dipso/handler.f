      INTEGER FUNCTION HANDLER(SIGARGS,MCHARGS)
      COMMON /ERRCOM/ ERROR, LUSER
      INTEGER SIGARGS(1),MCHARGS(1)
      LOGICAL ERROR, LUSER

C     INCLUDE 'SYS$LIBRARY:SIGDEF'

*  This line is executed on all operating systems.
      HANDLER = 0

*  The rest is only executed on VMS.
C     IF (.NOT.ERROR) THEN
C        IF (.NOT.LUSER) THEN
C           CALL SYS$PUTMSG(SIGARGS,,,)
C        ENDIF
C     ENDIF
C     IF ( LIB$MATCH_COND(SIGARGS(2),SS$_ACCVIO) ) THEN
C        WRITE (*,
C    :   '(''   *ERROR*   *ERROR*   *ERROR*   *ERROR*   *ERROR*''
C    :    /'' ''/
C    :     ''   The program has encountered an ACCESS VIOLATION''
C    :     /'' ''/
C    :     ''         No fixup possible;  calling Ctrl-C'',A)') 7
C        CALL CTRLC_AST
C     ENDIF
C     IF ( LIB$MATCH_COND(SIGARGS(2),SS$_ROPRAND) ) THEN
C        HANDLER=LIB$FIXUP_FLT(SIGARGS,MCHARGS)
C     END IF
C     CALL LIB$SIM_TRAP(SIGARGS,MCHARGS)
C     HANDLER=SS$_CONTINUE
C     ERROR=.TRUE.
      END
