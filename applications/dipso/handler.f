      INTEGER FUNCTION HANDLER(SIGARGS,MCHARGS)
      COMMON /ERRCOM/ ERROR, LUSER
      INTEGER SIGARGS(1),MCHARGS(1)
      LOGICAL ERROR, LUSER

D     INCLUDE 'SYS$LIBRARY:SIGDEF'

*  This line is executed on all operating systems.
      HANDLER = 0

*  The rest is only executed on VMS.
D     IF (.NOT.ERROR) THEN
D        IF (.NOT.LUSER) THEN
D           CALL SYS$PUTMSG(SIGARGS,,,)
D        ENDIF
D     ENDIF
D     IF ( LIB$MATCH_COND(SIGARGS(2),SS$_ACCVIO) ) THEN
D        WRITE (*,
D    :   '(''   *ERROR*   *ERROR*   *ERROR*   *ERROR*   *ERROR*''
D    :    /'' ''/
D    :     ''   The program has encountered an ACCESS VIOLATION''
D    :     /'' ''/
D    :     ''         No fixup possible;  calling Ctrl-C'',A)') 7
D        CALL CTRLC_AST
D     ENDIF
D     IF ( LIB$MATCH_COND(SIGARGS(2),SS$_ROPRAND) ) THEN
D        HANDLER=LIB$FIXUP_FLT(SIGARGS,MCHARGS)
D     END IF
D     CALL LIB$SIM_TRAP(SIGARGS,MCHARGS)
D     HANDLER=SS$_CONTINUE
D     ERROR=.TRUE.
      END
