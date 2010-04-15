*  History:
*     16 Nov 1993 (hme):
*        Replace backslash in string constants with CHAR(92).
*     31 July 2000 (ajc):
*        Re-write illegal concatenation
*        Change TYPE * to PRINT *
*        Unused GEN_DELIM
*-----------------------------------------------------------------------

      SUBROUTINE GEN_INPUT (LEVEL, PROMPT, CHAX, ISTR, JDEF)

      IMPLICIT  NONE

C   Routine to prompt with character string PROMPT and receive a
C   character string from the terminal.
C   If the return character string is null or carriage return
C   CHAX is left unchanged and JDEF is set to 1, if input is CTRL/Z JDEF is
C   set to 2; else the string is returned in CHAX
C   and JDEF is set to 0. In this last case ISTR returns the length
C   of the variable CHAX.

C   The routine by default reads from a (max 2048 character) command line
C   held in CLILINE. If the command line is exhausted then a prompt is
C   delivered to the terminal, and the resulting input 'tops up' the command
C   line. Subsequent input is again taken from the command line until
C   it is once more exhausted.

C   Each call to GETCH retrieves the contents of one item of terminal input.


*     Formal parameters

      INTEGER*4 LEVEL
      CHARACTER PROMPT*(*)
      CHARACTER CHAX*(*)
      INTEGER*4 ISTR
      INTEGER*4 JDEF

*     Local variables

      LOGICAL   EMPTY
      LOGICAL   QUERY
      INTEGER*4 ISPMAX
      INTEGER*4 ICLIST, ICLIFIN
      INTEGER*4 IBEG
      INTEGER*4 IEND
      INTEGER*4 IST, IFIN
      INTEGER*4 INEXT
      INTEGER*4 IERR
      INTEGER*4 NCH
      INTEGER*4 LUN
      INTEGER   IDL
      CHARACTER ID*80
      CHARACTER STRING*256
      CHARACTER PSTRING*512

*     Functions

      INTEGER*4 GEN_ILEN
      INTEGER*4 GEN_ICHTOT
      INTEGER*4 STACK_POINTER

*     Include files

      INCLUDE  'CLI_STACK.INC'

*  OK? Go...
      ISPMAX = ISP
      EMPTY  = .TRUE.
      QUERY  = .FALSE.

C  If CLI line not empty read from CLI line.

  100 IBEG = ICLI(1,ISP)
      IEND = ICLI(2,ISP)

      IF (IEND.GE.IBEG .AND. IBEG.GT.0) THEN

        IERR    = 0
        ICLIST  = GEN_ICHTOT (ISP-1) + IBEG
        ICLIFIN = GEN_ICHTOT (ISP)
*       Read from the command line and update the pointer to after the
*       terminating delimiter sequence

CD      Print *,'Calling GETIT3 with string = ',cliline(iclist:iclifin)
CD      Print *,'stack pars iclist, iclifin, icli(1,isp), icli(2,isp)'
CD      Print *,            iclist, iclifin, icli(1,isp), icli(2,isp)

        CALL GEN_GETIT3 (CLILINE(ICLIST:ICLIFIN),
     &                   LEVEL, IST, IFIN, INEXT, IERR)

CD      Print *,'Returned from GETIT3 with string = ',
CD   &          cliline(iclist+ist-1:iclist+ifin-1)
CD      Print *,'ist, ifin, inext = ', ist, ifin, inext

        ICLI(1,ISP) = IBEG + INEXT - 1
CD      Print *,'icli(1,isp) updated: ', icli(1,isp)

*       Error in getting next string
        IF (IERR.EQ.1 .OR. IERR.GT.2) THEN
          JDEF = -1
          CALL RESET_STK_PT                ! Abandon input here.
          DO WHILE (STACK_POINTER().NE.0)
            CALL GEN_UNWIND (1)
          END DO
          CALL INIT_CLI
          GO TO 200

*       Otherwise OK
        ELSE IF (IERR.EQ.0) THEN
          QUERY  = .FALSE.
          EMPTY  = .FALSE.
          STRING = CLILINE(ICLIST+IST-1:ICLIST+IFIN-1)
          ISTR   = GEN_ILEN (STRING)

          IF (ISTR.GE.1) THEN
            IF (STRING(1:1).EQ.'?') THEN              ! Force query to next level
              QUERY = .TRUE.
            ELSE IF (  STRING(1:2).EQ.'^Z'
     &            .OR. STRING(1:2).EQ.'^z'
     &            .OR. STRING(1:2).EQ.'^D'
     &            .OR. STRING(1:2).EQ.'^d') THEN
              JDEF = 2
CD            PRINT *, '<<EOF implied>>'
              GO TO 200
            END IF
          ELSE
            JDEF = 1                                  ! Default
            GO TO 200
          END IF
        END IF
      END IF

C  Need more data? ( EMPTY must still be TRUE if IERR=2 )

      IF (EMPTY) THEN
        JDEF = -1

        DO WHILE (JDEF.LT.0)
          CALL GEN_GETMORE (PROMPT, STRING, JDEF)
        END DO

*       Write the new line to the journal file, and then act: JDEF=0 means
*       that we received real input; JDEF=1 means that we are to accept the
*       default string (so return to calling routine); JDEF=2 means that we
*       got an EOF from the command file, so unwind the stack and read from
*       the calling command file instead.

        IF (JDEF.EQ.0) THEN
          IF (ISP.EQ.0) CALL GEN_JLINE (STRING)
        ELSE IF (JDEF.EQ.1)  THEN
          IF (ISP.EQ.0) CALL GEN_JLINE (STRING)
          GO TO 200
        ELSE IF (JDEF.EQ.2)  THEN
          IF (ISP.EQ.0) CALL GEN_JLINE (CHAR(92))
          IF (ISP.NE.0)   THEN
            CALL GEN_UNWIND (1)
            ISPMAX = ISP
            GO TO 100
          ELSE
            GO TO 200
          END IF
        END IF

        CALL REFRESH_CLI (STRING)

        IF (GEN_ILEN (CLILINE(GEN_ICHTOT(ISP-1)+1:)).NE.0)   GO TO 100
        ISTR = GEN_ILEN(STRING)

      ELSE IF(QUERY) THEN

*       If we read a '?' then query higher level routine for value (QUERY=.T.)?
*       Only if we are not already at terminal level...
        IF(ISP.NE.0) THEN
          CALL GEN_UNWIND(0)
          GO TO 100

*       ... prompt with modified string, to show that it is "immediate" mode
        ELSE

          JDEF = -1
          IF (ISPMAX.NE.0)   THEN
            INQUIRE (UNIT=ICLI(3,ISPMAX), NAME=ID)
            ID = ID(INDEX(ID,']')+1:)
            ID = ID(:INDEX(ID,'.')-1)
            ID = ID(:GEN_ILEN(ID))//'>'
          ELSE
            ID = 'SPECX >'
          END IF

*         Then go back to the terminal for more input, but note that
*         this cannot be inserted into the command line memory, since
*         there is already more stuff there to cover subsequent questions.

          DO WHILE(JDEF.LT.0)
            IDL = GEN_ILEN(ID)
            IF (PROMPT(1:1).NE.'"')  THEN
              PSTRING = ID(:IDL+1)
              PSTRING(IDL+2:) = PROMPT
              CALL GEN_GETMORE (PSTRING, STRING, JDEF)
            ELSE
              PSTRING = PROMPT(:1)
              PSTRING(2:3) = ''' '
              PSTRING(4:) = ID(:IDL)
              PSTRING(IDL+4:) = ''',/'
              PSTRING(IDL+6:) = PROMPT(2:)
              CALL GEN_GETMORE (PSTRING, STRING, JDEF)
            END IF
          END DO

*         Journal the new line

          IF (JDEF.EQ.0) THEN
            IF (ISP.EQ.0) CALL GEN_JLINE ('!'//STRING)
          ELSE IF(JDEF.EQ.1) THEN
            IF (ISP.EQ.0) CALL GEN_JLINE ('!'//STRING)
            GO TO 200
          ELSE IF(JDEF.EQ.2) THEN
            IF (ISP.EQ.0) CALL GEN_JLINE ('!'//CHAR(92))
            GO TO 200
          END IF

        END IF
        ISTR = GEN_ILEN (STRING)

      END IF

C  Otherwise STRING just read is the required item

      IF (ISTR.EQ.0)   THEN
        JDEF = 1
      ELSE
        NCH  = MIN0(LEN(CHAX),ISTR)
        CHAX = STRING(:NCH) // ' '
        JDEF = 0
      END IF

C  Before return jump back to top of stack

  200 ISP = ISPMAX
      LUN = ICLI(3,ISP)
      CALL SET_LUN_IN (LUN)

CD    PRINT *,'-- gen_input --'
CD    PRINT *,'   chax, istr, jdef: ', chax, istr, jdef

      RETURN
      END
