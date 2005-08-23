*  History:
*     16 Nov 1993 (hme):
*        Change file name from help.f to scl_help.f.
*        Replace STR$UPCASE with CHR_UCASE.
*     10 Dec 1993 (hme):
*        Change terminal output "^z" or "CTRL(Z)" to EOF.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     31 Jan 1994 (hme):
*        Disuse <> in formats.
*      6 Jun 2000 (ajc):
*        Replace 'type *' with 'PRINT *'
C-------------------------------------------------------------------------

      SUBROUTINE HELP (ILOUT)

C  Routine to list all commands similar to that given, i.e., all those commands
C  where a match can be obtained, even if the first word of the
C  (system) command is not checked

      INTEGER      GEN_KNTWRD
      INTEGER      GEN_ILEN
      CHARACTER*40 COM1, COM2, TITLE, WORD
      CHARACTER*40 BAR
      DATA         BAR /'----------------------------------------'/

      INCLUDE 'COMMAND_TABLE'

      ILC  = LEN (COMMS(1))
      COM1 = ' '

      PRINT *, 'This command prints out all built-in commands which'
      PRINT *, 'have any "word" starting with the string you quote.'
      PRINT *, 'Examine your own symbols using SHOW-SYMBOLS at >>prompt'

   50 CONTINUE

      WRITE (ILOUT,*)
      CALL GEN_GETSTR2 (1, 'String to match (topic)? ( EOF to quit )',
     &                  ' ', ' ', COM1, JDEF)
      IF (JDEF.LT.0 .OR. JDEF.EQ.2)  RETURN

      ILX   = GEN_ILEN (COM1)
      TITLE = COM1
      IF (ILX.EQ.0) THEN
        TITLE = '(All)'
        ILX   = 5
      END IF
*      WRITE (ILOUT, '(/1X,A<ILX>/1X,<ILX>(''-''))')  TITLE(:ILX)
      WRITE (ILOUT, '(/1X,A/1X,A)')  TITLE(:ILX), BAR(:ILX)

      CALL UUCASE (COM1)
      NWRD1 = GEN_KNTWRD (COM1(1:MIN(ILC,ILX)))
*     DO I = 1,NFPP
      DO I = 1,NFUNC
        COM2(1:ILC) = COMMS(I)
        NWRD2       = GEN_KNTWRD (COM2(1:ILC))
        NMATCH      = 0

        IF (NWRD2. GE. NWRD1)   THEN
          DO J = 1, (NWRD2-NWRD1+1)
            CALL GEN_MATCH  (COM1(1:ILC), COM2(1:ILC), NWRD1, IMATCH)
            CALL GEN_GETWRD (COM2(1:ILC), 1, LWRD, WORD)
            COM2(1:ILC-2) = COM2(LWRD+2:ILC)
            NMATCH = NMATCH + IMATCH
          END DO
          IF (NMATCH.NE.0 .OR. JDEF.EQ.1)   THEN
            WRITE (ILOUT, '(1X,A)') COMMS(I)
          END IF
        END IF
      END DO
      GO TO 50

      END

C-----------------------------------------------------------------------------

