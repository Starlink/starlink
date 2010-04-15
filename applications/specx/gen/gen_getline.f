*  History:
*     25 Mar 1989 (rp):
*        Modified 25-3-89 to allow immediate processing of comments,
*        and to ignore continuation marks at the end of comment lines.
*     07 Dec 1993 (hme):
*        Due to the change in gen_putpmt, we need an extra carriage
*        return if the user replied with an EOF character.
*     01 Feb 1995 (rpt)
*        Changed code to use get_aline.c to provide command-line
*        editing etc.with UNIX version.
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Set ILIN=0 if line is blank
*        Unused ILS
*-----------------------------------------------------------------------

      SUBROUTINE GEN_GETLINE (STRING,PROMPT,JDEF)

*   Routine to get a line from the terminal, assuming that it may consist
*   of several lines with continuation marks ('-') as the last character.
*   If the first character is ! or * the line is treated as a comment and
*   ignored.

      LOGICAL*4 COMMENT, CONTINUATION, BLANK
      INTEGER*4 GEN_ILEN, ILEN, ITRM
      CHARACTER ALINE*132,STRING*(*),PROMPT*(*)

      INTEGER*4 GET_ALINE
      EXTERNAL  GET_ALINE

      INCLUDE 'LOGICAL_UNITS.INC'
      INCLUDE 'CLI_STACK.INC'

      ITRM = 0

      IF (LUN_IN_SET.EQ.0  .OR. LUN_IN.EQ.0)    LUN_IN  = 5
      IF (LUN_OUT_SET.EQ.0 .OR. LUN_OUT.EQ.0)   LUN_OUT = 6

      JDEF   = 0
      ISTR   = 0
      STRING = ' '

      IF (LUN_OUT_SET.EQ.0) LUN_OUT = 6

      CONTINUATION = .TRUE.
      DO WHILE (CONTINUATION)

        COMMENT = .TRUE.
        BLANK   = .FALSE.
        DO WHILE (COMMENT .AND. .NOT.BLANK)

*         Read a line and increment the line number

          IF (LUN_IN.EQ.5) THEN
             IERR = GET_ALINE (ALINE, PROMPT, ITRM)
             IF (IERR .LT. -1) GO TO 10
             IF (IERR .EQ. -1) GO TO 20
             ILEN = IERR
             IERR = 0
          ELSE
             IF (ISP.eq.0)
     1         WRITE(LUN_OUT,'(A),$', ERR=10, IOSTAT=IERR) PROMPT
             READ (LUN_IN, '(A)', ERR=10, END=20, IOSTAT=IERR) ALINE
          ENDIF
          ICLI(4,ISP) = ICLI(4,ISP) + 1

   10     IF (IERR.NE.0)   THEN
            JDEF = -IERR
            RETURN
          END IF

*         Check not blank

          IF (ALINE.EQ.' ') THEN
            BLANK  = .TRUE.
            ILIN = 0

          ELSE
            BLANK  = .FALSE.

*           Remove leading blanks

            I = 1
            DO WHILE ( ALINE(I:I).EQ.' ')
              I = I + 1
            END DO

            ILIN = GEN_ILEN (ALINE)

            IF (ALINE(I:I).NE.'!' .AND. ALINE(I:I).NE.'*') THEN
              COMMENT = .FALSE.
            END IF

          END IF
        END DO

        CONTINUATION = .FALSE.
        IF (ILIN.NE.0)   THEN
          IF (ISTR.NE.0) STRING(ISTR:ISTR) = ' '        ! Must be on ctn'n line
          STRING (ISTR+1:ISTR+ILIN-I+1) = ALINE(I:ILIN) ! Add the new line
          ISTR = ISTR + (ILIN-I+1)                      ! Increment its length

          IF (ALINE(ILIN:ILIN).EQ.'-')  THEN            ! To be continued?
            CONTINUATION = .TRUE.
          END IF

        END IF

      END DO
      IF (GEN_ILEN (STRING).EQ.0)   JDEF = 1

CD    print *, ' gen_getline --> ', string(:istr)

      RETURN

   20 JDEF = 2
      WRITE (LUN_OUT, *)
      RETURN

      END
