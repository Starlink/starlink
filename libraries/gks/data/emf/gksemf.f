      PROGRAM GKSEMF

      INCLUDE '../../include/check.inc'
      INCLUDE '../../include/gkmc.par'

      INTEGER NREC, IERROR, N, IENDPT, NEXT, INPST, INREC

      INTEGER LEN
      INTRINSIC LEN

      INTEGER LINESZ
      PARAMETER (LINESZ = 74)

      INTEGER MESLEN
      PARAMETER (MESLEN = 508)

      INTEGER RECLEN
      PARAMETER (RECLEN = (MESLEN + 4)/KRCBYT)

      CHARACTER * (LINESZ) PART
      CHARACTER * (MESLEN) AMESS

      CHARACTER * 1 SPACE
      PARAMETER (SPACE = ' ')

      INTEGER FROM, TO
      PARAMETER ( FROM = 1, TO = 2)

      CHARACTER * (*) SEQN, RNDM
      PARAMETER ( SEQN = 'gksemf.dat',
     :            RNDM = 'gks.emf')

*     Prologue

      OPEN (UNIT = FROM, FILE = SEQN, STATUS = 'OLD', BLANK = 'ZERO')
      OPEN (UNIT = TO, FILE = RNDM, ACCESS = 'DIRECT', RECL = RECLEN)

      INREC = 0
      NREC = 1
      IERROR = -10000

*     Copy messages

*     Loop: repeat reading error messages
*     until an error or the end of file is detected (jump out)

   10 CONTINUE

        INREC = INREC + 1
        READ(FROM, 1000, IOSTAT = INPST) N, PART

        IF (INPST .GT. 0) THEN
          WRITE (*, 2000) INPST, NREC - 1
 2000     FORMAT ('Error reading file, status = ', I5,
     :            /, I5, ' messages read so far')
          GOTO 40
        ENDIF

        IF (N .NE. 0 .OR. INPST .LT. 0) THEN
*       First line of message or end of file
          IF (NREC .GT. 1) THEN
*         Pad preceding message with spaces and output it
            AMESS (NEXT:) = SPACE
            WRITE (TO, REC = NREC) IERROR, AMESS
            IF (INPST .LT. 0) THEN
*           End of file - normal exit
              GOTO 50
            ENDIF
          ENDIF
*         Initialise values for this message
          NREC = NREC + 1
          IF (N .LE. IERROR) THEN
            WRITE (*, 2010) N
 2010       FORMAT ('Error message ', I5, ' is in the wrong order')
            GOTO 40
          ENDIF
          IERROR = N
          NEXT = 0
        ELSE
*       Continuation of message; append space to what we have so far
          AMESS (NEXT:NEXT) = SPACE
        ENDIF

*       All cases: append part to message and update pointer

*       First set IENDPT to last non-space character in PART
        DO 20 IENDPT = LEN (PART), 1, -1
          IF (PART (IENDPT:IENDPT) .NE. SPACE) GO TO 30
   20   CONTINUE
        IENDPT = 0
   30   CONTINUE

        IF (IENDPT .EQ. 0) THEN
          WRITE (*, 2020) IERROR
 2020     FORMAT ('Error message ', I5, ' contains a blank line')
          GOTO 40
        ELSEIF (NEXT + IENDPT .GT. MESLEN) THEN
          WRITE (*, 2030) IERROR
 2030     FORMAT ('Error message ', I5, ' is too long')
          GOTO 40
        ELSE
          AMESS (NEXT + 1 : NEXT + IENDPT) = PART (1:IENDPT)
          NEXT = NEXT + IENDPT + 1
        ENDIF
      GO TO 10

*     Error has occurred

   40 CONTINUE
      WRITE (*, 2040) RNDM, INREC, SEQN
 2040 FORMAT (/, 'Failed to create file ', A, /
     :        '   after reading ', I3, ' lines of ', A, /)

*     Epilogue - normal exit at end of input file

   50 CONTINUE
      WRITE (TO, REC = 1) NREC
      CLOSE (FROM)
      CLOSE (TO)

 1000 FORMAT (I5, 1X, A)

      END
