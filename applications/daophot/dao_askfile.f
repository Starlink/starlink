**==askfile.spg  processed by SPAG 4.54K  at 14:22 on  4 Oct 1996
C
C
C
************************************************************************
      SUBROUTINE ASKFILE(PROMPT,FILE)
*      IMPLICIT NONE
C
C=======================================================================
C
C Prompt the user neatly to type in a filename.
C
C Arguments
C
C PROMPT  (INPUT) the character string to be typed on the terminal
C         (right-justified to column 50, if possible).
C
C   FILE  (INPUT/OUTPUT) the filename.  On input, it is the default
C         filename, if any, to be offered to the user.  On output,
C         it is what the user wants.
C
C=======================================================================
C
C*** Start of declarations inserted by SPAG
      INTEGER I , J , K , L , M
C*** End of declarations inserted by SPAG

      CHARACTER*50 PROMPT,OPROMP
      CHARACTER*30 FILE , INFILE , EXTEND , SWITCH
      CHARACTER*3 EXT

      INTEGER CHR_LEN

      OPROMP = ' '
C
C Find the colon or question mark terminating the prompt.
C
      DO 100 I = 1 , 50
         IF ( (PROMPT(I:I).EQ.':') .OR. (PROMPT(I:I).EQ.'?') ) GO TO 200
 100  CONTINUE
C
C If the input character string FILE is all blanks or nulls, or if it
C is 'END OF FILE', no default filename is to be offered.
C
 200  CONTINUE
      I = I - 1
      IF ( FILE(1:11).NE.'END OF FILE' ) THEN
         DO 250 J = 30 , 1 , -1
            IF ( (ICHAR(FILE(J:J)).NE.0) .AND. (FILE(J:J).NE.' ') )
     :           GO TO 400
 250     CONTINUE
      END IF
C
C No default filename is to be offered.
C
      J = MAX(1,49-I)
      CALL CHR_APPND( PROMPT(1:I), OPROMP , J )
 300  CONTINUE
      WRITE (6,9001) OPROMP
 9001 FORMAT ( A,': ',$)
C
C Correct the format statement for a string length of 30
C Nick Eaton, Durham University, 19 Feb 1992.
C
      READ (*,9002,END=1200,ERR=300) FILE
 9002 FORMAT (A)
      K = CHR_LEN( FILE )
      IF ( K.LE.0 ) GO TO 300
      RETURN                                             ! Normal return
C
C A default filename is to be offered.
C
 400  CONTINUE
      K = MAX(1,38-I-J)
C
C If the input default filename includes a version number, wipe it out.
C
      DO 500 L = 1 , 30
         IF ( FILE(L:L).EQ.';' ) GO TO 600
 500  CONTINUE
      GO TO 800
C
 600  CONTINUE
      DO 700 M = L , 30
         FILE(M:M) = ' '
 700  CONTINUE
      J = L - 1
C
C
C Extract the filename extension from the input filename.
C
 800  CONTINUE
      DO 900 L = 30 , 1 , -1
         IF ( FILE(L:L).EQ.'.' ) GO TO 1000
 900  CONTINUE
      EXT = '   '
      GO TO 1100
 1000 CONTINUE
      EXT = FILE(L+1:MIN(30,L+4))
 1100 CONTINUE
      CALL CHR_APPND( PROMPT(1:I), OPROMP, K )
      CALL CHR_APPND( '(default ', OPROMP, K )
      CALL CHR_APPND( FILE(1:J) , OPROMP, K )
      WRITE (*,9003) OPROMP
 9003 FORMAT ( A, '): ',$)
      READ (*,9002,END=1200,ERR=1100) INFILE
      L = CHR_LEN ( INFILE )
      IF ( L.NE.0 ) THEN
C
C If the first character is a period, then the user wants to retain
C the default filename, but change the extension.
C
         IF ( INFILE(1:1).EQ.'.' ) THEN
            FILE = SWITCH(FILE,INFILE(1:L))
C
C If the first character is a semicolon, then the user wants to retain
C the default filename and filename extension, but wants to use other
C than the most recent version.
C
         ELSE IF ( INFILE(1:1).EQ.';' ) THEN
            FILE = FILE // INFILE(1:L)
         ELSE
            FILE = INFILE
            IF ( EXT.NE.'   ' ) FILE = EXTEND(FILE,EXT)
         END IF
      END IF
      RETURN                                             ! Normal return
C
C CTRL-Z was entered.
C
 1200 CONTINUE
      FILE = 'END OF FILE'
      RETURN
C
      END
