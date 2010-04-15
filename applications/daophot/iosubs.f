C
C=======================================================================
C
C This file contains subroutines which prompt the user neatly to enter
C needed information via the terminal keyboard.
C
C            OFFICIAL DAO VERSION:  1991 April 18
C
C***********************************************************************
C
C Current contents
C
C   INQUIR  right justifies a prompting string to an arbitrary column
C   GETNAM  asks (neatly) for a filename, offering a default.
C   GETFIL  asks for the name and opens the file
C   GETDAT  asks (neatly) for real-number input.
C   GETYN   asks (neatly) for an answer, and accepts only 'Y', 'y',
C           'N', 'n', 'E', or 'e'.
C   GETCHR  asks (neatly) for a Character string
C
C If CTRL-Z is entered as input to any of the above, the routine will
C return directly to the calling routine, with a unique response.
C
C   TBLANK  types a blank line on the terminal.
C   SWITCH  removes the filename extension from an input filename and
C           attaches another character string in its place.
C   EXTEND  checks to see whether an input filename contains a filename
C           extension and, if not, attaches a default filename
C           extension.
C
C***********************************************************************
C
      SUBROUTINE  INQUIR (PROMPT, N)
C
C=======================================================================
C
C Simply type a prompting character string on the terminal,
C right-justified to column N and followed by a single blank, and
C suppress the carriage return.
C
C Argument
C
C PROMPT  (INPUT) is the character string.
C      N  (INPUT) is the column number to contain the last character.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MIN0
      CHARACTER PROMPT*(*), P*132, LINE*132
      INTEGER I, J, K, L, M, N
C
C-----------------------------------------------------------------------
C
C Find the last printing character in the prompt.
C
      M = MIN0(132, N)
      L = MIN0(132, LEN(PROMPT))
      P = PROMPT
 1000 K = 0
      DO I=1,L
         J = ICHAR(P(I:I))
         IF ((J .GE. 33) .AND. (J .LE.126)) K=I
      END DO
C
C K is the position of the last valid character in the
C prompt.  If K is greater than M, the prompt won't fit in one
C line. Find the last blank before position M; this
C will serve as a line break.
C
      IF (K .GT. M) THEN
         DO J=M,1,-1
            IF (P(J:J) .EQ. ' ') GO TO 1500
         END DO
 1500    WRITE (6,600) P(1:J)
 600     FORMAT (1X, A)
         P = P(J+1:K)
         GO TO 1000
      ELSE
         J = 0
      END IF
      LINE = ' '
C
C Now right justify to column M whatever is left
C in the prompting string.
C
      L = M
      DO I=K,J+1,-1
         LINE(L:L) = P(I:I)
         L = L-1
      END DO
C
      WRITE (6,601) LINE(1:M)//' '
  601 FORMAT (1X, A, $)
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  GETNAM (PROMPT, FILE)
C
C=======================================================================
C
C Prompt the user neatly to type in a filename.
C
C Arguments
C
C PROMPT  (INPUT) the character string to be typed on the terminal
C         (right-justified to column 50).
C
C   FILE  (INPUT/OUTPUT) the filename.  On input, it is the default
C         filename, if any, to be offered to the user.  On output,
C         it is what the user wants.
C
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER*(*) PROMPT, FILE
      CHARACTER*132 LINE
      CHARACTER*30 IFILE, SWITCH
      CHARACTER*1 PUNC
      INTEGER I, J, M, N, IERR
C
C If the input character string FILE is all non-printing characters,
C or if it C is 'END OF FILE', no default filename is to be offered.
C Special case: the filename is automatically truncated at a semicolon
C to erase VMS version numbers.
C
      IF (FILE(1:11) .EQ. 'END OF FILE') GO TO 1220
      N = 0
      DO I=1,LEN(FILE)
         J = ICHAR(FILE(I:I))
         IF (J .EQ. 59) GO TO 1210
         IF ((J .GE. 33) .AND. (J .LE. 126)) N = I
      END DO
C
C N marks the position of the last valid character (before the
C semicolon, if any).
C
 1210 IF (N .GT. 0) GO TO 1230
C
C No default filename is to be offered.
C
 1220 CALL INQUIR (PROMPT, 50)
      READ (5,501,END=9100,IOSTAT=IERR) FILE
  501 FORMAT (A)
      IF (IERR .NE. 0) THEN
         CALL STUPID ('Error reading filename.')
         GO TO 1220
      END IF
      RETURN                                             ! Normal return
C
 1230 CONTINUE
C
C A default filename is to be offered.  This is a bit more difficult.
C
C First find the last non-blank character of the prompt.  If this
C has an ascii code outside the ranges 65-90 and 97-122 regard it as
C punctuation.  Otherwise add a colon to the end of the prompt.
C
      M = 0
      DO I=1,LEN(PROMPT)
         J = ICHAR(PROMPT(I:I))
         IF ((J .NE. 0) .AND. (J .NE. 32)) M=I
      END DO
      J = ICHAR(PROMPT(M:M))
      IF ( ((J.GE.63).AND.(J.LE.90)) .OR. ((J.GE.97).AND.(J.LE.122)) )
     .     THEN
         PUNC = ':'
      ELSE
         PUNC = PROMPT(M:M)
         M = M-1
      END IF
C
C M now marks the last non-blank, non-null character before the terminal
C punctuation.  We are now ready to prompt for input.
C
 1240 CONTINUE
CCC      IF (M+N+12 .LE. 50) THEN
         LINE = PROMPT(1:M)//' (default '//FILE(1:N)//')'//PUNC
         CALL INQUIR (LINE, 50)
CCC      ELSE
CCC         CALL INQUIR (PROMPT(1:M)//' ('//FILE(1:N)//')'//PUNC, 50)
CCC      END IF
      READ (5,501,IOSTAT=IERR,END=9100) IFILE
      IF (IERR .NE. 0) THEN
         CALL STUPID ('Error reading filename.')
         GO TO 1240
      END IF
      IF (FILE(1:11) .EQ. 'END OF FILE') GO TO 9100
C
C If IFILE is blank, use the default.
C
      IF (IFILE .EQ. ' ') THEN
         FILE = FILE(1:N)
         RETURN
      END IF
C
C At this point there are three possibilities.  If the first character
C is a period ('.'), the input is to be treated as a filename extension
C which is to be substituted for that of the default.
C
      IF (IFILE(1:1) .EQ. '.') THEN
         FILE = SWITCH(FILE, IFILE)
         RETURN
      END IF
C
C Otherwise, if IFILE contains a period other than in the first
C position, it is to be adopted verbatim.
C
      DO I=1,30
         J = ICHAR(IFILE(I:I))
         IF (J .EQ. 46) THEN
            FILE = IFILE
            RETURN
         END IF
      END DO
C
C Now we are left with the trickiest possibility.  The input file name
C contains no period, so the extension from the input file is to be
C transferred (if any).
C
C Extract the filename extension from the default.  The extension is
C is any text after the last period.
C
      DO J=N,1,-1
         IF (FILE(J:J) .EQ. '.') GO TO 1250
      END DO
C
C No period was found.  The default has no extension, either, so adopt
C the input as the final filename.
C
      FILE = IFILE
      RETURN
C
 1250 FILE = SWITCH(IFILE, FILE(J:N))
      RETURN
C
C CTRL-Z or END OF FILE was entered.
C
 9100 FILE='END OF FILE'
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  GETFIL (PROMPT, FILE, LUN, STATUS)
      IMPLICIT NONE
      CHARACTER PROMPT*(*), FILE*(*), STATUS*1
      INTEGER LUN, ISTAT
C
 1000 CALL GETNAM (PROMPT, FILE)
      IF ((STATUS .EQ. 'N') .OR. (STATUS .EQ. 'n')) THEN
         CALL OUTFIL (LUN, FILE, ISTAT)
      ELSE
         CALL INFILE (LUN, FILE, ISTAT)
      END IF
      IF (ISTAT .NE. 0) GO TO 1000
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  GETDAT (PROMPT, DATA, N)
C
C=======================================================================
C
C Prompt the user to input numerical data.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string to be typed out,
C         right-justified to column 50.
C
C   DATA  (OUTPUT) is a vector of dimension N, to return the numerical
C         values entered by the user to the calling program.
C
C      N  (INPUT) is the number of individual data items the user
C         is required to enter.
C
C Accept only numerical data, or CTRL-Z.  If CTRL-Z is entered, DATA(1)
C is returned to the calling program equal to VAL__MINR.
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      CHARACTER PROMPT*(*), LINE*80
      INTEGER N
      REAL DATA(N)
      INTEGER I, IERR
C
C-----------------------------------------------------------------------
C
 1000 CALL INQUIR (PROMPT, 50)
      READ (5,501,IOSTAT=IERR,END=9100) LINE
  501 FORMAT (A)
      IF (IERR .NE. 0) THEN
         CALL STUPID ('Error reading data.')
         GO TO 1000
      END IF
      IF ((LINE(1:1) .EQ. 'E') .OR. (LINE(1:1) .EQ. 'e')) GO TO 9100
      READ (LINE,*,IOSTAT=IERR) (DATA(I), I=1,N)
      IF (IERR .NE. 0) THEN
         CALL STUPID ('Error reading data.')
         GO TO 1000
      END IF
      RETURN
C
C CTRL-Z was entered
C
 9100 DATA(1)=VAL__MINR
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  GETYN (PROMPT, ANSWER)
C
C=======================================================================
C
C A subroutine to get the user's response to a yes/no question.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string, to be typed out
C         right-justified to column 50.
C
C ANSWER  (OUTPUT) is the user's response to the question.
C
C Accept only 'Y', 'y', 'N', 'n', or CTRL-Z as a valid response:
C If 'Y' or 'y', return ANSWER = 'Y' to the calling routine.
C If 'N' or 'n', return ANSWER = 'N' to the calling routine.
C If CTRL-Z, return ANSWER = 'E' to the calling routine.
C
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER PROMPT*(*), ANSWER*1
      INTEGER IERR
C
 1000 CALL INQUIR (PROMPT, 50)
      READ (5,510,IOSTAT=IERR,END=9100) ANSWER
  510 FORMAT (A1)
      IF (IERR .NE. 0) THEN
         CALL STUPID ('Error reading answer.')
         GO TO 1000
      END IF
      IF (ANSWER .EQ. 'y') ANSWER='Y'
      IF (ANSWER .EQ. 'n') ANSWER='N'
      IF (ANSWER .EQ. 'e') ANSWER='E'
      IF ((ANSWER .NE. 'Y') .AND. (ANSWER .NE. 'N') .AND.
     .     (ANSWER .NE. 'E')) THEN
         CALL STUPID ('Error understanding answer.')
         GO TO 1000
      END IF
      RETURN                                             ! Normal return
C
C CTRL-Z was entered.
C
 9100 ANSWER='E'
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  GETCHR (PROMPT, ANSWER, N)
C
C=======================================================================
C
C A subroutine to get a character string from the user.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string, to be typed out
C         right-justified to column 50.
C
C ANSWER  (OUTPUT) is the user's response.
C
C      N  (OUTPUT) is the number of characters in the user's response.
C
C Accept about anything.
C
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER*(*) PROMPT, ANSWER
      INTEGER I, J, N, IERR
C
 1000 CALL INQUIR (PROMPT, 50)
      READ (5,510,IOSTAT=IERR,END=9000) ANSWER
  510 FORMAT (A)
      IF (IERR .NE. 0) THEN
         CALL STUPID ('Error reading input.')
         GO TO 1000
      END IF
      N = 0
      DO I = 1,LEN(ANSWER)
         J = ICHAR(ANSWER(I:I))
         IF ((J .NE. 0) .AND. (J .NE. 32)) N=I
      END DO
      RETURN
 9000 N = -1
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  TBLANK
C
C=======================================================================
C
C A subroutine to type a blank line on the terminal.
C
C=======================================================================
C
      IMPLICIT NONE
      WRITE (6,*)
      RETURN
C
      END!
C
C#######################################################################
C
      CHARACTER*30  FUNCTION  SWITCH  (FILE, ADDEND)
C
C=======================================================================
C
C Subroutine to chop off a filename extension from an input filename
C character string, and append a new character string to the end.
C
C Arguments
C
C   FILE (INPUT) is a character string containing a filename.
C
C ADDEND (INPUT) is the character string to be pasted onto the end
C        of the character string obtained by removing a directory
C        name and the period and all that follows from the input
C        filename.
C
C SWITCH is the filename part of NAFILE, with any directory name
C        contained in brackets chopped off from the front, and with
C        the character string ADDEND substituted for the period and
C        filename extension of the input string.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER LEN
C
      CHARACTER*(*) FILE, ADDEND
      INTEGER I, L, LAST
      L = LEN(FILE)
      I=0
 1000 I=I+1
      IF (I .GT. L) GO TO 9000
C
C If a left bracket is detected, then a directory name is present.
C Find the corresponding right bracket (ignoring any periods met along
C the way).
C
      IF ((FILE(I:I) .EQ. '[') .OR. (FILE(I:I) .EQ. '<')) THEN
 1001    I=I+1
         IF (I .GT. 30) GO TO 9100
         IF ((FILE(I:I) .NE. ']') .AND. (FILE(I:I) .NE. '>')) GO TO 1001
         GO TO 1000
      ELSE IF (FILE(I:I) .EQ. '.') THEN
         SWITCH=FILE(1:I-1)//ADDEND
         RETURN
      ELSE IF (FILE(I:I) .EQ. ' ') THEN
         GO TO 1000
      ELSE
         LAST=I
         GO TO 1000
      END IF
C
 9000 CONTINUE
      SWITCH=FILE(1:LAST)//ADDEND
      RETURN
C
 9100 CONTINUE
      WRITE (6,691) CHAR(7), FILE, ADDEND
  691 FORMAT (/' Error creating file name: ', A1, 5X, A30, 5X, A10/)
      CALL BYEBYE
      END!
C
C#######################################################################
C
      CHARACTER*30  FUNCTION  EXTEND (FILE,EXT)
C
C=======================================================================
C
C A function to see whether a filename contained in an input character
C string contains a filename extension.  If it does, leave it alone.
C If not, then attach the filename extension contained in EXT.
C
C Arguments:
C
C FILE a character string presumably containing a filename.
C
C  EXT a character string containing a default filename extension.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER LEN
C
      CHARACTER*(*) FILE, EXT
      INTEGER I, J, L
      L = LEN(FILE)
      I=0
 1000 I=I+1
      IF (I .GT. L) GO TO 9100
      IF ((FILE(I:I) .EQ. '[') .OR. (FILE(I:I) .EQ. '<')) THEN
C
C If a left bracket is found then a directory name is present.  Find the
C corresponding right bracket, ignoring any periods met along the way.
C
 1001    I=I+1
         IF ((FILE(I:I) .NE. ']') .AND. (FILE(I:I) .NE. '>')) GO TO 1001
         GO TO 1000
      ELSE
C
C If a period is found, then a filename extension is present.
C
         IF (FILE(I:I) .EQ. '.') THEN
            EXTEND=FILE
            RETURN
         END IF
C
C If a non-blank, non-null character is found, then the filename
C continues to at least this point in the string.
C
         IF ((FILE(I:I) .NE. ' ') .AND. (FILE(I:I) .NE. CHAR(0))) J=I
         IF (I .LT. L) GO TO 1000
      END IF
C
C No period was found, so append "." and EXT to the end of the
C character string.
C
      EXTEND=FILE(1:J)//'.'//EXT
      RETURN
 9100 WRITE (6,691) CHAR(7), FILE, EXT
  691 FORMAT (/' Error creating file name: ', A1, 5X, A30, 5X, A3/)
      CALL BYEBYE
      END!
C
C#######################################################################
C
      SUBROUTINE  RDSTAR (LU, NL, ID, X, Y, AMAG, SKY)
C
C=======================================================================
C
C Read in an ID number, x and y coordinates, magnitude, and sky value
C for the next star in the input file, whatever the file type (i.e.,
C NL = 1, 2, or 3).  If an end of file is encountered, set the star ID
C to a negative number and return; if a blank line is encountered, the
C star ID will be zero.  LU is the logical unit number to be used; the
C other arguments are obvious.
C
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER*133 LINE
      REAL X, Y, AMAG, SKY, DUMMY
      INTEGER LU, N, ISTAT, ID, NL
C
  900 CALL RDCHAR (LU, LINE, N, ISTAT)
      IF (ISTAT .GT. 0) THEN
         ID = -1   !  ID = -1 means END OF FILE
         RETURN
      ELSE IF (ISTAT .LT. 0) THEN
         CALL STUPID ('Unable to read line from input file')
         CALL OOPS
      END IF
C
      IF (N .LE. 1) THEN
         ID = 0                 ! Blank line
         RETURN
      END IF
C
      N = N+1
      LINE(N:N) = '/'
      IF (NL .EQ. 1) THEN
         READ (LINE(2:N),*,ERR=1000) ID, X, Y, AMAG, DUMMY, SKY
      ELSE IF (NL .EQ. 2) THEN
         READ (LINE(2:N),*,ERR=1000,END=2000) ID, X, Y, AMAG
         IF (ID .NE. 0) READ(LU,321,ERR=1000) SKY
  321    FORMAT (4X, F9.3)
      ELSE IF (NL .EQ. 3) THEN
         READ (LINE(2:N),*,ERR=1000) ID, X, Y, AMAG, SKY
      END IF
      RETURN                                             ! Normal return
 1000 CALL STUPID
     .     ('WARNING:  Corrupt star data encountered in input.')
      GO TO 900
C
C-----------------------------------------------------------------------
C
C End of file was encountered.
C
 2000 ID=-1
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  RDHEAD (LUN, NL, NCOL, NROW, LOBAD, HIBAD,
     .     THRESH, AP1, PHPADU, READNS, FRAD)
C
C=======================================================================
C
C Read file header from a sequential data file on the disk.  If the
C file has no header, rewind to beginning.
C
C Input argument
C
C    LUN  is the logical unit number of the disk file.
C
C     NL  if NL is greater than zero on input, then in the event that
C         the header is missing from the input file, the missing
C         header information will be requested via the keyboard.
C         If NL is equal to zero on input, only NCOL and NROW
C         will be requrested.  If NL is a negative number on input,
C         missing information will NOT be requested of the user.
C
C Output arguments
C
C     NL  is a code indicating the file type:
C            NL = 3 a group file
C            NL = 2 an aperture photometry file
C            NL = 1 other (output from FIND, PEAK, or NSTAR)
C            NL = 0 a file without a header
C
C    NCOL  is the number of columns in the picture (length in x)
C
C    NROW  is the number of rows in the picture (length in y)
C
C  THRESH  is the star-finding threshold that was used (from FIND)
C
C     AP1  is the radius of the first aperture (from PHOT)
C
C  PHPADU  is the number of photons per ADU that was given(from PHOT)
C
C  READNS  is the readout noise per pixel in ADU (from PHOT)
C
C   LOBAD  is the low bad pixel limit that was used (from FIND)
C
C   HIBAD  is the high bad pixel limit that was used (from FIND)
C
C    FRAD  is the fitting radius (from OPTION, used by PSF, PEAK,
C          and NSTAR)
C
C=======================================================================
C
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      CHARACTER*133 LINE
      CHARACTER*4 FIRST4, TEST
      REAL LOBAD, HIBAD, THRESH, AP1, PHPADU, READNS, FRAD, X
      INTEGER LUN, NL, NCOL, NROW, N, ISTAT, NLIN
C
      READ (LUN,900) FIRST4, TEST
  900 FORMAT (A4, 9X, A4)
      IF (FIRST4 .NE. ' NL ') GO TO 8000               ! No header lines
      IF (TEST .EQ. '  LO') THEN
C
C Latest DAOPHOT II file header.
C
         CALL RDCHAR (LUN, LINE, N, ISTAT)
         N = N+1
         LINE(N:N) = '/'
         READ (LINE,*) NL, NCOL, NROW, LOBAD, HIBAD, THRESH,
     .        AP1, PHPADU, READNS, FRAD
      ELSE IF ((TEST .NE. 'LOWB') .AND. (TEST .NE. 'OWBA')) THEN
C
C Obsolete file header.
C
         READ (LUN,901) NLIN, NCOL, NROW, THRESH, AP1, PHPADU, READNS,
     .        LOBAD, FRAD
  901    FORMAT (1X, I2, 2I4, 7F8.1)
         IF (NL .GE. 0) THEN
            IF (AP1 .LE. 0.) THEN
 1010          CALL GETDAT ('Lowest good data-value:', LOBAD, 1)
               IF (LOBAD .LE. VAL__MINR) GO TO 1010
            END IF
 1020       CALL GETDAT ('Highest good data-value:', HIBAD, 1)
            IF (HIBAD .LE. 0.) GO TO 1020
         END IF
         NL=NLIN
      ELSE
         READ (LUN,901) NL, NCOL, NROW, LOBAD, HIBAD, THRESH, AP1,
     .        PHPADU, READNS, FRAD
      END IF
      READ (LUN,900)
      RETURN                                           ! Normal return
C
 8000 REWIND (LUN)
      IF (NL .GE. 0) THEN
 8001    CALL GETDAT ('Number of columns in the image:', X, 1)
         IF (X .LT. 0.5) GO TO 8001
         NCOL = NINT(X)
 8002    CALL GETDAT ('Number of rows in the image:', X, 1)
         IF (X .LT. 0.5) GO TO 8002
         NROW = NINT(X)
      END IF
C
      IF (NL .GT. 0) THEN
 8003    CALL GETDAT ('Readout noise (ADU):', READNS, 1)
         IF (READNS .LE. 0.) GO TO 8003
 8006    CALL GETDAT ('Gain (photons per ADU):', PHPADU, 1)
         IF (PHPADU .LE. 0.) GO TO 8006
 8010    CALL GETDAT ('Lowest good data-value:', LOBAD, 1)
         IF (LOBAD .LE. VAL__MINR) GO TO 8010
 8020    CALL GETDAT ('Highest good data-value:', HIBAD, 1)
         IF (HIBAD .LE. VAL__MINR) GO TO 8020
      END IF
      NL=0
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  WRHEAD (LUN, NL, NCOL, NROW, ITEMS, LOBAD, HIBAD,
     .     THRESH, AP1, PHPADU, READNS, FRAD)
C
C=======================================================================
C
C Subroutine to write a standard header into an output sequential
C data file on the disk.  Same as RDHEAD, except that all the output
C arguments are now input arguments.  ITEMS tells how many of the
C individual arguments are to be written into the header.
C
C              Latest DAOPHOT II version:  1991 January 15
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER*8 HEAD(7)
      REAL A(7)
C
      REAL LOBAD, HIBAD, THRESH, AP1, PHPADU, READNS, FRAD
      INTEGER LUN, NL, NCOL, NROW, ITEMS, I
      DATA HEAD /'  LOWBAD', ' HIGHBAD', '  THRESH', '     AP1',
     .     '  PH/ADU', '  RNOISE', '    FRAD'/
C
C-----------------------------------------------------------------------
C
      A(1)=LOBAD
      A(2)=HIBAD
      A(3)=THRESH
      A(4)=AP1
      A(5)=PHPADU
      A(6)=READNS
      A(7)=FRAD
      WRITE (LUN,900) (HEAD(I), I=1,ITEMS)
  900 FORMAT (' NL   NX   NY', 8A8)
      WRITE (LUN,901) NL, NCOL, NROW, (A(I), I=1,ITEMS)
  901 FORMAT (1X, I2, 2I5, 2F8.1, 5F8.2)
      WRITE (LUN,901)                             ! Write a blank line
      RETURN                                        ! Normal return
C
      END!
C
C#######################################################################
C
      SUBROUTINE RDCHAR (LUN, LINE, N, ISTAT)
      IMPLICIT NONE
      CHARACTER*(*) LINE
      INTEGER LUN, N, ISTAT, I, J
C
      READ (LUN,1,END=8000,ERR=9000) LINE
    1 FORMAT (A)
      N = 0
      DO I=1,LEN(LINE)
         J = ICHAR(LINE(I:I))
         IF ((J .GE. 33) .AND. (J .LE. 126)) N = I
      END DO
      ISTAT = 0
      RETURN
 8000 LINE = ' '
      N = 0
      ISTAT = 1  ! ISTAT = 1 means END OF FILE
      RETURN
 9000 LINE = ' '
      N = 0
      ISTAT = -1  ! ISTAT = -1 means ERROR
      RETURN
      END!
C
C#######################################################################
C
      FUNCTION EXTRCT (STRING, IFLAG)
      IMPLICIT NONE

*  History:
*     17-Mar-1995 (GJP)
*     Replaced very negative numbers (-1E38) with VAL__MINR.

*  Global Constants:
      INCLUDE 'PRM_PAR'               ! PRIMDAT primitive data constants

      INTEGER LEN
      CHARACTER*(*) STRING
      REAL EXTRCT
      INTEGER L, IFLAG
C
      L = LEN(STRING)
      READ (STRING, *, ERR=9000) EXTRCT
      IFLAG = 0
      RETURN
 9000 EXTRCT = VAL__MINR
      IFLAG = -1
      RETURN
      END!
C
C#######################################################################
C
      SUBROUTINE  STUPID (MESSAG)
      IMPLICIT NONE
      CHARACTER*(*) MESSAG
      CHARACTER*1 BELL
      BELL = CHAR(7)
      WRITE (6,601) MESSAG, BELL
  601 FORMAT (/1X, A, A1/)
      RETURN
      END!
C
C#######################################################################
C
      INTEGER  FUNCTION  ICNVRT (ASTRING)
C
C=======================================================================
C
C This little function is supposed to take two ASCII characters and
C express them as an integer in the range 0-(32**2-1) without
C distinguishing upper and lower case:
C
C AA = Aa = aA = aa = 0, AB = Ab = aB = ab = 1, BA = Ba = bA = ba = 32,
C etc.
C
C Argument
C
C ASTRING is a character string containing two ASCII characters.
C
C=======================================================================
C
      IMPLICIT NONE
      CHARACTER*2 ASTRING
C
C-----------------------------------------------------------------------
C
      ICNVRT=32*MOD(ICHAR(ASTRING(1:1))-1,32)+
     .     MOD(ICHAR(ASTRING(2:2))-1,32)
      RETURN
C
      END!
C
C#######################################################################
C
      SUBROUTINE  OPTION (OPTFIL, NOPT, LBL, OPT, OMIN,
     .     OMAX, PROMPT, ISTAT)
C
C=======================================================================
C
C Acquire a set of optimization parameters, either from a disk file
C or from the keyboard.
C
C              OFFICIAL DAO VERSION:  1985 October 25
C
C Arguments
C
C OPTFIL (INPUT) a character string containing either (a) the name of
C         a disk file which will be offered as the default file from
C         which the parameter definitions are to be read (if OPTFIL =
C         ' ', no default file name will be offered; if OPTFIL =
C         'daophot.opt' and this file does not exist, no error message
C         is to be produced and the current contents of OPT are to be
C         left alone), or (b) the string 'KEYBOARD INPUT', in which
C         case the user is to be prompted to define parameter values
C         via the terminal.
C
C     OPT (INPUT/OUTPUT) numerical values for the NOPT user-definable
C         parameters.
C
C=======================================================================
C
      IMPLICIT NONE
      INTEGER MAXOPT, NOPT
      PARAMETER  (MAXOPT=100)
C
C Parameter:
C
C NOPT is the number of user-specifiable optional parameters.
C
      CHARACTER*26 LBL(NOPT)
      REAL OPT(NOPT), OMIN(NOPT), OMAX(NOPT)
      INTEGER IDOPT(MAXOPT)
C
      REAL EXTRCT
      INTEGER ICNVRT
C
      CHARACTER*80 OLINE
      CHARACTER*30 CASE
      CHARACTER*(*) OPTFIL, PROMPT
      INTEGER ISTAT, NPRO, I, J, INUNIT, N, IOPT
      LOGICAL CHANGE
C
C-----------------------------------------------------------------------
C
C Get ready to interpret the option.  For each of the defined labels,
C convert the first two non-blank characters to a number.
C
 1000 NPRO = LEN(PROMPT)
      DO 1010 I=1,NOPT
         DO J=1,26
            IF (LBL(I)(J:J) .NE. ' ') GO TO 1010
         END DO
 1010 IDOPT(I)=ICNVRT(LBL(I)(J:J+1))
C
C If OPTFIL equals anything but 'KEYBOARD INPUT', then treat it as a
C filename and try to open it and read the commands from it, then
C display the results; if it happens to be daophot.opt, return quietly
C afterwards; otherwise, after displaying what you've read, go back
C and prompt for new keyboard input.
C
      IF (OPTFIL .EQ. 'KEYBOARD INPUT') THEN
         INUNIT = 5
         CALL TBLANK
         WRITE (6,610) (LBL(I), OPT(I), I=1,NOPT)
  610    FORMAT (1X, A26, ' =', F9.2, 4X, A26, ' =', F9.2)
         CALL TBLANK
         CHANGE = .FALSE.
         GO TO 2000
      END IF
C
      INUNIT = 2
      CALL INFILE (2, OPTFIL, ISTAT)
      IF (ISTAT .NE. 0) GO TO 3000
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Loop to read and interpret commands one at a time.
C
 2000 IF (INUNIT .EQ. 5) CALL INQUIR (PROMPT, NPRO)
      CALL RDCHAR (INUNIT, OLINE, N, ISTAT)
      IF (ISTAT .GT. 0) THEN
         IF (INUNIT .EQ. 5) RETURN
         GO TO 3000
      END IF
      IF (ISTAT .LT. 0) GO TO 2000
      IF (N .LE. 3) GO TO 3000
C
C Now interpret this command line.
C
C First find the equals sign.  Set J to its numerical position in the
C string.
C
      DO 2010 J=3,N
      IF (OLINE(J:J) .EQ. '=') GO TO 2020
 2010 CONTINUE
      CALL STUPID ('The equals sign is missing.')
      GO TO 2000
C
C Now identify the parameter.
C
 2020 IOPT=ICNVRT(OLINE(1:2))
      DO 2030 I=1,NOPT
      IF (IOPT .EQ. IDOPT(I)) GO TO 2040
 2030 CONTINUE
      CALL STUPID ('Unrecognized parameter: '//OLINE(1:J-1))
      GO TO 2000
C
C Now decode the numeric constant and set the appropriate option to this
C value.  Then go back and obtain the next command line.
C
 2040 OPT(I) = EXTRCT (OLINE(J+1:N), ISTAT)
      IF (ISTAT .NE. 0) THEN
         CALL STUPID
     .        ('Invalid numeric constant: '//OLINE(J+1:N))
         ELSE
            CHANGE = .TRUE.
      END IF
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C SECTION 3
C
C If file is neither 'daophot.opt' nor 'KEYBOARD INPUT', allow
C the user to enter modifications by hand.
C
 3000 IF (INUNIT .EQ. 2) THEN
         CALL CLFILE (2)
         IF (OPTFIL .NE. CASE('daophot.opt')) THEN
            CALL TBLANK
            WRITE (6,610) (LBL(I), OPT(I), I=1,NOPT)
            CALL TBLANK
            INUNIT = 5
            CHANGE = .FALSE.
            GO TO 2000
         END IF
      END IF
C
C Check that all parameters are acceptable.
C
      DO 3010 I=1,NOPT
      IF ((OPT(I) .LT. OMIN(I)) .OR. (OPT(I) .GT. OMAX(I))) GO TO 4000
 3010 CONTINUE
C
C Type out all parameter values.
C
      IF (CHANGE) THEN
         CALL TBLANK
         WRITE (6,610) (LBL(I), OPT(I), I=1,NOPT)
      END IF
      ISTAT = 0
      RETURN
C
C-----------------------------------------------------------------------
C
C SECTION 4
C
C There was some problem with one of the parameters.  Type it out and
C ask for the problem to be corrected.
C
 4000 CALL STUPID ('Value unacceptable --- please re-enter')
      CALL INQUIR (LBL(I)//' =', 50)
      CALL RDCHAR (5, OLINE, N, ISTAT)
      IF (N .LE. 0) CALL OOPS
      IF (ISTAT .GT. 0) GO TO 3000
      IF (ISTAT .LT. 0) GO TO 4000
C
C Decode the constant.
C
      OPT(I) = EXTRCT(OLINE(1:N), ISTAT)
      IF (ISTAT .NE. 0) GO TO 4000
      GO TO 3000
C
C-----------------------------------------------------------------------
C
C ERROR:  unable to open the disk file.
C
C If the file name is 'daophot.opt', no problem-- just check the
C default values for validity and return to the main program.
C Otherwise, type an error message and ask for a new file name.
C
 9000 IF (OPTFIL .EQ. CASE('daophot.opt')) GO TO 3000
      OLINE = 'Error opening input file '//OPTFIL
      CALL STUPID (OLINE)
      OPTFIL='KEYBOARD INPUT'
      GO TO 1000
C
      END!
C
C#######################################################################
C
      SUBROUTINE CHECK(LUN,NL)
C
C Check whether an input file has a DAOPHOT data file header.
C
      IMPLICIT NONE
      CHARACTER*4 FIRST4
      INTEGER NL, LUN
C
      NL=1
      READ(LUN,200)FIRST4
  200 FORMAT(A4)
      IF(FIRST4.NE.' NL ')GO TO 1000
      READ(LUN,201)NL
  201 FORMAT(1X,I2)
      READ(LUN,200)
      RETURN
 1000 REWIND(LUN)
      RETURN
      END!
