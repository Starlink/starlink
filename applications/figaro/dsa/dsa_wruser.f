C+
C                           D S A _ W R U S E R
C
C  Routine name:
C     DSA_WRUSER
C
C  Function:
C     Outputs a message string to the user.
C
C  Description:
C     DSA_WRUSER outputs a message string to the user, buffering it until
C     either a line's worth of output has been received or until a string
C     containing a newline code (`backslash n', as in C's printf) is
C     received.  So a call to this routine does not necessarily result in
C     a single line being output immediately to the user.  Output is
C     through the Figaro parameter system (the routine PAR_WRUSER).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_WRUSER (STRING)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) STRING      (Fixed string,descr) The string to be output.  Words
C                     from STRING are buffered and output when the 80 char
C                     buffer is full, or when a newline (backslash n) is
C                     found in the string.  All spaces in STRING - including
C                     trailing spaces - are treated as significant.
C
C  External variables used -
C     Common variables used internally by the DSA_ package.
C
C  External subroutines / functions used:  PAR_WRUSER
C
C  Prior requirements:
C     DSA_OPEN should have been called to initialise the DSA_ system.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 29th August 1992
C-
C  Common variable details:
C     (!) BUFF_PTR      (Integer) Last buffer position used.
C     (>) BUFF_LEN      (Integer parameter) Length of buffer.
C     (!) MESS_BUFF     (Fixed string) Output buffer.
C     (!) LAST_BLANK    (Integer) Position of last blank char in buffer.
C
C  Subroutine / function details:
C     PAR_WRUSER    Write a single string out to the user.
C
C  History:
C     10th June 1987  Original version.  KS / AAO.
C     21st Aug 1992   Automatic portability modifications
C                     ("INCLUDE" syntax etc) made. KS/AAO
C     23rd Aug 1992   Modified to remove the backslash character from the
C                     code for portablility reasons. KS/AAO.
C     29th Aug 1992   "INCLUDE" filenames now upper case. KS/AAO
C+
      SUBROUTINE DSA_WRUSER (STRING)
C
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) STRING
C
C     DSA_ system common
C
      INCLUDE 'DSA_COMMON'
C
C     Local variables
C
      CHARACTER BACKSLASH*1            ! Holds the backslash character
      CHARACTER CHR*1                  ! Character from buffer
      LOGICAL ESCAPE                   ! Flags in middle of escape sequence
      INTEGER I                        ! Loop index through chars in STRING
      INTEGER STATUS                   ! PAR_WRUSER status return - ignored
C
C     Set the backslash character - some UNIX compilers don't like to see
C     this is Fortran code, so this works around it - at the expense of
C     assuming ASCII characters, I'm afraid.
C
      BACKSLASH=CHAR(92)
C
C     Loop through all the characters in the buffer
C
      ESCAPE=.FALSE.
      DO I=1,LEN(STRING)
         IF (BUFF_PTR.GE.BUFF_LEN) THEN
C
C           Buffer full, output up to and including last blank.  If
C           there are no blanks at all in the buffer, output the lot.
C
            IF (LAST_BLANK.EQ.0) THEN
               CALL PAR_WRUSER(MESS_BUFF,STATUS)
               BUFF_PTR=0
            ELSE
               CALL PAR_WRUSER(MESS_BUFF(:LAST_BLANK),STATUS)
               MESS_BUFF(1:)=MESS_BUFF(LAST_BLANK+1:)
               BUFF_PTR=BUFF_LEN-LAST_BLANK
            END IF
            LAST_BLANK=0
         END IF
C
C        Get next character
C
         CHR=STRING(I:I)
         IF (ESCAPE) THEN
C
C           Previous character was a backslash, so we are in the middle of an
C           escape sequence.  If this is a newline, flush the buffer.
C           Otherwise, just ignore the sequence.
C
            IF ((CHR.EQ.'N').OR.(CHR.EQ.'n')) THEN
               CALL PAR_WRUSER(MESS_BUFF(:BUFF_PTR),STATUS)
               BUFF_PTR=0
               LAST_BLANK=0
            END IF
            ESCAPE=.FALSE.
         ELSE
C
C           If character is a backslash then set escape flag.  Otherwise,
C           stuff it into the buffer.
C
            IF (CHR.EQ.BACKSLASH) THEN
               ESCAPE=.TRUE.
            ELSE
               BUFF_PTR=BUFF_PTR+1
               MESS_BUFF(BUFF_PTR:BUFF_PTR)=CHR
               IF (CHR.EQ.' ') LAST_BLANK=BUFF_PTR
            END IF
         END IF
      END DO
C
      END
