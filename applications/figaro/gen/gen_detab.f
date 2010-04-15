C+
C                               G E N _ D E T A B
C
C  Routine name:
C     GEN_DETAB
C
C  Function:
C     Removes tabs from a line of text.
C
C  Description:
C     This routine is passed a string which may contain tab characters.
C     It is also passed a list of the tab settings (for standard 8 col
C     tabs, for example, these values are I*8+1, ie 9,17,25,33,41,..).
C     It returns a string in which the tab characters have been replaced
C     by the appropriate number of blank spaces.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL GEN_DETAB (IN,OUT,NTAB,TABS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) IN         (Fixed string,descr) The string to be detabbed
C     (>) NTAB       (Integer,ref) The number of specified tab positions
C     (>) TABS       (Integer array,ref) The tab positions
C     (<) OUT        (Fixed string,descr) The resulting detabbed string
C
C  External variables used:  None.
C
C  External subroutines / functions used:  None.
C
C  Prior requirements:  None.
C
C  Support: Keith Shortridge, AAO
C
C  Version date: 11th Jan 1990.
C
C  Note:
C     Once the list of tab positions is exhausted, any further tabs
C     in the input are ignored.  The output string may be truncated
C     if OUT is not long enough - if so, there is no indication of this.
C-
C  History:
C     1st March 1989   Original version.  KS / AAO.
C     11th  Jan 1990   Name changed from DETAB_LINE to GEN_DETAB.  KS/AAO.
C+
      SUBROUTINE GEN_DETAB (IN,OUT,NTAB,TABS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NTAB, TABS(NTAB)
      CHARACTER*(*) IN, OUT
C
C     Local variables
C
      INTEGER I              ! Loop index through chars in IN.
      INTEGER ITAB           ! Index of current tab position
      INTEGER LENGTH         ! Chars already in OUT
      LOGICAL TABINC         ! Indicates TABPOS is still too low
      LOGICAL TABOK          ! Indicates TABPOS is still valid
      INTEGER TABPOS         ! Position of next available tab stop
C
C     Value of the tab character
C
      CHARACTER TAB
*      PARAMETER (TAB=CHAR(9))
C
      TAB=CHAR(9)
      ITAB=0
      OUT=' '
      LENGTH=0
      TABPOS=0
      DO I=1,LEN(IN)
         LENGTH=LENGTH+1
         IF (LENGTH.GT.LEN(OUT)) GO TO 500   ! Break from I loop
         IF (IN(I:I).NE.TAB) THEN
            OUT(LENGTH:LENGTH)=IN(I:I)
         ELSE
            TABOK=TABPOS.GT.LENGTH
            IF (.NOT.TABOK) THEN
               IF (ITAB.LT.NTAB) THEN
                  TABINC=.TRUE.
                  DO WHILE (TABINC)
                     ITAB=ITAB+1
                     IF (ITAB.GT.NTAB) THEN
                        TABINC=.FALSE.
                        TABPOS=LENGTH
                     ELSE
                        TABPOS=TABS(ITAB)
                        TABINC=TABPOS.LE.LENGTH
                     END IF
                  END DO
               END IF
            END IF
            LENGTH=TABPOS-1
         END IF
      END DO
C
  500 CONTINUE
      END
