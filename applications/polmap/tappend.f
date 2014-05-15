      SUBROUTINE TAPPEND(CPARAM,TITLE,OUT_LU)
C+
C
C Subroutine:
C
C  T A P P E N D
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C CPARAM (<), TITLE (><), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Appends a string to the title of the current spectrum
C
C
C-

      IMPLICIT NONE
      CHARACTER*(*) CPARAM,TITLE
      CHARACTER*80 TEMP
      INTEGER OUT_LU
      INTEGER I
      LOGICAL FOUND
C
      CALL SSTRIP(CPARAM)
      FOUND=.FALSE.
      I=80
      DO WHILE (.NOT.FOUND)
       IF (TITLE(I:I).NE.' ') THEN
        FOUND=.TRUE.
       ENDIF
       I=I-1
       IF (I.EQ.0) THEN
        FOUND=.TRUE.
        I=-1
       ENDIF
      ENDDO
      IF (I.EQ.-1) THEN
       TITLE=CPARAM
       ELSE
       TEMP=TITLE(:(I+1))//' '//CPARAM
      ENDIF
      TITLE=TEMP
      END





