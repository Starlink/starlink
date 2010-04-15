      SUBROUTINE LS(STK_TITLE,TOP_STK,OUT_LU)
C+
C
C Subroutine:
C
C        L S
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C STK_TITLE (<), TOP_STK (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C This subroutine lists the stack contents in a page format
C
C
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
C
C The stack arrays
C
      CHARACTER*80 STK_TITLE(MAXSPEC)
      INTEGER TOP_STK,OUT_LU,I,K
C
      IF (TOP_STK.EQ.0) THEN
         CALL WR_ERROR('Stack empty',OUT_LU)
         GOTO 666
      ENDIF
C
      K=0
      DO I=1,TOP_STK
       K=K+1
       WRITE(OUT_LU,'(I3,1X,A20,$)') I,STK_TITLE(I)(1:20)
       IF (K.EQ.3) THEN
        WRITE(OUT_LU,*) ' '
        K=0
       ENDIF
      ENDDO
      IF (K.NE.3) THEN
       WRITE(OUT_LU,*) ' '
      ENDIF
666   CONTINUE
      END

