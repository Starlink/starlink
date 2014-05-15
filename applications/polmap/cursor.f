      SUBROUTINE CURSOR(OUT_LU)
C+
C
C Subroutine:
C
C   C U R S O R
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Interface for the PGCURSE command
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL X,Y
      CHARACTER*1 CH
      X = 0.
      Y = 0.
      CH='X'
      WRITE(OUT_LU,*) 'Hit Q to quit'
      WRITE(OUT_LU,*) ' '
      DO WHILE ((CH.NE.'q').AND.(CH.NE.'Q'))
       CALL PGCURSE(X,Y,CH)
       IF ((CH.NE.'q').AND.(CH.NE.'Q')) THEN
        WRITE(OUT_LU,*) x,y
       ENDIF
      ENDDO
      END
