      SUBROUTINE CTRANGE(WMIN,WMAX,TMIN,TMAX,TAUTOLIM,PFLUX,
     &                   QU_TRIPLOT,OUT_LU)
C+
C
C Subroutine: 
C
C
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C WMIN (<), WMAX (<), TMIN (>), TMAX (>), TAUTOLIM (><), PFLUX (<),
C QU_TRIPLOT (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C  Sets the position angle panel range interactively
C
C
C
C-
C
C
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL WMIN,WMAX,TMIN,TMAX,X
      CHARACTER*1 CH
      LOGICAL TAUTOLIM,PFLUX,QU_TRIPLOT
C
      IF (QU_TRIPLOT) THEN
       CALL WR_ERROR('QU panel is selected',OUT_LU)
       GOTO 666
      ENDIF
C
      TAUTOLIM = .FALSE.
      IF (PFLUX) THEN
       CALL PGVPORT(0.1,0.9,0.7,0.9)
       ELSE
       CALL PGVPORT(0.1,0.9,0.65,0.9)
      ENDIF
      CALL PGWINDOW(WMIN,WMAX,TMIN,TMAX)
      WRITE(OUT_LU,*) 'Press a key to set the minimum PA'
      X=WMIN
      CALL PGCURSE(X,TMIN,CH)
      TMAX = TMIN
      WRITE(OUT_LU,*) 'Press a key to set the maximum PA'
      CALL PGCURSE(X,TMAX,CH)
 666  CONTINUE
      END


