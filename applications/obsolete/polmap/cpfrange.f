      SUBROUTINE CPFRANGE(WMIN,WMAX,PFMIN,PFMAX,PFAUTOLIM,PFLUX,
     &                   QU_TRIPLOT,OUT_LU)
C+
C
C Subroutine: 
C
C        C P F R A N G E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C WMIN (<), WMAX (<), PFMIN (>), PFMAX (>), PFAUTOLIM (><), PFLUX (><),
C QU_TRIPLOT (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Sets the range of the polarized flux panel interactively
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL WMIN,WMAX,PFMIN,PFMAX,X
      CHARACTER*1 CH
      LOGICAL PFAUTOLIM,PFLUX,QU_TRIPLOT
C
      IF (.NOT.PFLUX) THEN
       CALL WR_ERROR('Polarized flux panel is not selected',OUT_LU)
       GOTO 666
      ENDIF
C
      PFAUTOLIM = .FALSE.
      CALL PGVPORT(0.1,0.9,0.5,0.7)
      CALL PGWINDOW(WMIN,WMAX,PFMIN,PFMAX)
      WRITE(OUT_LU,*) 'Press a key to set the minimum Pflux'
      X=WMIN
      CALL PGCURSE(X,PFMIN,CH)
      PFMAX = PFMIN
      WRITE(OUT_LU,*) 'Press a key to set the maximum Pflux'
      CALL PGCURSE(X,PFMAX,CH)
 666  CONTINUE
      END



