      SUBROUTINE CPRANGE(WMIN,WMAX,PMIN,PMAX,PAUTOLIM,PFLUX,
     &                   QU_TRIPLOT,OUT_LU)
C+
C
C Subroutine: 
C
C    C P R A N G E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C WMIN (<), WMAX (<), PMIN (>), PMAX (>), PAUTOLIM (><), PFLUX (><),
C QU_TRIPLOT (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Sets the range of the polarization panel interactively
C
C
C-

C
C
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL WMIN,WMAX,PMIN,PMAX,X
      CHARACTER*1 CH
      LOGICAL PAUTOLIM,PFLUX,QU_TRIPLOT
C
      IF (QU_TRIPLOT) THEN
       CALL WR_ERROR('QU panel is selected',OUT_LU)
       GOTO 666
      ENDIF
C
      PAUTOLIM = .FALSE.
      IF (PFLUX) THEN
       CALL PGVPORT(0.1,0.9,0.3,0.5)
       ELSE
       CALL PGVPORT(0.1,0.9,0.4,0.65)
      ENDIF
      CALL PGWINDOW(WMIN,WMAX,PMIN,PMAX)
      WRITE(OUT_LU,*) 'Press a key to set the minimum polarization'
      X=WMIN
      CALL PGCURSE(X,PMIN,CH)
      PMAX = PMIN
      WRITE(OUT_LU,*) 'Press a key to set the maximum polarization'
      CALL PGCURSE(X,PMAX,CH)
 666  CONTINUE
      END
