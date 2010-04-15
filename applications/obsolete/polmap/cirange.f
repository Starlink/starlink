      SUBROUTINE CIRANGE(WMIN,WMAX,IMIN,IMAX,IAUTOLIM,PFLUX,OUT_LU)
C+
C
C Subroutine:
C
C     C I R A N G E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C WMIN (<), WMAX (<), IMIN (><), IMAX (><), IAUTOLIM (><),
C PFLUX (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C This routine sets the wavelength range for the plots using the cursor.
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL WMIN,WMAX,IMIN,IMAX,X
      CHARACTER*1 CH
      LOGICAL IAUTOLIM,PFLUX

      IAUTOLIM = .FALSE.
      IF (PFLUX) THEN
       CALL PGVPORT(0.1,0.9,0.1,0.3)
       ELSE
       CALL PGVPORT(0.1,0.9,0.1,0.4)
      ENDIF
      CALL PGWINDOW(WMIN,WMAX,IMIN,IMAX)
      WRITE(OUT_LU,*) 'Press a key to set the minimum intensity'
      X=WMIN
      CALL PGCURSE(X,IMIN,CH)
      IMAX = IMIN
      WRITE(OUT_LU,*) 'Press a key to set the maximum intensity'
      CALL PGCURSE(X,IMAX,CH)
      END
