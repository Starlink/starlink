      SUBROUTINE CWRANGE(WMIN,WMAX,WAUTOLIM,OUT_LU)
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
C WMIN (>),WMAX (>), WAUTOLIM (><), OUT_LU (<)
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
      REAL WMIN,WMAX,Y
      CHARACTER*1 CH
      LOGICAL WAUTOLIM
     
      WAUTOLIM = .FALSE.
      WRITE(OUT_LU,*) 'Press a key to set the minimum wavelength'
      CALL PGCURSE(WMIN,Y,CH)
      WMAX = WMIN
      WRITE(OUT_LU,*) 'Press a key to set the maximum wavelength'
      CALL PGCURSE(WMAX,Y,CH)
      END
