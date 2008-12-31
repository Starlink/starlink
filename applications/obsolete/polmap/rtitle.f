      SUBROUTINE RTITLE(CPARAM,TITLE,OUT_LU)
C+
C
C Subroutine: 
C
C  R T I T L E
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C CPARAM (<), TITLE (>), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C Titles the current polarization spectrum
C
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      CHARACTER*80 CPARAM
      CHARACTER*80 TITLE
      TITLE = CPARAM
      END
