      SUBROUTINE CONTLIST(CONT_ST,CONT_EN,NZONES,OUT_LU)
C+
C
C Subroutine:
C
C     C O N T L I S T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C CONT_ST (<), CONT_EN (<), NZONES (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C
C
C
C Lists the defined continuum zones.
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      REAL CONT_ST(*)
      REAL CONT_EN(*)
      INTEGER NZONES,I
C
10    FORMAT(1X,'Start: ',F7.1,' End: ',F7.1)
C
      IF (NZONES.EQ.0) THEN
       CALL WR_ERROR('No continuum bins defined',OUT_LU)
      ELSE
       DO I = 1,NZONES
        WRITE(OUT_LU,10) CONT_ST(I),CONT_EN(I)
       ENDDO
      ENDIF
      END
