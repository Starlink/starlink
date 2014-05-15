      SUBROUTINE MEAN_COUNTS(STOKES_I,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C  M E A N _ C O U N T S
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C STOKES_I (<), NPTS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Find the mean counts per bin in the current polarization spectrum
C
C
C-

      IMPLICIT NONE
      INTEGER NPTS,I,OUT_LU
      REAL STOKES_I(*),TOT
      TOT=0.
      IF (NPTS.EQ.0) THEN
       CALL WR_ERROR('No data in current arrays',OUT_LU)
       GOTO 666
      ENDIF
      DO I=1,NPTS
       TOT=TOT+STOKES_I(I)
      ENDDO
      TOT=TOT/REAL(NPTS)
      WRITE(OUT_LU,'(1X,A,1PE12.5,0P)') 'Mean Counts per Bin: ',TOT
 666  CONTINUE
      END
