      SUBROUTINE INTEG(NPARAMS,PARAMS,STOKES_I,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C       I N T E G
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), STOKES_I (<), LAMBDA (<), NPTS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Integrates the Stokes I spectrum using a very simple trapeziodal
C integration.
C
C
C-

      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NPARAMS,NPTS,OUT_LU
      REAL PARAMS(*)
      REAL STOKES_I(*)
      REAL LAMBDA(*)
      REAL TOT
      INTEGER I
      IF (NPARAMS.GT.0) THEN
        CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPTS.EQ.0) THEN
        CALL WR_ERROR('Current arrays are empty',OUT_LU)
      ENDIF
      TOT=0.
      DO I=1,NPTS-1
       TOT=TOT+(LAMBDA(I+1)-LAMBDA(I))*(STOKES_I(I+1)+STOKES_I(I))/2.
      ENDDO
      WRITE(OUT_LU,'(1X,A,1PE9.3,0P)') 'Total: ',TOT
      END



