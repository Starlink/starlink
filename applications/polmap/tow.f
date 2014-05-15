      SUBROUTINE TOW(NPARAMS,PARAMS,NPTS,LAMBDA,XLAB,OUT_LU)
C+
C
C Subroutine:
C
C        T O W
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), NPTS (<), LAMBDA (><), XLAB (>), OUT_LU (<)
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
C Converts current spectrum to wavelength space
C
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPARAMS,NPTS
      REAL PARAMS(*)
      REAL LAMBDA(*)
      REAL C_SPEED
      INTEGER I
      CHARACTER*(*) XLAB
      LOGICAL OK
C
      XLAB='Wavelength (\\A)'
C
      C_SPEED = 2.99792458E5
      IF (NPARAMS.GT.1) THEN
        CALL WR_ERROR('Additional parameters ignored',OUT_LU)
      ENDIF
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Wavelength',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
C
      IF (PARAMS(1).EQ.0.) THEN
       CALL WR_ERROR('Invalid wavelength',OUT_LU)
       GOTO 666
      ENDIF
C
      DO I = 1,NPTS
       LAMBDA(I) = PARAMS(1)*(1.+LAMBDA(I)/C_SPEED)
      ENDDO
666   CONTINUE
      END
