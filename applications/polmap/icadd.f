      SUBROUTINE ICADD(NPARAMS,PARAMS,STOKES_I,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C     I C A D D
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<),PARAMS (<), STOKES_I (><), NPTS (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C  Adds a constant value to the Stokes I array of the current spectrum.
C
C
C-

      IMPLICIT NONE
      INTEGER NPTS,I,OUT_LU,NPARAMS
      REAL STOKES_I(*),PARAMS(*)
      LOGICAL OK
      IF (NPTS.EQ.0) THEN
       CALL WR_ERROR('No data in current arrays',OUT_LU)
       GOTO 666
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Constant',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      DO I=1,NPTS
       STOKES_I(I)=STOKES_I(I)+PARAMS(1)
      ENDDO
 666  CONTINUE
      END
