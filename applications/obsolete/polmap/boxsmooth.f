      SUBROUTINE BOXSMOOTH(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C       B O X S M O O T H
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C

C Parameters:
C
C NPARAMS (<), PARAMS (<), STOKES_I (><), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (<), NPTS (<),OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Applies a box smooth to the current Stokes spectrum. Employs the
C Bevington smoothing algorithm
C
C
C-


      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NPARAMS
      REAL PARAMS(*)
      INTEGER OUT_LU,I
      INTEGER NPTS
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      LOGICAL OK
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Number of repetitions',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      DO I=1,INT(PARAMS(1))
       CALL SMOOTH(STOKES_I,NPTS)
       CALL SMOOTH(STOKES_Q,NPTS)
       CALL SMOOTH(STOKES_QV,NPTS)
       CALL SMOOTH(STOKES_U,NPTS)
       CALL SMOOTH(STOKES_UV,NPTS)
      ENDDO
 666  CONTINUE
      END
