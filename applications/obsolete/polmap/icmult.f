      SUBROUTINE ICMULT(NPARAMS,PARAMS,NPTS,STOKES_I,STOKES_Q,
     &                  STOKES_QV,STOKES_U,STOKES_UV,SYMB,OUT_LU)
C+
C
C Subroutine:
C
C    I C M U L T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<),PARAMS (<), NPTS (<), STOKES_I (><), STOKES_Q (><),
C STOKES_QV (><), STOKES_U (><), STOKES_UV (><), SYMB (<), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C
C Multiplies the I, Q and U arrays by a constant.
C
C
C-

C
      IMPLICIT NONE
C
      INTEGER NPARAMS,NPTS,OUT_LU,I
      REAL PARAMS(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL FAC
      CHARACTER*1 SYMB
      LOGICAL OK
C
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Factor',PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
       NPARAMS=1
      ENDIF
C
      IF (NPTS.EQ.0) THEN
        CALL WR_ERROR('Current arrays are empty',OUT_LU)
        GOTO 666
       ENDIF
C
      FAC=PARAMS(1)
C
      IF (SYMB.EQ.'/') THEN
        FAC=1./FAC
      ENDIF
C
      DO I = 1,NPTS
        STOKES_I(I)=STOKES_I(I)*FAC
        STOKES_Q(I)=STOKES_Q(I)*FAC
        STOKES_QV(I)=STOKES_QV(I)*FAC**2
        STOKES_U(I)=STOKES_U(I)*FAC
        STOKES_UV(I)=STOKES_UV(I)*FAC**2
      ENDDO
C
 666  CONTINUE
      END

