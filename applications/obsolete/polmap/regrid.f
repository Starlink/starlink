      SUBROUTINE REGRID(NPARAMS,PARAMS,STOKES_I,STOKES_Q,STOKES_QV,
     &                  STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine:
C
C
C     R E G R I D
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters:
C
C NPARAMS (<), PARAMS (<), STOKES_I (><), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (><), NPTS (><), OUT_LU (<)
C
C History:
C
C   May 1994 Created
C
C
C
C Rebins the current polarization by summing a number of adjacent bins.
C
C
C
C-
      IMPLICIT NONE
      INCLUDE 'array_size.inc'
      INTEGER NPARAMS
      INTEGER OUT_LU
      INTEGER NPTS,BINSIZE
      REAL PARAMS(*)
      REAL LAMBDA(*)
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL TMP_STOKES_I(MAXPTS)
      REAL TMP_STOKES_Q(MAXPTS)
      REAL TMP_STOKES_QV(MAXPTS)
      REAL TMP_STOKES_U(MAXPTS)
      REAL TMP_STOKES_UV(MAXPTS)
      REAL TMP_LAMBDA(MAXPTS)
      LOGICAL OK
      INTEGER I,J,K
      IF (NPARAMS.GT.1) THEN
       CALL WR_ERROR('Superfluous parameters ignored',OUT_LU)
      ENDIF
      IF (NPARAMS.EQ.0) THEN
       CALL GET_PARAM('Number of grid points per new bin',
     & PARAMS(1),OK,OUT_LU)
       IF (.NOT.OK) GOTO 666
      ENDIF
      BINSIZE=INT(PARAMS(1))
      IF ((BINSIZE.LT.1).OR.(BINSIZE.GT.NPTS)) THEN
       CALL WR_ERROR('Invalid bin size',OUT_LU)
       GOTO 666
      ENDIF
      I=0
      DO J=1,NPTS,BINSIZE
        IF ((J+BINSIZE-1).LT.NPTS) THEN
         I=I+1
         TMP_STOKES_I(I)=0.
         TMP_STOKES_Q(I)=0.
         TMP_STOKES_QV(I)=0.
         TMP_STOKES_U(I)=0.
         TMP_STOKES_UV(I)=0.
         TMP_LAMBDA(I)=(LAMBDA(J)+LAMBDA(J+BINSIZE-1))/2.
         DO K=J,J+BINSIZE-1
          TMP_STOKES_I(I)=TMP_STOKES_I(I)+STOKES_I(K)
          TMP_STOKES_Q(I)=TMP_STOKES_Q(I)+STOKES_Q(K)
          TMP_STOKES_QV(I)=TMP_STOKES_QV(I)+STOKES_QV(K)
          TMP_STOKES_U(I)=TMP_STOKES_U(I)+STOKES_U(K)
          TMP_STOKES_UV(I)=TMP_STOKES_UV(I)+STOKES_UV(K)
         ENDDO
         TMP_STOKES_I(I)=TMP_STOKES_I(I)/REAL(BINSIZE)
         TMP_STOKES_Q(I)=TMP_STOKES_Q(I)/REAL(BINSIZE)
         TMP_STOKES_QV(I)=TMP_STOKES_QV(I)/REAL(BINSIZE)**2
         TMP_STOKES_U(I)=TMP_STOKES_U(I)/REAL(BINSIZE)
         TMP_STOKES_UV(I)=TMP_STOKES_UV(I)/REAL(BINSIZE)**2
        ENDIF
      ENDDO
      NPTS=I
      DO I=1,NPTS
       LAMBDA(I)=TMP_LAMBDA(I)
       STOKES_I(I)=TMP_STOKES_I(I)
       STOKES_Q(I)=TMP_STOKES_Q(I)
       STOKES_QV(I)=TMP_STOKES_QV(I)
       STOKES_U(I)=TMP_STOKES_U(I)
       STOKES_UV(I)=TMP_STOKES_UV(I)
      ENDDO
 666  CONTINUE
      END
