      SUBROUTINE QUSWAP(STOKES_I,STOKES_Q,STOKES_QV,
     &                 STOKES_U,STOKES_UV,LAMBDA,NPTS,OUT_LU)
C+
C
C Subroutine: 
C
C   Q U S W A P
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C STOKES_I (<), STOKES_Q (><), STOKES_QV (><),
C STOKES_U (><), STOKES_UV (><), LAMBDA (><), NPTS (<), OUT_LU (<)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C  Swaps the q and u arrays of the current polarization spectrum
C
C
C-
      IMPLICIT NONE
      INTEGER OUT_LU
      INTEGER NPTS
C
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
C
C Misc.
C
      INTEGER I
      REAL TEMP,TEMPV
C
      IF (NPTS.EQ.0) THEN
        CALL WR_ERROR('Current arrays are empty',OUT_LU)
        GOTO 666
      ENDIF
C
      DO I=1,NPTS
       TEMP=STOKES_Q(I)
       TEMPV=STOKES_QV(I)
       STOKES_Q(I)=STOKES_U(I)
       STOKES_QV(I)=STOKES_UV(I)
       STOKES_U(I)=TEMP
       STOKES_UV(I)=TEMPV
      ENDDO
 666  CONTINUE
      END

       
