      SUBROUTINE WRITE_IT(NPTS,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &STOKES_U,STOKES_UV,N,IAR,Q,QV,U,UV,WA)
C+
C
C Subroutine: 
C
C    W R I T E _ I T 
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPTS (<), LAMBDA (<), STOKES_I (<), STOKES_Q (<), STOKES_QV (<),
C STOKES_U (<), STOKES_UV (<), N (>), IAR (>), Q (>), QV (>), U (>),
C UV (>), WA (>)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C
C Writes out the array mapped previously
C
C
C
C
C-
      INTEGER NPTS
      REAL STOKES_I(*)
      REAL STOKES_Q(*)
      REAL STOKES_QV(*)
      REAL STOKES_U(*)
      REAL STOKES_UV(*)
      REAL LAMBDA(*)
      INTEGER N,I
C
      REAL Q(N),U(N),QV(N),UV(N),WA(N),IAR(N)

      DO I = 1,N
       IAR(I) = STOKES_I(I)
       WA(I) = LAMBDA(I)
       Q(I) = STOKES_Q(I)
       QV(I) = STOKES_QV(I)
       U(I) = STOKES_U(I)
       UV(I) = STOKES_UV(I)
      ENDDO
      END
