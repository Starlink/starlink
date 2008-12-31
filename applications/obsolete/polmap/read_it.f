      SUBROUTINE READ_IT(NPTS,LAMBDA,STOKES_I,STOKES_Q,STOKES_QV,
     &STOKES_U,STOKES_UV,N,IAR,Q,QV,U,UV,WA)
C+
C
C Subroutine: 
C
C   R E A D _ I T
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C Parameters: 
C
C NPTS (>), LAMBDA (>), STOKES_I (>), STOKES_Q (>), STOKES_QV (>),
C STOKES_U (>), STOKES_UV (>), N (<), IAR (<), Q (<), QV (<), U (<)
C UV (<), WA (<)
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
C Reads polarization spectrum into previously mapped arrays
C
C
C-
C
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
       STOKES_I(I) = IAR(I)
       LAMBDA(I) = WA(I)
       STOKES_Q(I) = Q(I)
       STOKES_QV(I) = QV(I)
       STOKES_U(I) = U(I)
       STOKES_UV(I) = UV(I)
      ENDDO
      END
