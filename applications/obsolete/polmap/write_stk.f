      SUBROUTINE WRITE_STK(STK_NPTS,STK_LAMBDA,STK_STOKES_I,
     &STK_STOKES_Q,
     &STK_STOKES_QV,STK_STOKES_U,STK_STOKES_UV,TOP_STK,
     &NPTS,WA,IAR,
     &Q,QV,U,UV)
C+
C
C Subroutine: 
C
C   W R I T E _ S T K
C
C
C Author: Tim Harries (tjh@st-and.ac.uk)
C
C
C Parameters: 
C
C STK_NPTS (<), STK_LAMBDA (<), STK_STOKES_I (<),
C STK_STOKES_Q (<),
C STK_STOKES_QV (<), STK_STOKES_U (<), STK_STOKES_UV (<), TOP_STK (<),
C NPTS (<), WA (>), IAR (>), Q (>), QV (>), U (>), UV (>)
C
C History: 
C  
C   May 1994 Created
C 
C
C  
C
C Writes out a stack mapped previously
C
C
C-

      IMPLICIT NONE
C
      INCLUDE 'array_size.inc'
      INTEGER TOP_STK
      REAL STK_STOKES_I(MAXPTS,MAXSPEC)
      REAL STK_STOKES_Q(MAXPTS,MAXSPEC)
      REAL STK_STOKES_QV(MAXPTS,MAXSPEC)
      REAL STK_STOKES_U(MAXPTS,MAXSPEC)
      REAL STK_STOKES_UV(MAXPTS,MAXSPEC)
      REAL STK_LAMBDA(MAXPTS,MAXSPEC)
      INTEGER STK_NPTS(MAXSPEC)
      INTEGER I,J
C
      REAL IAR(MAXPTS,TOP_STK)
      REAL Q(MAXPTS,TOP_STK)
      REAL QV(MAXPTS,TOP_STK)
      REAL U(MAXPTS,TOP_STK)
      REAL UV(MAXPTS,TOP_STK)
      INTEGER NPTS(TOP_STK)
      REAL WA(MAXPTS,TOP_STK)

      DO J = 1,TOP_STK
       DO I = 1,MAXPTS
        IAR(I,J) = STK_STOKES_I(I,J)
        Q(I,J) = STK_STOKES_Q(I,J)
        QV(I,J) = STK_STOKES_QV(I,J)
        U(I,J) = STK_STOKES_U(I,J)
        UV(I,J) = STK_STOKES_UV(I,J)
        WA(I,J) = STK_LAMBDA(I,J)
       ENDDO
       NPTS(J) = STK_NPTS(J)
      ENDDO
      END
