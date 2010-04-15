      PROGRAM COVARTEST
*
*  Test program for PDA covariance and normal scores routines.
*
*  Written by: P.W. Draper 19th February 1997.
*

      IMPLICIT NONE

      INTEGER NMAX
      PARAMETER ( NMAX = 6 )

      DOUBLE PRECISION PDA_V11
      EXTERNAL PDA_V11

      DOUBLE PRECISION EXP1, EXP2, SUMSQ, V11
      INTEGER I, IFAIL, J, N2, N, K
      DOUBLE PRECISION PP(NMAX), VEC(NMAX,NMAX), PVEC(NMAX*(NMAX+1)/2)

      DO N = 2, NMAX
         N2 = N/2
         CALL PDA_NSCOR(PP,N,N2,IFAIL)

         EXP1 = PP( 1 )
         IF ( N .EQ. 2 ) THEN
            EXP2 = -EXP1
         ELSE IF ( N .EQ. 3 ) THEN
            EXP2 = 0.0D0
         ELSE
            EXP2 = PP( 2 )
         END IF
         SUMSQ = 0.0D0
         DO I = 1, N2
            SUMSQ = SUMSQ + PP( I ) * PP( I )
         END DO
         SUMSQ = 2.0D0 * SUMSQ

         V11 = PDA_V11( N, IFAIL )
         CALL PDA_COVMAT( VEC, N, NMAX, V11, EXP1, EXP2, SUMSQ, IFAIL )

         WRITE(*,*)
         WRITE(*,*) 'Sample size = ', N
         WRITE(*,*) 'Variance-covariance matrix'
         DO J = 1, N
            IF ( N .EQ. 2 ) THEN
               WRITE(*,'(1X,2F8.4)') (VEC(I,J), I=1,J)
            ELSE IF ( N .EQ. 3 ) THEN
               WRITE(*,'(1X,3F8.4)') (VEC(I,J), I=1,J)
            ELSE IF ( N .EQ. 4 ) THEN
               WRITE(*,'(1X,4F8.4)') (VEC(I,J), I=1,J)
            ELSE IF ( N .EQ. 5 ) THEN
               WRITE(*,'(1X,5F8.4)') (VEC(I,J), I=1,J)
            ELSE IF ( N .EQ. 6 ) THEN
               WRITE(*,'(1X,6F8.4)') (VEC(I,J), I=1,J)
            END IF
         END DO

*  Pack the vector into a NAG-like format (just an example).
         K = 1
         DO J = 1, N
            DO I = 1, J
               PVEC(K) = VEC(I,J)
               K = K + 1
            END DO
         END DO
      END DO
      END
