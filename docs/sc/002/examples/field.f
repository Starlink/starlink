      PROGRAM FIELD
      IMPLICIT NONE
*    Generate two data cubes.  One contains a Gaussian function, the
*    other a Gaussian on a sloping background.
*    A C Davenhall (Edinburgh), 3/12/95.
      DOUBLE PRECISION
     :  XCEN,   ! X Centre of distribution.
     :  YCEN,   ! Y   "    "       "      .
     :  ZCEN,   ! Z   "    "       "      .
     :  A,      ! Scale factor.
     :  B,      ! Exponential scale factor.
     :  C       ! Background slope.
      PARAMETER
     : (XCEN = 2.5D1,  YCEN = 2.5D1,  ZCEN = 2.5D1,
     :  A = 1.0D1,     B = 1.0D1,     C = 1.0D-1)
      DOUBLE PRECISION
     :  DIST,   ! Distance of point from centre.
     :  VALUE1, ! Value of Gaussian function at the point.
     :  VALUE2  ! Value of Gaussian on sloping background at the point.
      INTEGER
     :  I, J, K ! Loop indices.

      OPEN(UNIT=10, FILE='field.lis', STATUS='NEW')

      DO I = 1, 50
         DO J = 1, 50
            DO K = 1, 50
               DIST = SQRT( ((REAL(I) - XCEN)**2) +
     :           ((REAL(J) - YCEN)**2) +
     :           ((REAL(K) - ZCEN)**2) )

               VALUE1 = A*EXP(-DIST / B)
               VALUE2 = VALUE1 + (C * REAL(I))

               WRITE(10, '(1X, 0PF16.8, 2X, 0PF16.8)' ) VALUE1, VALUE2
            END DO
         END DO
      END DO

      CLOSE(UNIT=10)

      END
