      PROGRAM PARTICLE
      IMPLICIT NONE
*    Generate a particle dataset, with the individual particles tracing
*    out positions on a cone.  The dependent variable varies
*    (indirectly) with the Z distance.
*    A C Davenhall (Edinburgh), 4/12/95.
      REAL
     :  DR,      ! Increment in radius.
     :  DTHETA,  !     "     "  theta.
     :  DZ,      !     "     "  z.
     :  PI       ! Pi.
      PARAMETER
     : (DR = 1.0E-1,  DTHETA = 1.0E-2, 
     :  DZ = 1.0E-1,  PI = 3.1415927E0)
      REAL
     :  R,       ! Radius.
     :  THETA,   ! Theta.
     :  X,       ! X coordinate.
     :  Y,       ! Y     "     .
     :  Z,       ! Z     "     .
     :  VALUE    ! Value of the point.
      INTEGER
     :  I, J, K  ! Loop indices.

      OPEN(UNIT=10, FILE='particle.lis', STATUS='NEW')

      DO I = 1, 10000
         R = REAL(I) * DR
         THETA = REAL(I) * DTHETA
         THETA = MOD(THETA, 2*PI)
         X = R * COS(THETA)
         Y = R * SIN(THETA)
         Z = REAL(I) * DZ

         VALUE = SIN(MOD( REAL(I)/5.0E3, 2*PI) )

         WRITE(10, '(0PE12.4, 1X, 0PE12.4, 1X, 0PE12.4, 1X, 0PE12.4)' )
     :     X, Y, Z, VALUE
      END DO

      CLOSE(UNIT=10)

      END
