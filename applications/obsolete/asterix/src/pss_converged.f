*+  PSS_CONVERGED - Have two values converged to required accuracy
      LOGICAL FUNCTION PSS_CONVERGED( A, B )
*
*    Description :
*    History :
*
*      3 Feb 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL       A,B
*-

      IF ( GE_CONVERGE_METHOD .EQ. PSS__ABSOLUTE ) THEN

        PSS_CONVERGED = (ABS(A-B).LT.GE_CONVERGE_TOL)

      ELSE IF ( GE_CONVERGE_METHOD .EQ. PSS__RELATIVE ) THEN
        IF ( B .NE. 0.0 ) THEN
          PSS_CONVERGED = (ABS((A-B)/B).LT.GE_CONVERGE_TOL)
        ELSE IF ( A .NE. 0.0 ) THEN
          PSS_CONVERGED = (ABS((A-B)/A).LT.GE_CONVERGE_TOL)
        ELSE
          PSS_CONVERGED = .TRUE.
        END IF

      END IF

      END
