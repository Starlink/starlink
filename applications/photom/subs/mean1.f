************************************************************************

      SUBROUTINE MEAN1 ( SKY, NSKY, SMEAN, SIGMA )

*+
*  Name :
*     MEAN1
*
*  Purpose :
*     This calculates the mean and standard deviation from the input data.
*
*  Language :
*     FORTRAN
*
*  Invocation :
*     CALL MEAN1( SKY, NSKY, SMEAN, SIGMA )
*
*  Description :
*     This calculates the mean and standard deviation from the input data.
*
*  Arguments :
*     SKY( NSKY ) = REAL (Given)
*        Vector of data samples
*     NSKY = INTEGER (Given)
*        Dimension of data array
*     SMEAN = REAL (Returned)
*        Mean of sample
*     SIGMA = REAL (Returned)
*        Standard deviation of sample
*
*  Algorithm :
*     {algorithm_description}...
*
*  Deficiencies :
*     {routine_deficiencies}...
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}
*
*  History :
*     10-SEP-1989 (NE):
*        Original version.
*     {enter_changes_here}
*
*  Bugs :
*     {note_any_bugs_here}
*-

*  Type Definitions :
      IMPLICIT NONE

*  Arguments Given :
      INTEGER NSKY
      REAL SKY( NSKY )

*  Arguments Returned :
      REAL SMEAN
      REAL SIGMA

*  Local Variables :
      INTEGER J

      DOUBLE PRECISION D, SUM, SUM2
*.

*   Check that there are a positive number of points
      IF ( NSKY .LT. 1 ) THEN
         SMEAN = 0.0
         SIGMA = -1.0
         GOTO 99
      ENDIF

*   Calculate the mean
      SUM = 0.0D0
      DO J = 1, NSKY
         SUM = SUM + DBLE( SKY( J ) )
      ENDDO
      SMEAN = REAL( SUM / DBLE( NSKY ) )

*   Calculate the standard deviation
      SUM2 = 0.0D0
      DO J = 1, NSKY
         D = DBLE( SKY( J ) - SMEAN )
         SUM2 = SUM2 + D ** 2
      ENDDO
      SIGMA = SQRT( SUM2 / DBLE( NSKY ) )

  99  CONTINUE

      END

