      SUBROUTINE KPG1_MDETD( N, EL, ARRAY, WORK1, WORK2, DET, STATUS )
*+
*  Name:
*     KPG1_MDETx
 
*  Purpose:
*     Computes the determinant of a matrix.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_MDETX( N, EL, ARRAY, WORK1, WORK2, DET, STATUS )
 
*  Description:
*     This routine calculates the determinant of an arbitrary n-by-n
*     matrix.  It LU decomposes the matrix and then forms the product
*     of the diagonals by summing their logarithms to base 10.
*     Logarithmic calculations are used to prevent overflows. A bad
*     status is returned when the matrix is singular or the determinant
*     is too large to store in a DOUBLE PRECISION-number representation.
 
*  Arguments:
*     N = INTEGER (Given)
*        The number of rows and columns in the matrix whose determinant
*        is to be derived.
*     EL = INTEGER (Given)
*        The size of the first dimension of the array as declared in the
*        calling routine.  This should be at least equal to N.
*     ARRAY( EL, N ) = ? (Given and Returned)
*        On input this is the matrix whose determinant is to be found.
*        Since the LU decomposition is performed in situ, the input
*        values are lost.  (On exit it is the LU decomposed form of the
*        rowwise-permuted input ARRAY, with the diagonals being part of
*        the upper triangular matrix.)
*     WORK1( N ) = INTEGER (Returned)
*        Workspace for the LU decomposition.
*     WORK2( N ) = ? (Returned)
*        Workspace for the LU decomposition.
*     DET = ? (Returned)
*        The determinant of matrix ARRAY.  It is set to 1.0 when the
*        matrix is singular.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for the data types real or double precision:
*     replace "x" in the routine name by R or D respectively, as
*     appropriate.  The matrix and the SECOND workspace should have
*     this data type as well.
 
*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1993 March 3 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
 
*  Arguments Given:
      INTEGER N
      INTEGER EL
 
*  Arguments Given and Returned:
      DOUBLE PRECISION ARRAY( EL, N )
 
*  Arguments Returned:
      INTEGER WORK1
      DOUBLE PRECISION WORK2
      DOUBLE PRECISION DET
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      DOUBLE PRECISION DETSIG              ! Sign of the determinant
      LOGICAL EVEN               ! If true the rows were interchanged an
                                 ! even number of times
      INTEGER I                  ! Loop counter
      DOUBLE PRECISION VALMAX              ! Logarithm of the maximum value
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Perform the LU decomposition.
      CALL KPG1_LUDCD( N, EL, ARRAY, WORK1, WORK2, EVEN, STATUS )
 
*  Initialise the sign of the product of the diagonal elements.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( EVEN ) THEN
            DETSIG = 1.0D0
         ELSE
            DETSIG = -1.0D0
         END IF
 
*  Assign the maximum logarithmic value.
         VALMAX = LOG10( VAL__MAXD )
 
*  Form the logarithmic determinant, recording the product of the signs.
         DET = 0.0D0
         DO I = 1, N
            DET = DET + LOG10( ABS( ARRAY( I, I ) ) )
            DETSIG = DETSIG * SIGN( 1.0D0, ARRAY( I, I ) )
         END DO
 
*  Watch and report any overflows.
         IF ( DET .GT. VALMAX ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'DET', DET )
            CALL ERR_REP( 'KPG1_MDETx_OVERFLOW',
     :        'The value of a matrix determinant is too large to '/
     :        /'represent. Its base 10 logarithm is ^DET.', STATUS )
 
*  Reform the determinant.
         ELSE
            DET = DETSIG * 10.0D0 ** DET
         END IF
 
*  Check that the number will not overflow.
      ELSE
         DET = 1.0D0
      END IF
 
      END
