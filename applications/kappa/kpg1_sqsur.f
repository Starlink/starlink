      SUBROUTINE KPG1_SQSUR( EL, ARRAY, SUMSQ, STATUS )
*+
*  Name:
*     KPG1_SQSUx
 
*  Purpose:
*     Finds the sum of the squares of an array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_SQSUx( EL, ARRAY, SUMSQ, STATUS )
 
*  Description:
*     This routine sums the squares of a supplied array and return the
*     sum.  This might used to sum the residuals for a minimisation.
*     Bad values are ignored.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the array to sum.
*     ARRAY( EL ) = ? (Given)
*        The array whose sqaured values are to be summed.
*     SUMSQ = ? (Returned)
*        The sum of the squared array values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  [optional_subroutine_items]...
*  Notes:
*     There is a routine for integer, real, and double precision data
*     types: replace "x" in the routine name by I, R, or D respectively.
*     The ARRAY and SUMSQ arguments supplied to the routine must have
*     the data type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1996 January 31 (MJC):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
 
*  Arguments Given:
      INTEGER EL
      REAL ARRAY( EL )
 
*  Arguments Returned:
      REAL SUMSQ
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
      DOUBLE PRECISION SUM       ! Summation of squares
      DOUBLE PRECISION V         ! Array value
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise the summation.  Use a double-precision value.
      SUM = 0.0D0
 
*  Loop through all array elements.
      DO I = 1, EL
 
*  Test for bad values.
         IF ( ARRAY( I ) .NE. VAL__BADR ) THEN
 
*  Form the sum after converting the array value to double precision.
            V = NUM_RTOD( ARRAY( I ) )
            SUM = SUM + V * V
         END IF
      END DO
 
*  Check for a sum out of range.  This is for the integer instantiation.
      IF ( SUM .GT. NUM_RTOD( VAL__MAXR ) ) THEN
         SUMSQ = VAL__BADR
      ELSE
         SUMSQ = NUM_DTOR( SUM )
      END IF
 
      END
