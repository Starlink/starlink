      SUBROUTINE KPG1_HCOND( M, N, IN, STATUS )
*+
*  Name:
*     KPG1_HCONx
 
*  Purpose:
*     Take the complex conjugate of an Hermitian image.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_HCONx( M, N, IN, STATUS )
 
*  Description:
*     The complex conjugate of the supplied Hermitian image is returned.
*     See routine KPG1_HMLTx for more information on Hermitian images.
 
*  Arguments:
*     M = INTEGER (Given)
*        Number of columns.
*     N = INTEGER (Given)
*        Number of rows.
*     IN( N, M ) =? (Given and Returned)
*        On input it is the Hermitian image.  On exit it holds the
*        complex conjugate of the supplied array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for processing single- and double-precision
*     arrays; replace "x" in the routine name by R or D as appropriate.
*     The data type of the IN argument must match the routine used.
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     23-FEB-1995 (DSB):
*        Original version.
*     1995 March 22 (MJC):
*        Made generic, corrected typo's, removed long lines and tabs.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      INTEGER M
      INTEGER N
 
*  Arguments Given and Returned:
      DOUBLE PRECISION IN( M, N )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER J                ! Column index
      INTEGER K                ! Row index
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Negate the imaginary terms in each row (they are stored at the
*  end of each row, following the real terms).
      DO K = 1, N
         DO J = M/2 + 2, M
            IN( J, K ) = -IN( J, K )
         END DO
      END DO
 
*  Negate the imaginary terms in each column (they are stored at the
*  high end of each column, above the real terms).
      DO J = 1, M
         DO K = N/2 + 2, N
            IN( J, K ) = -IN( J, K )
         END DO
      END DO
 
      END
