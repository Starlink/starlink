      SUBROUTINE KPG1_PROWR( EL, ROW, IROW, ARRAY, STATUS )
*+
*  Name:
*     KPG1_PROWx
 
*  Purpose:
*     Put values into a row of a 2-dimensional array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_PROWx( EL, ROW, IROW, ARRAY, STATUS )
 
*  Description:
*     The routine enters values into a specified row of a 2-dimensional
*     array, the values being supplied in a separate 1-dimensional array
*     whose size matches the row size.
 
*  Arguments:
*     EL = INTEGER (Given)
*        Number of elements in a single row of the 2-d array.
*     ROW( EL ) = ? (Given)
*        Array of values to be inserted into the row.
*     IROW = INTEGER (Given)
*        The row number in the 2-d array into which the values are to be
*        inserted.
*     ARRAY( EL, * ) = ? (Given and Returned)
*        The 2-d array which is to recieve the new values. The declared
*        second dimension size of this array must not be less than
*        IROW. The values in other rows of this array are not altered.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type. Replace "x" in
*     the routine name by B, UB, W, UW, I, R or D as appropriate. The
*     data type of the ROW and ARRAY arrays should match the routine
*     being used.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     27-JUN-1990 (RFWS):
*        Original version.
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      INTEGER EL
      REAL ROW( EL )
      INTEGER IROW
 
*  Arguments Given and Returned:
      REAL ARRAY( EL, * )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter for array elements
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Copy the 1-d array into the appropriate row of the 2-d array.
      DO 1 I = 1, EL
         ARRAY( I, IROW ) = ROW( I )
1     CONTINUE
 
      END
