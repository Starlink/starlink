      SUBROUTINE KPG1_UNZ2R( EL, IN, OUT1, OUT2, STATUS )
*+
*  Name:
*     KPG1_UNZ2x
 
*  Purpose:
*     Unzips a 2-dimensional co-ordinate array into two 1-dimensional
*     arrays.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_UNZ2x( EL, IN, OUT1, OUT2, STATUS )
 
*  Description:
*     This routine takes an array of dimension 2 by EL elements and
*     puts the two columns into separate arrays.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of lines in the input array, and elements in each of
*        the output arrays.
*     IN( 2, EL ) = ? (Given)
*        The array to be `unzipped'.
*     OUT1( EL ) = ? (Returned)
*        The vector to contain first column of the input array.
*     OUT2( EL ) = ? (Returned)
*        The vector to contain second column of the input array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for real and double-precision data types:
*     replace "x" in the routine name by R or D respectively.  The
*     routine arguments IN, OUT1, and OUT2 must have the data type
*     specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1995 April 12 (MJC):
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
      REAL IN( 2, EL )
 
*  Arguments Returned:
      REAL OUT1( EL )
      REAL OUT2( EL )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Loop for every output element.  Copy the elements to the appropriate
*  output arrays.
      DO I = 1, EL
         OUT1( I ) = IN( 1, I )
         OUT2( I ) = IN( 2, I )
      END DO
 
      END
