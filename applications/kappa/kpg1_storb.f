      SUBROUTINE KPG1_STORB( EL, INDEX, VALUE, DATA, STATUS )
*+
*  Name:
*     KPG1_STORx
 
*  Purpose:
*     Stores a value in an array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_STORx( EL, INDEX, VALUE, DATA, STATUS )
 
*  Description:
*     The supplied value is stored in the array at the given index.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the array.
*     INDEX = INTEGER (Given)
*        The index at which to store the supplied value.
*     VALUE = ? (Given)
*        The value to be stored in the array.
*     DATA( EL ) = ? (Given and Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Notes:
*     -  There is a routine for all numeric data types: replace "x" in
*     the routine name by B, D, I, R, UB, UW, or W as appropriate.  The
*     VALUE and DATA arguments must have the data type specified.
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     12-NOV-1993 (DSB):
*        Original version.
*     1995 April 12 (MJC):
*        Made generic, and renamed from KPS1_STOR.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
 
*  Arguments Given:
      INTEGER EL
      INTEGER INDEX
      BYTE VALUE
 
*  Arguments Given and Returned:
      BYTE DATA( EL )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Put the supplied value in the data array at the supplied index.
      DATA( INDEX ) = VALUE
 
      END
