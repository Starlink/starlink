      SUBROUTINE KPG1_RETRUW( EL, INDEX, DATA, VALUE, STATUS )
*+
*  Name:
*     KPG1_RETRx
 
*  Purpose:
*     Retrieves a value from an array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_RETRx( EL, INDEX, DATA, VALUE, STATUS )
 
*  Description:
*     The value stored at a given index within the supplied array is
*     returned.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the array.
*     INDEX = INTEGER (Given)
*        The index within the array of the required value.
*     DATA( EL ) = ? (Given)
*        The input array.
*     VALUE = ? (Returned)
*        The returned value.
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
*        Made generic, and renamed from KPS1_RETR.
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
      INTEGER*2 DATA( EL )
 
*  Arguments Returned:
      INTEGER*2 VALUE
 
*  Status:
      INTEGER STATUS             ! Global status
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Get the required value from the supplied data array at the supplied
*  index.
      VALUE = DATA( INDEX )
 
      END
