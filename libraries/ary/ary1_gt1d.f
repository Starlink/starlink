      SUBROUTINE ARY1_GT1D( BAD, LOC, TYPE, EL, RESULT, DCE, STATUS )
*+
*  Name:
*     ARY1_GT1D
 
*  Purpose:
*     Read a 1-dimensional array of DOUBLE PRECISION values from an HDS object.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL ARY1_GT1D( BAD, LOC, TYPE, EL, RESULT, DCE, STATUS )
 
*  Description:
*     The routine reads a 1-dimensional array of DOUBLE PRECISION values from an
*     HDS object. The object must also be 1-dimensional and must
*     contain exactly the number of elements to be read. Data type
*     conversion from any primitive numeric HDS data type is performed
*     if necessary, with optional testing for bad pixel values.
 
*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to test for bad pixel values.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to 1-dimensional HDS object to be read.
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS data type of the object to be read. This must be in upper
*        case.
*     EL = INTEGER (Given)
*        Number of array elements to be read.
*     RESULT( N ) = DOUBLE PRECISION (Returned)
*        The array of DOUBLE PRECISION values.
*     DCE = LOGICAL (Returned)
*        Whether any data type conversion errors occurred (the affected
*        elements of the RESULT array are set to bad values if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.
 
*  Algorithm:
*     -  Initialise.
*     -  If no data type conversion is required, then get the data
*     values directly.
*     -  If type conversion is required, then map the data without type
*     conversion, perform the conversion explicitly and unmap the data.
 
*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     13-JUN-1989 (RFWS):
*        Original version.
*     18-SEP-1989 (RFWS):
*        Changed DAT_UNMAP call to ARY1_HUNMP to ensure unmapping under
*        error conditions.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
 
*  Arguments Given:
      LOGICAL BAD
      CHARACTER * ( * ) LOC
      CHARACTER * ( * ) TYPE
      INTEGER EL
 
*  Arguments Returned:
      DOUBLE PRECISION RESULT
      LOGICAL DCE
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local variables:
      INTEGER DIM( 1 )           ! Object dimension array
      INTEGER PNTR               ! Pointer to mapped data
 
*.
 
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise.
      DIM( 1 ) = EL
 
*  If no data type conversion is required, then read the data values
*  directly.
      IF ( TYPE .EQ. '_DOUBLE' ) THEN
         DCE = .FALSE.
         CALL DAT_GET( LOC, '_DOUBLE', 1, DIM, RESULT, STATUS )
 
*  If type conversion is required, then map the data without type
*  conversion, perform the type conversion explicitly and unmap the
*  data.
      ELSE
         CALL DAT_MAP( LOC, TYPE, 'READ', 1, DIM, PNTR, STATUS )
         CALL ARY1_CVTD( BAD, EL, TYPE, PNTR, RESULT, DCE, STATUS )
         CALL ARY1_HUNMP( LOC, STATUS )
      END IF
 
*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_GT1D',
     :STATUS )
 
      END
