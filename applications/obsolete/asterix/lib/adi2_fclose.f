      SUBROUTINE ADI2_FCLOSE( FID, STATUS )
*+
*  Name:
*     ADI2_FCLOSE

*  Purpose:
*     Close a FITSfile object

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI2_FCLOSE( FID, STATUS )

*  Description:
*     Free FITSIO and system resources associated with the FITSfile object
*     supplied. It is assumed that all data has been written to the file
*     that will be required.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
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
      INTEGER			FID			! FITSfile identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			FSTAT			! FITSIO status
      INTEGER			LUN			! Logical unit number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract logical unit
      CALL ADI_CGET0I( FID, '.LUN', LUN, STATUS )

*  Close the file
      FSTAT = 0
      CALL FTCLOS( LUN, FSTAT )

*  Return logical unit to system
      CALL FIO_PUNIT( LUN, STATUS )

*  Trap bad close status
      IF ( FSTAT .NE. 0 ) THEN
        CALL ADI2_FITERP( FSTAT, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCLOSE', STATUS )

      END
