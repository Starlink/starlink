      SUBROUTINE ADI2_LOCHDU( FID, HDU, ID, STATUS )
*+
*  Name:
*     ADI2_LOCHDU

*  Purpose:
*     Locate HDU buffer structure in FITSfile object

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI2_LOCHDU( FID, HDU, ID, STATUS )

*  Description:
*     Locates (and creates if necessary) the buffer structures for storing
*     FITS keyword data in FITSfile derived classes

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARSCTER*(*) (given)
*        Logical HDU whose keyword this is. Blank for primary
*     ID = INTEGER (returned)
*        ADI identifier of FITSfile object
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
*     package:adi, usage:private, FITS

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
      INTEGER                   FID                     ! File identifier
      CHARACTER*(*)             HDU                     ! HDU name

*  Arguments Returned:
      INTEGER                   ID                      ! Structure identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			EID			! EXTENSIONS identifier

      LOGICAL			CREATED			! Did we create object?
      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      CREATED = .FALSE.

*  Locate the approriate place depending on the HDU value. Blank means
*  primary HDU
      IF ( HDU(1:1) .EQ. ' ' ) THEN
        CALL ADI_THERE( FID, 'PRIMARY', THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL ADI_CNEW0( FID, 'PRIMARY', 'STRUC', STATUS )
          CREATED = .TRUE.
        END IF
        CALL ADI_FIND( FID, 'PRIMARY', ID, STATUS )

*  Otherwise named HDU in the EXTENSIONS structure
      ELSE
        CALL ADI_THERE( FID, 'EXTENSIONS', THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL ADI_CNEW0( FID, 'EXTENSIONS', 'STRUC', STATUS )
        END IF
        CALL ADI_FIND( FID, 'EXTENSIONS', EID, STATUS )
        CALL ADI_THERE( EID, HDU, THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL ADI_CNEW0( EID, HDU, 'STRUC', STATUS )
          CREATED = .TRUE.
        END IF
        CALL ADI_FIND( EID, HDU, ID, STATUS )

*  Remove temporary
        CALL ADI_ERASE( EID, STATUS )

      END IF

*  Did we create the structure?
      IF ( CREATED ) THEN
        CALL ADI_CPUT0L( ID, '.DEF_START', .FALSE., STATUS )
        CALL ADI_CPUT0L( ID, '.DEF_END', .FALSE., STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_LOCHDU', STATUS )

      END
