      SUBROUTINE ADI2_FCREAT( FILE, ID, FID, STATUS )
*+
*  Name:
*     ADI2_FCREAT

*  Purpose:
*     {routine_purpose}

*  Language:
*     {routine_language}

*  Invocation:
*     CALL ADI2_FCREAT( [p]... )

*  Description:
*     {routine_description}

*  Arguments:
*     {argument_name}[dimensions] = {data_type} ({argument_access_mode})
*        {argument_description}
*     STATUS = INTEGER ({status_access_mode})
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
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     1 Feb 1995 (DJA):
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
      INTEGER			FILE			! File name to open
      INTEGER			ID			! Template object

*  Arguments Returned:
      INTEGER			FID			! New file object

*  Status:
      INTEGER 			STATUS                  ! Global status

*  External References:
C      [external_declaration]
C      {data_type} {external_name} ! [external_description]

*  Local Constants:
c      {data_type} {constant_name} ! [constant_description]
C      PARAMETER ( {constant_name} = {cons} )

*  Local Variables:
c      {data_type} {name}[dimensions] ! [local_variable_description]

*  Internal References:
c      {data_type} {internal_name} ! [internal_description]
c      [internal_definition_statement]...

*  Local Data:
c      DATA {data_elm} / {data_values}... /
c      [data_stmt]...

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

	CALL ADI_PRINT( ID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCREAT', STATUS )

      END
