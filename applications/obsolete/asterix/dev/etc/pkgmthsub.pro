      SUBROUTINE {ROUTINE_NAME}( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     {ROUTINE_NAME}

*  Purpose:
*     {routine_purpose}

*  Language:
*     {routine_language}

*  Invocation:
*     CALL {ROUTINE_NAME}( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned) 
*        Output data
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

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     Copyright (C) University of Birmingham, {year}

*  Authors:
*     {author_identifier}: {authors_name} ({affiliation})
*     {enter_new_authors_here}

*  History:
*     {date} ({author_identifier}):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE '{global_constants_file}' ! [global_constants_description]

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      [external_declaration]
      {data_type} {external_name} ! [external_description]

*  Local Constants:
      {data_type} {constant_name} ! [constant_description]
      PARAMETER ( {constant_name} = {cons} )

*  Local Variables:
      {data_type} {name}[dimensions] ! [local_variable_description]

*  Internal References:
      {data_type} {internal_name} ! [internal_description]
      [internal_definition_statement]...

*  Local Data:
      DATA {data_elm} / {data_values}... /
      [data_stmt]...

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( '{ROUTINE_NAME}', STATUS )

      END
