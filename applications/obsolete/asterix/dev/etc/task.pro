      SUBROUTINE {ROUTINE_NAME}( STATUS )
*+
*  Name:
*     {ROUTINE_NAME}

*  Purpose:
*     {routine_purpose}

*  Language:
*     Starlink Fortran

*  Type of Module:
*     ASTERIX task

*  Invocation:
*     CALL {ROUTINE_NAME}( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     {routine_description}

*  Usage:
*     {routine_name} {parameter_usage}

*  Environment Parameters:
*     {parameter_name}[pdims] = {parameter_type} ({parameter_access_mode})
*        {parameter_description}

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

*  Implementation Status:
*     {routine_implementation_status}

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
*     {date} {ast_version}-0 ({author_identifier}):
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

*  Global Variables:
      INCLUDE '{global_variables_file}' ! [global_variables_description]
*        {global_name}[dimensions] = {data_type} ({global_access_mode})
*           [global_variable_purpose]

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      [external_declaration]
      {data_type} {external_name} ! [external_description]

*  Local Constants:
      {data_type} {constant_name} ! [constant_description]
      PARAMETER ( {constant_name} = {cons} )

      CHARACTER*30		VERSION
        PARAMETER		( VERSION = '{ROUTINE_NAME} Version {ast_version}-0' )

*  Local Variables:
      {data_type} {name}[dimensions] ! [local_variable_description]

*  Internal References:
      {data_type} {internal_name} ! [internal_description]
      [internal_definition_statement]...

*  Local Data:
      DATA {data_elm} / {data_values}... /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Version id
      CALL MSG_PRNT( VERSION )

*  Initialise ASTERIX
      CALL AST_INIT()

*  Tidy up
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
