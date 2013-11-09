      SUBROUTINE {routine_name}( ACTION, STATUS )
*+
*  Name:
*     {routine_name}

*  Purpose:
*     Top-level ADAM monolith routine for the {routine_name} package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL {routine_name}( ACTION, STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     ACTION = CHARACTER * ( * ) (Given and Returned)
*        The action name to be interpreted. The value given will be
*        forced to upper case by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

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

*  Arguments Given and Returned:
      CHARACTER * ( * ) ACTION

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the action name to upper case.
      CALL CHR_UCASE( ACTION )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

*  [comment]
      IF ( ACTION .EQ. '{action_name}' ) THEN
         CALL {action_name}( STATUS )

      [ADAM_action]...

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( '{routine_name}_ERR',
     :                 '{routine_name}: The action name ''^ACTION'' is ' //
     :                 'not recognised by the {routine_name} monolith.',
     :                 STATUS )
      END IF

      END
