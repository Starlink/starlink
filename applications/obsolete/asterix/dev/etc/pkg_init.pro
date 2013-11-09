      SUBROUTINE {package}0_INIT( STATUS )
*+
*  Name:
*     {package}0_INIT

*  Purpose:
*     Make ADI definitions required for {package} package

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL {package}0_INIT( STATUS )

*  Description:
*     {routine_description}

*  Arguments:
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
*     {package} Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/{package}.html

*  Keywords:
*     package:{package}, usage:private

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
      INCLUDE 'AST_PKG'

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      INTEGER			DID			! Dummy return value
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Not already loaded?
      IF ( .NOT. AST_QPKGI( {package}__PKG ) ) CALL {package}0_INIT( STATUS )
      
*  Mark as initialised
        CALL AST_SPKGI( {package}__PKG )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( '{package}0_INIT', STATUS )

      END
