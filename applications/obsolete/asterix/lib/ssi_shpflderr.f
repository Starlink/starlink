      SUBROUTINE SSI_SHPFLDERR( ID, FLD, MXDIM, DIMS, NDIM, STATUS )
*+
*  Name:
*     SSI_SHPFLDERR

*  Purpose:
*     Get dimensions of a field's error component

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SSI_SHPFLDERR( ID, FLD, MXDIM, DIMS, NDIM, STATUS )

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

*  References:
*     SSI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/ssi.html

*  Keywords:
*     package:ssi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 May 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER			ID			! Dataset id
      CHARACTER*(*)		FLD			! Field name
      INTEGER			MXDIM			! Max # dimensions

*  Arguments Returned:
      INTEGER			DIMS(*)			! Error dimensions
      INTEGER			NDIM			! Error rank

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)		FLOC		! Field locator
      CHARACTER*(DAT__SZLOC)		LOC		! Dataset locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI1_GETLOC( ID, LOC, STATUS )

*  Locate field
      CALL SSO_LOCFLD( LOC, FLD, FLOC, STATUS )

*  Get dimensions of error
      CALL CMP_SHAPE( FLOC, 'ERROR', MXDIM, DIMS, NDIM, STATUS )

*  Free field locator
      CALL DAT_ANNUL( FLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SSI_SHPFLDERR', STATUS )
      END IF

      END
