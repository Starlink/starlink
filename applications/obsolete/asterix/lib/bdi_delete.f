      SUBROUTINE BDI_DELETE( ID, ITEM, STATUS )
*+
*  Name:
*     BDI_DELETE

*  Purpose:
*     Remove the specified item from a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_DELETE( ID, ITEM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        Dataset id
*     ITEM = CHARACTER*(*) (given)
*        Name of item to delete
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     5 Apr 1995 (DJA):
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
      CHARACTER*(*)		ITEM			! Item name

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_SIMLR
        LOGICAL			CHR_SIMLR

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Locator to object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI1_GETLOC( ID, LOC, STATUS )

      IF ( CHR_SIMLR(ITEM,'VARIANCE') ) THEN
        CALL DAT_ERASE( LOC, 'VARIANCE', STATUS )

      ELSEIF ( CHR_SIMLR(ITEM,'DATA') ) THEN
        CALL DAT_ERASE( LOC, 'DATA_ARRAY', STATUS )

      ELSE IF ( CHR_SIMLR(ITEM,'QUALITY') ) THEN
        CALL DAT_ERASE( LOC, 'QUALITY', STATUS )

      ELSE
        CALL MSG_SETC( 'ITEM', ITEM )
        CALL ERR_REP( ' ', 'Don''t know how to delete ^ITEM', STATUS )
      END IF

      END

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_DELETE', STATUS )

      END
