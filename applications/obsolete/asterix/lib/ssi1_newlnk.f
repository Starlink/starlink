      SUBROUTINE SSI1_NEWLNK( LHS, RHS, STATUS )
*+
*  Name:
*     SSI1_NEWLNK

*  Purpose:
*     Make link between SSDS object and new HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SSI1_NEWLNK( LHS, RHS, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LHS = INTEGER (given)
*        The high level object, in this case the SSDS object
*     RHS = INTEGER (given)
*        The low level object, in this case the HDS file
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
*     SSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ssi.html

*  Keywords:
*     package:ssi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Nov 1995 (DJA):
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
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Dataset locator

      LOGICAL			ISSET			! Output is a set
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator
      CALL ADI1_GETLOC( RHS, LOC, STATUS )

*  Is output a set
      CALL ADI_DERVD( LHS, 'SSDSset', ISSET, STATUS )

*  Change the type
      IF ( ISSET ) THEN
        CALL DAT_RETYP( LOC, 'SSDS', STATUS )
      ELSE
        CALL DAT_RETYP( LOC, 'SSDS_SET', STATUS )
      END IF

*  Make the file link
      CALL ADI_SETLNK( LHS, RHS, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'SSI1_NEWLNK', STATUS )

      END
