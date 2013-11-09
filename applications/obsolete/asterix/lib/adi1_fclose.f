      SUBROUTINE ADI1_FCLOSE( FID, STATUS )
*+
*  Name:
*     ADI1_FCLOSE

*  Purpose:
*     Close an HDSfile object

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI1_FCLOSE( FID, STATUS )

*  Description:
*     Free HDS resources associated with the HDSfile object supplied.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the HDSfile object
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      2 Feb 1995 (DJA):
*        Original version.
*     21 Sep 1995 (DJA):
*        Use DAT_ANNUL instead of deprecated HDS_CLOSE
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
      INTEGER			FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZGRP)	GRPNAM			! Group name
      CHARACTER*(DAT__SZLOC)	LOC			! Top level locator
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get group name
      CALL ADI1_MKGRP( FID, GRPNAM, STATUS )

*  Flush associated locators
      CALL HDS_FLUSH( GRPNAM, STATUS )

*  Extract locator
      CALL ADI1_GETLOC( FID, LOC, STATUS )

*  Close the file. LOC should be the only primary locator associated with
*  the file if it has only been opened once, in which case the file is
*  closed. If the file is open more than once this decreases the reference
*  count by one
      CALL DAT_ANNUL( LOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_FCLOSE', STATUS )

      END
