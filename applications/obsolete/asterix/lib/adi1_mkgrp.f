      SUBROUTINE ADI1_MKGRP( ID, GRPNAM, STATUS )
*+
*  Name:
*     ADI1_MKGRP

*  Purpose:
*     Construct HDS group name given an ADI identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_MKGRP( ID, GRPNAM, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier
*     GRPNAM = CHARACTER*(*) (returned)
*        The HDS group name
*     STATUS = INTEGER (given)
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
*     6 Sep 1995 (DJA):
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
      INTEGER			ID

*  Arguments Returned:
      CHARACTER*(*)		GRPNAM

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*12		LGRP			! Local write buffer

      INTEGER			FSTAT			! Fortran i/o status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Format the ID locally
      WRITE( LGRP, '(A4,Z8.8)', IOSTAT=FSTAT ) 'ADI_', ID

*  Copy to external variable
      GRPNAM = LGRP

      END
