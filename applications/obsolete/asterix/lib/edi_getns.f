      SUBROUTINE EDI_GETNS( ID, NEVENT, NLIST, STATUS )
*+
*  Name:
*     EDI_GETNS

*  Purpose:
*     Get number of events and number of lists from event dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_GETNS( ID, NEVENT, NLIST, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     NEVENT = INTEGER (returned)
*        Number of events or records in event dataset
*     NLIST = INTEGER (returned)
*        Number of columns or lists in event dataset
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'EDI_CMN'                                 ! EDI common block
*       EDI_INIT = LOGICAL (given)
*         EDI class definitions loaded?

*  Arguments Given:
      INTEGER			ID

*  Arguments Returned:
      INTEGER			NEVENT, NLIST

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			EDI0_BLK		! Ensures inclusion
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. EDI_INIT ) CALL EDI0_INIT( STATUS )

*  Check object id
      CALL EDI0_CHKDER( ID, STATUS )

*  Extract values
      CALL ADI_CGET0I( ID, 'NEVENT', NEVENT, STATUS )
      CALL ADI_CGET0I( ID, 'NLIST', NLIST, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_GETNS', STATUS )

      END
