      SUBROUTINE TCI_NEW( SYSTEM, TIMID, STATUS )
*+
*  Name:
*     TCI_NEW

*  Purpose:
*     Read the timing info from a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI_NEW( SYSTEM, TIMID, STATUS )

*  Description:
*     Creates a new timing information object

*  Arguments:
*     SYSTEM = CHARACTER*(*) (given)
*        Time frame, eg. LOCAL, BARYCENTRIC etc
*     TIMID = INTEGER (returned)
*        ADI identifier of timing info
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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'ADI_PAR'

*  Global Variables:
      INCLUDE 'TCI_CMN'                 ! ASTERIX TCI common block
*       TCI_INIT = LOGICAL (given)
*         TCI class definitions loaded?

*  Arguments Given:
      CHARACTER*(*)		SYSTEM

*  Arguments Returned:
      INTEGER                   TIMID

*  Status:
      INTEGER                   STATUS                  ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. TCI_INIT ) CALL TCI0_INIT( STATUS )

*  Create new object
      CALL ADI_NEW0( 'TimingInfo', TIMID, STATUS )

*  Define system if supplied
      IF ( SYSY .GT. ' ' ) THEN
        CALL ADI_CPUT0C( TIMID, 'System', SYSTEM, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI_NEW', STATUS )

      END
