      SUBROUTINE DCI_PUTID( ID, DETID, STATUS )
*+
*  Name:
*     DCI_PUTID

*  Purpose:
*     Write detector configuration info to a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI_PUTID( ID, DETID, STATUS )

*  Description:
*     Writes data describing the satellite, instrument, detector and filter
*     info to the specified dataset.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     DETID = INTEGER (given)
*        ADI identifier of detector configuration data
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
*     DCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dci.html

*  Keywords:
*     package:dci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Mar 1995 (DJA):
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
      INCLUDE 'DCI_CMN'                 ! ASTERIX DCI common block
*       DCI_INIT = LOGICAL (given)
*         DCI class definitions loaded?

*  Arguments Given:
      INTEGER                   ID                      ! Dataset id
      INTEGER                   DETID                   ! Detector info

*  Status:
      INTEGER                   STATUS                  ! Global status

*  Local Variables:
      INTEGER			ARGS(2)			! Method arguments
      INTEGER			RESID			! Method result
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. DCI_INIT ) CALL DCI0_INIT( STATUS )

*  Construct argument list
      ARGS(1) = ID
      ARGS(2) = DETID

*  Invoke the write method
      CALL ADI_EXEC( 'WriteDC', 2, ARGS, RESID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI_PUTID', STATUS )

      END
