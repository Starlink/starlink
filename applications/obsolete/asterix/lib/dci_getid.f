      SUBROUTINE DCI_GETID( ID, DETID, STATUS )
*+
*  Name:
*     DCI_GETID

*  Purpose:
*     Read the detector configuration info from a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI_GETID( ID, DETID, STATUS )

*  Description:
*     Returns an object describing the hardware configuration associated
*     with the specified dataset. This describes the satellite, instrument,
*     detector and filter info.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     DETID = INTEGER (returned)
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

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

*  Arguments Returned:
      INTEGER                   DETID                   ! Detector info

*  Status:
      INTEGER                   STATUS                  ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. DCI_INIT ) CALL DCI1_INIT( STATUS )

*  Initialise return values
      DETID = ADI__NULLID

*  Invoke the read method
      CALL ADI_EXEC( 'ReadDC', 1, ID, DETID, STATUS )

*  Store the returned object on the property list of the object
      CALL ADI_CPUTID( ID, DCI_PROP, DETID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI_GETID', STATUS )

      END
