      SUBROUTINE DCI2_READ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     DCI2_READ

*  Purpose:
*     Read detector info from FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI2_READ( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     DCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/dci.html

*  Keywords:
*     package:dci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Oct 1995 (DJA):
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			PHDU			! Main HDU in file

      LOGICAL			OK			! Keyword copied?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create output object
      CALL ADI_NEW0( 'MissionStrings', OARG, STATUS )

*  Locate main HDU
      CALL ADI2_FNDHDU( ARGS(1), ' ', PHDU, STATUS )

*  Copy selected keywords
      CALL ADI2_KCF2A( PHDU, 'INSTRUME', OARG, 'Instrument',
     :                 OK, STATUS )
      CALL ADI2_KCF2A( PHDU, 'DETNAM', OARG, 'Detector',
     :                 OK, STATUS )
      CALL ADI2_KCF2A( PHDU, 'FILTER', OARG, 'Filter',
     :                 OK, STATUS )
      CALL ADI2_KCF2A( PHDU, 'TARGET', OARG, 'Target',
     :                 OK, STATUS )
      IF ( .NOT. OK ) THEN
        CALL ADI2_KCF2A( PHDU, 'OBJECT', OARG, 'Target',
     :                   OK, STATUS )
      END IF
      CALL ADI2_KCF2A( PHDU, 'OBSERVER', OARG, 'Observer',
     :                 OK, STATUS )
      CALL ADI2_KCF2A( PHDU, 'DATAMODE', OARG, 'DataMode',
     :                 OK, STATUS )

*  Release the HDU
      CALL ADI_ERASE( PHDU, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI2_READ', STATUS )

      END
