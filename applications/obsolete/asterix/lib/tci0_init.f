      SUBROUTINE TCI0_INIT( STATUS )
*+
*  Name:
*     TCI0_INIT

*  Purpose:
*     Load ADI definitions required for TCI operation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI0_INIT( STATUS )

*  Description:
*     Loads those class definitions required by the TCI subroutine group.
*     Results in the following classes being defined,
*
*     Methods are defined to read and write TCI information from HDS and
*     FITS files.

*  Arguments:
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
*     ADI:
*        ADI_REQPKG - Load a package from the load path

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Jan 1995 (DJA):
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
      INCLUDE 'TCI_CMN'					! TCI globals
*        TCI_INIT = LOGICAL (given and returned)
*           TCI definitions load attempted?

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			ADI_REQPKG
      EXTERNAL			TCI1_READ

*  Local Variables:
      INTEGER			DID			! Dummy variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check not already initialised?
      IF ( .NOT. TCI_INIT ) THEN

*    Load the ADI classes
        CALL ADI_REQPKG( 'time', STATUS )

*    Define methods
        CALL ADI_DEFMTH( 'ReadTiming(HDSfile)', TCI1_READ, DID, STATUS )

*    Now initialised
	TCI_INIT = .TRUE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI0_INIT', STATUS )

      END
