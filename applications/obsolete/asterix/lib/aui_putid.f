      SUBROUTINE AUI_PUTID( ID, NAME, VID, STATUS )
*+
*  Name:
*     AUI_PUTID

*  Purpose:
*     Write ADI value as auxilliary parameter

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AUI_PUTID( ID, NAME, VID, STATUS )

*  Description:
*     Writes auxilliary value store in ADI object to the specified dataset.
*     Auxilliary data is just odd bits of information which don't fit into
*     the dataset model which applications need to put somewhere. Generally
*     knowledge of the format of this information is very restricted, often
*     to the program that writes it.

*  Arguments:
*     ID = INTEGER (given)
*        Dataset to which data is to be written
*     NAME = CHARACTER*(*) (given)
*        The character name of the attribute being written
*     VID = INTEGER (given)
*        The value of the attribute
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
*     AUI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/aui.html

*  Keywords:
*     package:aui, usage:public, auxilliary data, write

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Apr 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Global Variables:
      INCLUDE 'AUI_CMN'                 ! ASTERIX AUI common block
*       AUI_INIT = LOGICAL (given)
*         AUI class definitions loaded?

*  Arguments Given:
      INTEGER			ID			! Dataset identifier
      CHARACTER*(*)		NAME			! Attribute name
      INTEGER			VID			! Attribute value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			ARGS(3)			! Method arguments
      INTEGER			OARG			! Method result
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AUI_INIT ) CALL AUI0_INIT( STATUS )

*  First method argument is the dataset id
      ARGS(1) = ID

*  Create ADI object describing name
      CALL ADI_NEWV0C( NAME, ARGS(2), STATUS )

*  Store value object
      ARGS(3) = VID

*  Invoke method
      CALL ADI_EXEC( 'WriteAux', 3, ARGS, OARG, STATUS )

*  Destroy temporary object
      CALL ADI_ERASE( ARGS(2), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'AUI_PUTID', STATUS )

      END
