      SUBROUTINE BDI_GETTYP( ID, TYPE, STATUS )
*+
*  Name:
*     BDI_GETTYP

*  Purpose:
*     Retrieve the basic data type of data model object supported by BDI

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GETTYP( ID, TYPE, STATUS )

*  Description:
*     Extracts the type of a data model object for subsequent
*     manipulation by BDI. Accepts ADI type specifications or
*     HDS style ones (as ADI but with a leading underscore).

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     TYPE = CHARACTER*(*) (returned)
*        The type of the data
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      INCLUDE 'BDI_CMN'                                 ! BDI common block
*       BDI_INIT = LOGICAL (given)
*         BDI class definitions loaded?

*  Arguments Given:
      INTEGER			ID

*  Arguments Returned:
      CHARACTER*(*)		TYPE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI0_BLK		! Ensures inclusion
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. BDI_INIT ) CALL BDI0_INIT( STATUS )

*  Read the type string
      CALL ADI_CGET0C( ID, 'TYPE', TYPE, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_GETTYP', STATUS )

      END
