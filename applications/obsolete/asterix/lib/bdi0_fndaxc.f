      SUBROUTINE BDI0_FNDAXC( ID, QCODE, IAX, STATUS )
*+
*  Name:
*     BDI0_FNDAXC

*  Purpose:
*     Returns the axis number corresponding to the quanity code

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI0_FNDAXC( ID, QCODE, IAX, STATUS )

*  Description:
*     BDI users can refer to axes using a quantity code which has some
*     absolute physical meaning. Supported codes are described in the
*     BDI programmer guide.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     QCODE = CHARACTER*1 (given)
*        The quantity code
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
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     10 Aug 1995 (DJA):
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
      CHARACTER*1		QCODE

*  Arguments Returned:
      INTEGER			IAX

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI0_BLK		! Ensures inclusion

*  Local Variables:
      LOGICAL			FOUND			! Found axis?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      IAX = 0
      FOUND = .FALSE.

*  Check known code
      IF ( INDEX( 'ETPXY', QCODE ) .GT. 0 ) THEN

*    Loop over axes checking labels

*    Not found?
        IF ( .NOT. FOUND ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'C', QCODE )
          CALL ERR_REP( 'BDI0_FNDAXC_1', 'Unable to locate axis '/
     :                /'specified by quantity code ^C', STATUS )
        END IF

*  Report as unknown
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'C', QCODE )
        CALL ERR_REP( 'BDI0_FNDAXC_1', 'Unrecognised axis quantity '/
     :                /'code (^C)', STATUS )

*  End of know code test
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI0_FNDAXC', STATUS )

      END
