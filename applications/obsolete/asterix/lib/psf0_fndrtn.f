      SUBROUTINE PSF0_FNDRTN( PSID, NAME, THERE, ADDR, STATUS )
*+
*  Name:
*     PSF0_FNDRTN

*  Purpose:
*     Locate a named psf routine

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PSF0_FNDRTN( PSID, NAME, THERE, ADDR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        Psf storage identifier
*     NAME = CHARACTER*(*) (given)
*        Name of routine to look for
*     THERE = LOGICAL (returned)
*        Routine exists?
*     ADDR = INTEGER (returned)
*        Routine address
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
*     PSF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/psf.html

*  Keywords:
*     package:psf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      1 May 1996 (DJA):
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
      INTEGER			PSID
      CHARACTER*(*)		NAME

*  Arguments Returned:
      LOGICAL			THERE
      INTEGER			ADDR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		MNAME
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make member name
      MNAME = NAME//'Rtn'

*  Does it exist?
      CALL ADI_THERE( PSID, MNAME, THERE, STATUS )
      IF ( THERE ) THEN

*    Get its value
        CALL ADI_CGET0I( PSID, MNAME, ADDR, STATUS )
        THERE = (ADDR.NE.0)

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'N', NAME )
        CALL ERR_REP( ' ', 'Unknown psf method /^N/', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF0_FNDRTN', STATUS )
      END IF

      END
