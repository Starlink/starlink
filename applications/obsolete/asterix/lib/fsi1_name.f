      SUBROUTINE FSI1_NAME( REFID, TAG, NAME, STATUS )
*+
*  Name:
*     FSI1_NAME

*  Purpose:
*     Generate a reference or selection name given an ADI reference identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FSI1_NAME( REFID, TAG, NAME, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     REFID = INTEGER (given)
*        ADI object holding reference number
*     TAG = CHARACTER*(*) (given)
*        The text string to prefix NAME, usually REF or SEL
*     NAME = CHARACTER*(*) (returned)
*        Number of method arguments
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
*     FSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fsi.html

*  Keywords:
*     package:fsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1995 (DJA):
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
      INTEGER                   REFID
      CHARACTER*(*)		TAG

*  Arguments Returned:
      CHARACTER*(*)		NAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*6		RSTR			! IREF in string form

      INTEGER			IREF			! Reference number
      INTEGER			RLEN			! Length of RSTR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract reference number
      CALL ADI_GET0I( REFID, IREF, STATUS )

*  Encode the integer
      CALL CHR_ITOC( IREF, RSTR, RLEN )

*  Create output
      NAME = TAG//RSTR(1:RLEN)

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FSI1_NAME', STATUS )

      END
