      SUBROUTINE ADI2_MRKCHG( HDUID, COBJ, STATUS )
*+
*  Name:
*     ADI2_MRKCHG

*  Purpose:
*     Mark object in HDU as changed

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_MRKCHG( HDUID, COBJ, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     COBJ = INTEGER (given)
*        ADI object in HDU which is changed. This will be object holding
*        keyword, comment or history details
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Sep 1995 (DJA):
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
      INTEGER			HDUID, COBJ

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			MCARD			! First changed card
      INTEGER			NCARD			! HDU card number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Mark keyword and HDU as changed
      CALL ADI_CPUT0L( HDUID, 'Changed', .TRUE., STATUS )
      CALL ADI_CPUT0L( COBJ, '.Changed', .TRUE., STATUS )

*  Get card number
      CALL ADI_CGET0I( COBJ, '.Icard', NCARD, STATUS )

*  Update first changed card number
      CALL ADI_CGET0I( HDUID, 'MinDiffCard', MCARD, STATUS )
      IF ( MCARD .EQ. 0 ) MCARD = NCARD
      CALL ADI_CPUT0I( HDUID, 'MinDiffCard', MIN(MCARD,NCARD), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_MRKCHG', STATUS )

      END
