      SUBROUTINE ADI2_HPKY<T>( HDUID, KEY, VALUE, CMNT, STATUS )
*+
*  Name:
*     ADI2_HPKY<T>

*  Purpose:
*     Write value of keyword to specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HPKY<T>( HDUID, KEY, VALUE, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword to be extracted
*     VALUE = <TYPE> (given)
*        The keyword value
*     CMNT = CHARACTER*(*)
*        The keyword comment
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
      INTEGER			HDUID
      <TYPE>			VALUE
      CHARACTER*(*)		KEY, CMNT

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER			FC			! First proper KEY char
      INTEGER			VID			! Keyword value object

      LOGICAL			SCAND			! HDU has been scanned?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If first character is '@' this is an internal write and we won't need to
*  update the keyword in file commit
      FC = 1
      IF ( KEY(1:1) .EQ. '@' ) FC = 2

*  Have keywords been scanned for this HDU
      CALL ADI_CGET0L( HDUID, 'Scanned', SCAND, STATUS )
      IF ( .NOT. SCAND ) THEN
        CALL ADI2_SCAN( HDUID, STATUS )
      END IF

*  Create ADI object to hold value
      CALL ADI_NEWV0<T>( VALUE, VID, STATUS )

*  Write comment if significant
      IF ( CMNT .GT. ' ' ) THEN
        CALL ADI_CNEWV0C( VID, '.Comment', CMNT(:CHR_LEN(CMNT)),
     :                    STATUS )
      END IF

*  Add keyword definition
      CALL ADI2_ADDKEY( HDUID, KEY(FC:), VID, (FC.EQ.1), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HPKY<T>', STATUS )

      END
