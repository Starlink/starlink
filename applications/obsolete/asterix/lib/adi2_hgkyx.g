      SUBROUTINE ADI2_HGKY<T>( HDUID, KEY, VALUE, CMNT, STATUS )
*+
*  Name:
*     ADI2_HGKY<T>

*  Purpose:
*     Get value of keyword from specified HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_HGKY<T>( HDUID, KEY, VALUE, CMNT, STATUS )

*  Description:
*     Get value of keyword from specified HDU. It is an error for the
*     keyword not to exist. If the first character of the keyword is
*     '>' then the Written flag is set for the keyword.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     KEY = CHARACTER*(*) (given)
*        The name of the keyword to be extracted
*     VALUE = <TYPE> (returned)
*        The keyword value
*     CMNT = CHARACTER*(*) (returned)
*        Comment string
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			HDUID
      CHARACTER*(*)		KEY

*  Arguments Returned:
      <TYPE>			VALUE
      CHARACTER*(*)		CMNT

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			CID			! Keyword cache obhect
      INTEGER			FC			! First char of
							! keyword name

      LOGICAL			THERE			! Keyword exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for special character
      FC = 1
      IF ( KEY(1:1) .EQ. '>' ) FC = 2

*  Locate keyword
      CALL ADI2_CFIND_KEY( HDUID, KEY(FC:), .FALSE., CID, .FALSE.,
     :                     STATUS )

*  Keyword doesn't exist?
      IF ( CID .EQ. ADI__NULLID ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'K', KEY(FC:) )
        CALL ERR_REP( ' ', 'No such keyword value ^K', STATUS )

*  Keyword exists
      ELSE
        CALL ADI_CGET0<T>( CID, 'Value', VALUE, STATUS )
        CALL ADI_THERE( CID, 'Comment', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CGET0C( CID, 'Comment', CMNT, STATUS )
        ELSE
          CMNT = ' '
        END IF

*    Mark as written?
        IF ( FC .EQ. 2 ) THEN
          CALL ADI_CPUT0L( CID, 'Written', .TRUE., STATUS )
        END IF

*    Release key
        CALL ADI_ERASE( CID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_HGKY<T>', STATUS )

      END
