      SUBROUTINE ADI2_ADDKEY( HDUID, KEY, VALUE, CMNT, STATUS )
*+
*  Name:
*     ADI2_ADDKEY

*  Purpose:
*     Read keywords from an HDU and store

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ADDKEY( HDUID, KEY, VALUE, CMNT, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     HDUID = INTEGER (given)
*        HDU to add key to
*     KEY = CHARACTER*(*) (given)
*        Name of keyword. Prefix with '@' to inhibit HDU modification flag
*        being set true. Prefix with ',' to only write the comment
*     VALUE = INTEGER (given)
*        ADI identifier of value
*     CMNT = CHARACTER*(*) (given)
*        Comment. Use '~' for standard comment
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
      INTEGER			HDUID, VALUE
      CHARACTER*(*)		KEY, CMNT

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      INTEGER			CID			! Card cache object
      INTEGER			FC			! First key character

      LOGICAL			DIDCRE			! Created keyword?
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First character of key name
      FC = 1
      IF ( KEY(1:1) .EQ. '@' ) FC = 2
      IF ( KEY(1:1) .EQ. ',' ) FC = 2

*  Locate the keyword cache object, creating if required
      CALL ADI2_CFIND_KEY( HDUID, KEY(FC:), .TRUE., CID, DIDCRE,
     :                     STATUS )

*  Write value to cache object
      IF ( KEY(1:1) .NE. ',' ) THEN
        CALL ADI_CPUTID( CID, 'Value', VALUE, STATUS )
      END IF

*  Write comment if non-blank
      IF ( CMNT .GT. ' ' ) THEN
        CALL ADI_THERE( CID, 'Comment', THERE, STATUS )
        IF ( THERE ) THEN
          CALL ADI_CERASE( CID, 'Comment', STATUS )
        END IF
        CALL ADI_CNEWV0C( CID, 'Comment', CMNT(:CHR_LEN(CMNT)), STATUS )
      END IF

*  Release the card
      CALL ADI_ERASE( CID, STATUS )

*  Mark HDU as modified
      IF ( FC .EQ. 1 ) THEN
        CALL ADI_CPUT0L( HDUID, 'Modified', .TRUE., STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_ADDKEY', STATUS )
      END IF

      END
