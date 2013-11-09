      SUBROUTINE ADI2_ADDCMT( HDUID, CMNT, UPDATE, STATUS )
*+
*  Name:
*     ADI2_ADDCMT

*  Purpose:
*     Add a comment description to an HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ADDCMT( HDUID, CMNT, UPDATE, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     CMNT = CHARACTER*(*) (given)
*        The comment value
*     UPDATE = LOGICAL (given)
*        The keyword is being added with the intention of updating the file?
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
      CHARACTER*(*)		CMNT
      LOGICAL			UPDATE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*4		CNAME			! Comment struc name

      INTEGER			CCID			! Comments list
      INTEGER			CID			! Comment object
      INTEGER			NCARD			! HDU card number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate comments container
      CALL ADI_FIND( HDUID, 'Comments', CCID, STATUS )

*  Create comment object
      CALL ADI_NEWV0C( CMNT, CID, STATUS )

*  Allocate a number for the card
      CALL ADI2_ADDCRC( HDUID, 'C', CID, NCARD, STATUS )

*  Mark as updated?
      IF ( UPDATE ) THEN
        CALL ADI2_MRKCHG( HDUID, CID, STATUS )
        CALL ADI_CPUT0L( CID, '.New', .TRUE., STATUS )
      END IF

*  Create structure name from card number
      WRITE( CNAME, '(A1,I3.3)' ) 'C', NCARD

*  Write component to container
      CALL ADI_CPUTID( CCID, CNAME, CID, STATUS )

*  Release comment container
      CALL ADI_ERASE( CCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ADDCMT', STATUS )

      END
