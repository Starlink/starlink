      SUBROUTINE ADI2_ADDHIS( HDUID, HIST, UPDATE, STATUS )
*+
*  Name:
*     ADI2_ADDHIS

*  Purpose:
*     Add a history description to an HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_ADDHIS( HDUID, HIST, UPDATE, STATUS )

*  Description:
*     Write value of keyword to specified HDU. Any existing keyword value
*     is overwritten.

*  Arguments:
*     HDUID = INTEGER (given)
*        ADI identifier of HDU object
*     HIST = CHARACTER*(*) (given)
*        The history value
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
      CHARACTER*(*)		HIST
      LOGICAL			UPDATE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*4		HNAME			! History struc name

      INTEGER			HCID			! Historys list
      INTEGER			CID			! History object
      INTEGER			NCARD			! HDU card number
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate historys container
      CALL ADI_FIND( HDUID, 'History', HCID, STATUS )

*  Create history object
      CALL ADI_NEWV0C( HIST, CID, STATUS )

*  Get keyword number and update
      IF ( UPDATE ) THEN

*    Mark keyword as changed
        CALL ADI2_MRKCHG( HDUID, CID, STATUS )
        CALL ADI_CPUT0L( KID, '.New', .TRUE., STATUS )

      ELSE

*    Allocate a number for the card
        CALL ADI2_ADDCRC( HDUID, 'H', CID, NCARD, STATUS )

      END IF

*  Create structure name from card number
      WRITE( HNAME, '(A1,I3.3)' ) 'H', NCARD

*  Write component to container
      CALL ADI_CPUTID( HCID, HNAME, CID, STATUS )

*  Release history container
      CALL ADI_ERASE( HCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_ADDHIS', STATUS )

      END
