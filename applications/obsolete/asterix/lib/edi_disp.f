      SUBROUTINE EDI_DISP( ID, STATUS )
*+
*  Name:
*     EDI_DISP

*  Purpose:
*     Display list of lists present in a dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_DISP( ID, STATUS )

*  Description:
*     Display formatted list of lists available in an event dataset

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
      EXTERNAL			EDI0_PRPIL
        CHARACTER*8		EDI0_PRPIL

*  Local Variables:
      CHARACTER*60		LABEL			! List label
      CHARACTER*20		LNAME			! List name
      CHARACTER*8		PNAME			! Property name
      CHARACTER*79		TEXT			! O/p text

      INTEGER			I			! Loop over lists
      INTEGER			LCID			! List container
      INTEGER			LID			! List object
      INTEGER			NEV			! # events
      INTEGER			NL			! # lists

      LOGICAL			LTHERE			! Label exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( EDI__PKG ) ) CALL EDI0_INIT( STATUS )

*  Check correct object
      CALL EDI0_CHKDER( ID, STATUS )

*  Get number of lists
      CALL EDI_GETNS( ID, NEV, NL, STATUS )

*  Ok?
      IF ( STATUS .EQ. SAI__OK  )THEN

*    Locate list container
        CALL ADI_FIND( ID, 'Lists', LCID, STATUS )

*    Loop over lists
        DO I = 1, NL

*      Construct property name
          PNAME = EDI0_PRPIL( I )

*      Get name of this list
          CALL ADI_CGET0C( LCID, PNAME, LNAME, STATUS )

*      Locate list and extract label if present
          CALL ADI_FIND( LCID, LNAME, LID, STATUS )

*      Is label present
          CALL ADI_THERE( LID, 'Label', LTHERE, STATUS )
          IF ( LTHERE ) THEN
            CALL ADI_CGET0C( LID, 'Label', LABEL, STATUS )
          ELSE
            LABEL = ' '
          END IF

*      Write text line
          WRITE( TEXT, '(1X,I2,2X,2A)' ) I, LNAME, LABEL(:44)
          CALL MSG_PRNT( TEXT )

*      Free list
          CALL ADI_ERASE( LID, STATUS )

        END DO

*    Free list container
        CALL ADI_ERASE( LCID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_DISP', STATUS )

      END
