      SUBROUTINE BDI_COPY( ID, ITEMS, OID, OITEMS, STATUS )
*+
*  Name:
*     BDI_COPY

*  Purpose:
*     Copy the named items from the input to the output file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_COPY( ID, ITEMS, OID, OITEMS, STATUS )

*  Description:
*     Retrieves the items specified by the ITEMS string with the specified
*     TYPE. Should only be used for numeric items.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof
*     ITEMS = CHARACTER*(*) (given)
*        List of items to be copied
*     OID = INTEGER (given)
*        ADI identifier of BinDS, Array or Scalar object, or derivatives
*        thereof. Object to which items are copied
*     OITEMS = CHARACTER*(*) (given)
*        Name of corresponding item in output. If blank, objects are
*        copied to the same name
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
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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

*  Global Variables:
      INCLUDE 'BDI_CMN'                                 ! BDI common block
*       BDI_INIT = LOGICAL (given)
*         BDI class definitions loaded?

*  Arguments Given:
      INTEGER			ID, OID
      CHARACTER*(*)		ITEMS, OITEMS

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			BDI0_BLK		! Ensures inclusion

*  Local Variables:
      INTEGER			C1, C2			! Character pointers
      INTEGER			DOBJ			! Object to copy
      INTEGER			IITEM			! Item counter
      INTEGER			ILID			! I/p linked object
      INTEGER			OC1, OC2		! O/p character pointers
      INTEGER			OITEM			! O/p item counter
      INTEGER			OLID			! O/p linked object

      LOGICAL			SAME			! O/p item list blank?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. BDI_INIT ) CALL BDI0_INIT( STATUS )

*  Get object links
      CALL ADI_GETLINK( ID, ILID, STATUS )
      CALL ADI_GETLINK( OID, OLID, STATUS )

*  If OITEMS is blank the output item name is the same as the input
      SAME = (OITEMS .LE. ' ')

*  Loop over items while more of them and status is ok
      CALL UDI0_CREITI( ITEMS, C1, C2, IITEM, STATUS )
      IF ( .NOT. SAME ) THEN
        CALL UDI0_CREITI( OITEMS, OC1, OC2, OITEM, STATUS )
      END IF
      DO WHILE ( (C1.NE.0) .AND. (STATUS.EQ.SAI__OK) )

*    Get an ADI object holding the object data using mode 2 (ignore errors
*    getting objects)
        CALL BDI_GET1( ID, ILID, ITEMS(C1:C2), 2, DOBJ, STATUS )

*    Object exists in input?
        IF ( DOBJ .NE. ADI__NULLID ) THEN

*      Write object to output file
          IF ( SAME ) THEN
            CALL BDI_PUT1( OID, OLID, ITEMS(C1:C2), DOBJ, STATUS )
          ELSE
            CALL BDI_PUT1( OID, OLID, OITEMS(OC1:OC2), DOBJ, STATUS )
          END IF

*      Release temporary object
          CALL ADI_ERASE( DOBJ, STATUS )

        END IF

*    Advance iterator to next item
        CALL UDI0_ADVITI( ITEMS, C1, C2, IITEM, STATUS )
        IF ( .NOT. SAME ) THEN
          CALL UDI0_ADVITI( OITEMS, OC1, OC2, OITEM, STATUS )
        END IF

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_COPY', STATUS )

      END
