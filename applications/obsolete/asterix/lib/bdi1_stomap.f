      SUBROUTINE BDI1_STOMAP( PSID, ISINV, LOC, FPTR, PTR, TYPE,
     :                        NELM, MODE, STATUS )
*+
*  Name:
*     BDI1_STOMAP

*  Purpose:
*     Store various things to do with a mapped HDS item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_STOMAP( PSID, ISINV, LOC, FPTR, PTR, TYPE, NELM, MODE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        Item's private storage
*     ISINV = LOGICAL (given)
*        Does mapped memory belong to an invented object
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator to object
*     FPTR = INTEGER (given)
*        Address of mapped file object, or identifier of invented object
*     PTR = INTEGER (given)
*        Address of item mapped memory
*     TYPE = CHARACTER*(*) (given)
*        Data access type
*     NELM = INTEGER (given)
*        Number of elements mapped
*     MODE = CHARACTER*(*) (given)
*        Memory access mode
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
*     package:bdi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER			PSID, FPTR, PTR, NELM
      LOGICAL			ISINV
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		TYPE, MODE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the various items in the storage block
      CALL ADI_CNEWV0C( PSID, 'Locator', LOC, STATUS )
      CALL ADI_CNEWV0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CNEWV0I( PSID, 'Nelm', NELM, STATUS )
      CALL ADI_CNEWV0L( PSID, 'Invented', ISINV, STATUS )
      IF ( ISINV ) THEN
        CALL ADI_CNEWREF( PSID, 'InvObj', FPTR, STATUS )
      ELSE
        CALL ADI_CNEWV0I( PSID, 'FilePtr', FPTR, STATUS )
      END IF
      CALL ADI_CPUT0C( PSID, 'Type', TYPE(:CHR_LEN(TYPE)), STATUS )
      CALL ADI_CPUT0C( PSID, 'Mode', MODE(:CHR_LEN(MODE)), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_STOMAP', STATUS )

      END
