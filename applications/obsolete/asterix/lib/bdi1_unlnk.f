      SUBROUTINE BDI1_UNLNK( LHS, RHS, STATUS )
*+
*  Name:
*     BDI1_UNLNK

*  Purpose:
*     Service UnLink method for various class to HDSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI1_UNLNK( LHS, RHS, STATUS )

*  Description:
*     Establishes ADI file link between high level objects Scalar, Array
*     and BinDS and the HDSfile.

*  Arguments:
*     LHS = INTEGER (given)
*        ADI identifier of high level object
*     RHS = INTEGER (given)
*        ADI identifier of low level object
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*4               BDICMP
        PARAMETER               ( BDICMP = '.BDI' )

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator
      CHARACTER*6		MODE			! Access mode
      CHARACTER*40		TYPE			! Data type

      INTEGER			BDIID			! Identifier of BDICMP
      INTEGER			ICMP			! Loop over BDICMP comps
      INTEGER			ITID			! Identifier of item
      INTEGER			MCOUNT			! Map count
      INTEGER			NCMP			! # BDICMP components

      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get mode
      CALL ADI_CGET0C( RHS, 'MODE', MODE, STATUS )
      IF ( (STATUS .EQ. SAI__OK) .AND. (MODE(1:1).EQ.'W') ) THEN

*    Get locator and existing type
        CALL ADI1_GETLOC( RHS, LOC, STATUS )
        CALL DAT_TYPE( LOC, TYPE, STATUS )

*    Type is unknown?
        IF ( (STATUS .EQ. SAI__OK) .AND.
     :       (TYPE(1:7) .EQ. 'UNKNOWN') ) THEN
          CALL ADI_THERE( LHS, 'DatasetType', THERE, STATUS )
          IF ( THERE ) THEN
            CALL ADI_CGET0C( LHS, 'DatasetType', TYPE, STATUS )
          ELSE
            CALL ADI_TYPE( LHS, TYPE, STATUS )
          END IF
          IF ( TYPE .LE. ' ' ) TYPE = 'BinDS'

*     Retype the object
          IF ( STATUS .EQ. SAI__OK ) THEN
            CALL DAT_RETYP( LOC, TYPE, STATUS )
          END IF

        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
      END IF

*  Look for BDI container
      CALL ADI_THERE( LHS, BDICMP, THERE, STATUS )
      IF ( THERE ) THEN

*    Locate it
        CALL ADI_FIND( LHS, BDICMP, BDIID, STATUS )

*    For each component
        CALL ADI_NCMP( BDIID, NCMP, STATUS )
        DO ICMP = 1, NCMP

*      Get this item
          CALL ADI_INDCMP( BDIID, ICMP, ITID, STATUS )

*      Get map count
          CALL ADI_CGET0I( ITID, 'MapCount', MCOUNT, STATUS )

*      Unmap if object still mapped
          IF ( MCOUNT .GT. 0 ) THEN
            CALL BDI1_UNMAP_INT( LHS, RHS, ITID, STATUS )
          END IF

*      Release the item
          CALL ADI_ERASE( ITID, STATUS )

        END DO

*    Release container
        CALL ADI_ERASE( BDIID, STATUS )

*    And destroy it
        CALL ADI_CERASE( LHS, BDICMP, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI1_UNLNK', STATUS )

*  Invoke base method to perform unlinkage
      CALL ADI_CALNXT( STATUS )

      END
