      SUBROUTINE BDI2_UNLNK( LHS, RHS, STATUS )
*+
*  Name:
*     BDI2_UNLNK

*  Purpose:
*     Service UnLink method for various class to FITSfile links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI2_UNLNK( LHS, RHS, STATUS )

*  Description:
*     Breaks ADI file link between high level objects Scalar, Array
*     and BinDS and the FITSfile.

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

*  Arguments Given:
      INTEGER                   LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*4               BDICMP
        PARAMETER               ( BDICMP = '.BDI' )

*  Local Variables:
      INTEGER			BDIID			! Identifier of BDICMP
      INTEGER			ICMP			! Loop over BDICMP comps
      INTEGER			ITID			! Identifier of item
      INTEGER			MCOUNT			! Map count
      INTEGER			NCMP			! # BDICMP components

      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

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
            CALL ADI2_UNMAP( RHS, ITID, 0, STATUS )
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
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI2_UNLNK', STATUS )

*  Invoke base method to perform unlinkage
      CALL ADI_CALNXT( STATUS )

      END
