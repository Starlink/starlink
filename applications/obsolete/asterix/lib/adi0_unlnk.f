      SUBROUTINE ADI0_UNLNK( LHS, RHS, STATUS )
*+
*  Name:
*     ADI0_UNLNK

*  Purpose:
*     Service UnLink method for various class to FITS/HDS links

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI0_UNLNK( LHS, RHS, STATUS )

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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*      9 Aug 1995 (DJA):
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
      CHARACTER*7               PSTCMP
        PARAMETER               ( PSTCMP = '.Pstore' )

*  Local Variables:
      INTEGER			ICMP			! Loop over PSTCMP comps
      INTEGER			ITID			! Identifier of item
      INTEGER			MCOUNT			! Map count
      INTEGER			NCMP			! # PSTCMP components
      INTEGER			PSTID			! Identifier of PSTCMP

      LOGICAL			THERE			! Object exists
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for private store container
      CALL ADI_THERE( LHS, PSTCMP, THERE, STATUS )
      IF ( THERE ) THEN

*    Locate it
        CALL ADI_FIND( LHS, PSTCMP, PSTID, STATUS )

*    For each component
        CALL ADI_NCMP( PSTID, NCMP, STATUS )
        DO ICMP = 1, NCMP

*      Get this item
          CALL ADI_INDCMP( PSTID, ICMP, ITID, STATUS )

*      Get map count
          CALL ADI_CGET0I( ITID, 'MapCount', MCOUNT, STATUS )

*      Unmap if object still mapped
          IF ( MCOUNT .GT. 0 ) THEN
            CALL ADI0_UNMAP( LHS, RHS, ITID, STATUS )
          END IF

*      Release the item
          CALL ADI_ERASE( ITID, STATUS )

        END DO

*    Release container
        CALL ADI_ERASE( PSTID, STATUS )

*    And destroy it
        CALL ADI_CERASE( LHS, PSTCMP, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI0_UNLNK', STATUS )

      END
