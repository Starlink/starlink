      SUBROUTINE ADI1_CCH2AA( LOC, CMP, ID, MEMBER, STATUS )
*+
*  Name:
*     ADI1_CCH2AA

*  Purpose:
*     Conditional copy of HDS component of type ARRAY to ADI data member

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_CCH2AA( LOC, CMP, ID, MEMBER, STATUS )

*  Description:
*     Copies an HDS object to an ADI data member if the HDS data is
*     defined.

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (Given)
*        The locator to the HDS structure to contain the component
*     CMP = CHARACTER*(*) (Given)
*        The name of the new HDS component
*     ID = INTEGER (Given)
*        The ADI identifier containing the data member
*     MEMBER = CHARACTER*(*) (Given)
*        The name of the data member to copy
*     STATUS = INTEGER (Given and returned)
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
*      8 Nov 1995 (DJA):
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
      CHARACTER*(DAT__SZLOC)	LOC			! See above
      CHARACTER*(*)		CMP
      INTEGER			ID
      CHARACTER*(*)		MEMBER

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	CLOC			! Component locator
      CHARACTER*(DAT__SZLOC)	MLOC			! ARRAY mapped object

      INTEGER			DIMS(DAT__MXDIM)	! Dimensions
      INTEGER			NDIM			! Dimensionality
      INTEGER			VPTR			! Mapped data

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access component?
      IF ( CMP .GT. ' ' ) THEN

*    Does component exist
        CALL DAT_THERE( LOC, CMP, THERE, STATUS )

*    Locate HDS object
        IF ( THERE ) THEN
          CALL DAT_FIND( LOC, CMP, CLOC, STATUS )
        END IF

*  Otherwise access object supplied
      ELSE
        THERE = .TRUE.
        CALL DAT_CLONE( LOC, CLOC, STATUS )

      END IF

*  Try to copy if it exists
      IF ( THERE ) THEN

*    Get HDS array shape and total number of elements
        CALL ADI1_ARYSHP( CLOC, DAT__MXDIM, DIMS, NDIM, ATYPE, STATUS )

*    Map the HDS data
        CALL ADI1_ARYMAP( CLOC, ATYPE, MLOC, VPTR, STATUS )

*    Write to ADI
        IF ( MEMBER .GT. ' ' ) THEN
          CALL ADI_CNEW( ID, MEMBER, ATYPE, NDIM, DIMS, STATUS )
          CALL ADI_CPUT( ID, MEMBER, ATYPE, NDIM, DIMS, %VAL(VPTR),
     :                   STATUS )

*    Simple object
        ELSE
          CALL ADI_NEW( ATYPE, NDIM, DIMS, ID, STATUS )
          CALL ADI_PUT( ID, ATYPE, NDIM, DIMS, %VAL(VPTR), STATUS )

        END IF

*    Free HDS object
        IF ( MLOC .NE. DAT__SZLOC ) THEN
          CALL DAT_ANNUL( MLOC, STATUS )
        ELSE
          CALL DYN_UNMAP( VPTR, STATUS )
        END IF

        CALL DAT_ANNUL( CLOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI1_CCH2AA', STATUS )
      END IF

      END
