      SUBROUTINE ADI1_CCH2A<T>( LOC, CMP, ID, MEMBER, STATUS )
*+
*  Name:
*     ADI1_CCH2A<T>

*  Purpose:
*     Conditional copy of HDS component to ADI data member

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_CCH2A<T>( LOC, CMP, ID, MEMBER, STATUS )

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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     29 Mar 1995 (DJA):
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

      <TYPE>			VALUE			! Intermediate value

      INTEGER			DIMS(DAT__MXDIM)	! Dimensions
      INTEGER			NDIM			! Dimensionality
      INTEGER			NELM			! Number of elements
      INTEGER			VPTR			! Mapped data

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does component exist
      CALL DAT_THERE( LOC, CMP, THERE, STATUS )

*  Try to copy if it exists
      IF ( THERE ) THEN

*    Locate HDS object
        CALL DAT_FIND( LOC, CMP, CLOC, STATUS )

*    Get shape of DHS data
        CALL DAT_SHAPE( CLOC, DAT__MXDIM, DIMS, NDIM, STATUS )

*    Read the HDS data
        IF ( NDIM .EQ. 0 ) THEN
          CALL DAT_GET0<T>( LOC, VALUE, STATUS )
        ELSE
          CALL DAT_MAPV( CLOC, '<HTYPE>', 'READ', VPTR, NELM, STATUS )
        END IF

*    Write to ADI
        IF ( MEMBER .GT. ' ' ) THEN
          IF ( NDIM .EQ. 0 ) THEN
            CALL ADI_CNEW0<T>( ID, MEMBER, VALUE, STATUS )
          ELSE
            CALL ADI_CNEWV( ID, MEMBER, NDIM, DIMS, %VAL(VPTR), STATUS )
          END IF

*    Simple object
        ELSE
          IF ( NDIM .EQ. 0 ) THEN
            CALL ADI_NEWV0<T>( VALUE, ID, STATUS )
          ELSE
            CALL ADI_NEWV( NDIM, DIMS, %VAL(VPTR), ID, STATUS )
          END IF

        END IF

*    Free HDS object
        IF ( NDIM .GT. 0 ) THEN
          CALL DAT_UNMAP( CLOC, STATUS )
        END IF
        CALL DAT_ANNUL( CLOC, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_CCH2A<T>', STATUS )

      END
