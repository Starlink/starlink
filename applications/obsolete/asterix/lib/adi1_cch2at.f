      SUBROUTINE ADI1_CCH2AT( LOC, CMP, ID, MEMBER, STATUS )
*+
*  Name:
*     ADI1_CCH2AT

*  Purpose:
*     Conditional copy of HDS component to ADI data member

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_CCH2AT( LOC, CMP, ID, MEMBER, STATUS )

*  Description:
*     Copies an HDS object to an ADI data member if the HDS data is
*     defined. Uses the most appropriate ADI type for the HDS data.

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
*     29 Mar 1995 (DJA):
*        Original version.
*      8 Nov 1995 (DJA):
*        Extended to cope with SGP/38 style ARRAY objects
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
      CHARACTER*(DAT__SZLOC)	LOC
      CHARACTER*(*)		CMP,MEMBER
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZTYP)	TYPE			! Type of HDS object

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get HDS type
      IF ( CMP .GT. ' ' ) THEN
        CALL DAT_THERE( LOC, CMP, THERE, STATUS )
        IF ( THERE ) THEN
          CALL CMP_TYPE( LOC, CMP, TYPE, STATUS )
        ELSE
          GOTO 99
        END IF
      ELSE
        CALL DAT_TYPE( LOC, TYPE, STATUS )
      END IF

*  Switch on type
      IF ( TYPE .EQ. '_REAL' ) THEN
        CALL ADI1_CCH2AR( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
        CALL ADI1_CCH2AD( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
        CALL ADI1_CCH2AI( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_LOGICAL' ) THEN
        CALL ADI1_CCH2AL( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
        CALL ADI1_CCH2AB( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
        CALL ADI1_CCH2AW( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_UBYTE' ) THEN
        CALL ADI1_CCH2AUB( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE .EQ. '_UWORD' ) THEN
        CALL ADI1_CCH2AUW( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE(1:5) .EQ. '_CHAR' ) THEN
        CALL ADI1_CCH2AC( LOC, CMP, ID, MEMBER, STATUS )

      ELSE IF ( TYPE(1:5) .EQ. 'ARRAY' ) THEN
        CALL ADI1_CCH2AA( LOC, CMP, ID, MEMBER, STATUS )

      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI1_CCH2AT_1', 'Copy operation not defined '/
     :                /'for HDS type ^T', STATUS )

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_CCH2AT', STATUS )

      END
