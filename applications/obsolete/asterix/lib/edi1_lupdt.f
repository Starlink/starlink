      SUBROUTINE EDI1_LUPDT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI1_LUPDT

*  Purpose:
*     Update a list attribute

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_LUPDT( NARG, ARGS, OARG, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     package:edi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     18 Aug 1995 (DJA):
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
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		ATTR			! List attribute name
      CHARACTER*(DAT__SZLOC)	LOC			! Top-level locator
      CHARACTER*(DAT__SZNAM)	LIST			! List name
      CHARACTER*(DAT__SZLOC)	LLOC			! New list structure

      INTEGER			LID			! List object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )
      LID = ARGS(4)

*  Get name of list
      CALL ADI_CGET0C( LID, 'Name', LIST, STATUS )

*  Locate list structure
      CALL DAT_FIND( LOC, LIST, LLOC, STATUS )

*  Get attribute to update
      CALL ADI_GET0C( ARGS(3), ATTR, STATUS )

*  Conditional copy of list structure members to HDS components
      IF ( ATTR .EQ. 'Label' ) THEN
        CALL ADI1_CCA2HC( LID, 'Label', LLOC, 'LABEL', STATUS )
      ELSE IF ( ATTR .EQ. 'Units' ) THEN
        CALL ADI1_CCA2HC( LID, 'Units', LLOC, 'UNITS', STATUS )
      ELSE IF ( ATTR .EQ. 'Decreasing' ) THEN
        CALL ADI1_CCA2HL( LID, 'Decreasing', LLOC, 'DECREASING',
     :                    STATUS )
      ELSE IF ( ATTR .EQ. 'Min' ) THEN
        CALL ADI1_CCA2HT( LID, 'Min', LLOC, 'FIELD_MIN', STATUS )
      ELSE IF ( ATTR .EQ. 'Max' ) THEN
        CALL ADI1_CCA2HT( LID, 'Max', LLOC, 'FIELD_MAX', STATUS )
      ELSE IF ( ATTR .EQ. 'Quantum' ) THEN
        CALL ADI1_CCA2HT( LID, 'Quantum', LLOC, 'QUANTUM', STATUS )
      END IF

*  Release new list
      CALL DAT_ANNUL( LLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_LUPDT', STATUS )

      END
