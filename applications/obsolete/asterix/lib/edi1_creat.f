      SUBROUTINE EDI1_CREAT( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI1_CREAT

*  Purpose:
*     Create a new event list in an HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_CREAT( NARG, ARGS, OARG, STATUS )

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
      CHARACTER*(DAT__SZLOC)	LOC			! Top-level locator
      CHARACTER*(DAT__SZNAM)	LIST			! List name
      CHARACTER*(DAT__SZLOC)	LLOC			! New list structure
      CHARACTER*(DAT__SZTYP)	TYPE			! List data type

      INTEGER			DIMS(DAT__MXDIM)	! List Dimensions
      INTEGER			EID			! EventDS object
      INTEGER			LID			! List object
      INTEGER			NDIM			! List Dimensionality
      INTEGER			NEVENT			! # events
      INTEGER			NLIST			! # lists

      LOGICAL			THERE			! List already exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract arguments
      EID = ARGS(1)
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )
      LID = ARGS(3)

*  Get number of events and existing lists
      CALL EDI_GETNS( EID, NEVENT, NLIST, STATUS )

*  Get name of list
      CALL ADI_CGET0C( LID, 'Name', LIST, STATUS )

*  Does it already exist? If so, delete existing list
      CALL DAT_THERE( LOC, LIST, THERE, STATUS )
      IF ( THERE ) THEN
        CALL DAT_ERASE( LOC, LIST, STATUS )
      END IF

*  Create new list structure, and locate it
      CALL DAT_NEW( LOC, LIST, 'LIST', 0, 0, STATUS )
      CALL DAT_FIND( LOC, LIST, LLOC, STATUS )

*  Extract type and convert to HDS style
      CALL ADI_CGET0C( LID, 'TYPE', TYPE, STATUS )
      TYPE = '_'//TYPE

*  Get dimensions, and create new last dimension which is number of events
      CALL ADI_CGET1I( LID, 'SHAPE', DAT__MXDIM-1, DIMS, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_ANNUL( STATUS )
        NDIM = 0
      END IF
      NDIM = NDIM + 1
      DIMS(NDIM) = NEVENT

*  Create data array
      CALL DAT_NEW( LLOC, 'DATA_ARRAY', TYPE, NDIM, DIMS, STATUS )

*  Conditional copy of list structure members to HDS components
      CALL ADI1_CCA2HC( LID, 'Label', LLOC, 'LABEL', STATUS )
      CALL ADI1_CCA2HC( LID, 'Units', LLOC, 'UNITS', STATUS )
      CALL ADI1_CCA2HL( LID, 'Decreasing', LLOC, 'DECREASING', STATUS )
      CALL ADI1_CCA2HT( LID, 'Min', LLOC, 'FIELD_MIN', STATUS )
      CALL ADI1_CCA2HT( LID, 'Max', LLOC, 'FIELD_MAX', STATUS )
      CALL ADI1_CCA2HT( LID, 'Quantum', LLOC, 'QUANTUM', STATUS )

*  Release new list
      CALL DAT_ANNUL( LLOC, STATUS )

*  Update list description
      CALL EDI0_UPDLD( EID, LID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_CREAT', STATUS )

      END
