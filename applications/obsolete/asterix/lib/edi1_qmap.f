      SUBROUTINE EDI1_QMAP( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI1_QMAP

*  Purpose:
*     Service ListMapQuantum requests from the EDI system for HDS files

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_QMAP( NARG, ARGS, OARG, STATUS )

*  Description:
*     Services EDI map quantum requests for HDS files.

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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	QLOC			! List DATA_ARRAY
      CHARACTER*(DAT__SZLOC)	LOC			! HDS filel locator
      CHARACTER*(DAT__SZLOC)	LLOC			! List locator
      CHARACTER*(DAT__SZLOC)	SLOC			! Mapped object

      CHARACTER*20		LIST
      CHARACTER*6		MODE
      CHARACTER*7		TYPE

      INTEGER			LBND,UBND		! Mapping bounds
      INTEGER			LID			! List structure
      INTEGER			NELM			! # mapped items
      INTEGER			PTR			! Mapped data address
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default return value
      OARG = ADI__NULLID

*  Extract file locator
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Extract the arguments
      CALL ADI_GET0C( ARGS(3), LIST, STATUS )
      CALL ADI_GET0C( ARGS(4), TYPE, STATUS )
      CALL ADI_GET0C( ARGS(5), MODE, STATUS )
      CALL ADI_GET0I( ARGS(6), LBND, STATUS )
      CALL ADI_GET0I( ARGS(7), UBND, STATUS )

*  Locate the list structure
      CALL EDI_IDXNAM( ARGS(1), LIST, LID, STATUS )

*  Locate the HDS list component
      CALL DAT_FIND( LOC, LIST, LLOC, STATUS )

*  Locate the data array
      CALL DAT_FIND( LLOC, 'QUANTUM', QLOC, STATUS )

*  First bound of zero means map the whole list
      IF ( LBND .EQ. 0 ) THEN
        CALL DAT_CLONE( QLOC, SLOC, STATUS )
      ELSE
        CALL DAT_SLICE( QLOC, 1, LBND, UBND, SLOC, STATUS )
      END IF

*  Map the slice with the requested type
      CALL DAT_MAPV( SLOC, '_'//TYPE, MODE, PTR, NELM, STATUS )

*  Store pointer in return argument
      CALL ADI_NEWV0I( PTR, OARG, STATUS )

*  Write property containing locator used for mapping. This is need to
*  unmap cleanly
      CALL ADI_CNEWV0C( LID, '.MappedComponentQ', SLOC, STATUS )

*  Release the list data locator, and list container locator
      CALL DAT_ANNUL( QLOC, STATUS )
      CALL DAT_ANNUL( LLOC, STATUS )

*  And the list structure
      CALL ADI_ERASE( LID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_QMAP', STATUS )

      END
