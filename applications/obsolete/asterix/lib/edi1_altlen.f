      SUBROUTINE EDI1_ALTLEN( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     EDI1_ALTLEN

*  Purpose:
*     Adjust length of lists in HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI1_ALTLEN( NARG, ARGS, OARG, STATUS )

*  Description:
*     Adjusts lengths of all the lists in an HDS event dataset

*  Arguments:
*     NARG = INTEGER (given)
*        Number of input arguments
*     ARGS[NARG] = INTEGER (given)
*        Input argument identifiers
*     OARG = INTEGER (returned)
*        Output argument
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER			OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	DLOC			! List data array
      CHARACTER*(DAT__SZLOC)	LLOC			! List component
      CHARACTER*(DAT__SZLOC)	LOC			! Top level object
      CHARACTER*20		NAME			! List name

      INTEGER			I			! Loop over lists
      INTEGER			LID			! Lists object id
      INTEGER			NEVENT			! Number of records
      INTEGER			NLIST			! Number of lists

      LOGICAL			THERE			! Object is mapped?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  No return value
      OARG = ADI__NULLID

*  Get locator for top level object
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Get existing list length, and number of lists
      CALL EDI_GETNS( ARGS(1), NEVENT, NLIST, STATUS )

*  Get new list length
      CALL ADI_CGET0I( ARGS(3), NEVENT, STATUS )

*  Loop over lists, unmapping those which are still mapped
      DO I = 1, NLIST

*    Locate the list
        CALL EDI_IDX( ARGS(1), I, LID, STATUS )

*    Get its name
        CALL ADI_NAME( LID, NAME, STATUS )

*    Is it mapped?
        CALL ADI_THERE( LID, '.MappedComponent', THERE, STATUS )
        IF ( THERE ) THEN
          STATUS = SAI__ERROR
          CALL MSG_SETC( 'L', NAME )
          CALL ERR_REP( 'EDI1_ALTLEN_1', 'Unable to alter length of '/
     :                       /'list ^L because it is mapped', STATUS )
        END IF

*    Locate the list data component
        CALL DAT_FIND( LOC, NAME, LLOC, STATUS )
        CALL DAT_FIND( LLOC, 'DATA_ARRAY', DLOC, STATUS )
        CALL DAT_ALTER( DLOC, 1, NEVENT, STATUS )
        CALL DAT_ANNUL( DLOC, STATUS )
        CALL DAT_ANNUL( LLOC, STATUS )

*    Free the list identifier
        CALL ADI_ERASE( LID, STATUS )

      END DO

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI1_ALTLEN', STATUS )

      END
