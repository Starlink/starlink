      SUBROUTINE EDI_RANGE( FID, LIST, DPTR, MIN, MAX, STATUS )
*+
*  Name:
*     EDI_RANGE

*  Purpose:
*     Get list value ranges

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_RANGE( FID, LIST, DPTR, MIN, MAX, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of EventDS object
*     LIST = CHARACTER*(*) (given)
*        Name of list whose range is be extracted
*     DPTR = INTEGER (given)
*        Pointer to mapped real list data, or 0 otherwise
*     MIN = REAL (returned)
*        Lower extrema
*     MAX = REAL (returned)
*        Upper extrema
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
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     11 Jan 1996 (DJA):
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
      INTEGER			FID, DPTR
      CHARACTER*(*)		LIST

*  Arguments Returned:
      REAL			MIN, MAX

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			LID			! List identifier
      INTEGER			NEVENT			! # events
      INTEGER			NLIST			! # lists

      LOGICAL			MINOK, MAXOK		! Extrema there?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the list
      CALL EDI_IDXNAM( FID, NAME, LID, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Do both Min and Max exist?
        CALL ADI_THERE( LID, 'Min', MINOK, STATUS )
        CALL ADI_THERE( LID, 'Max', MAXOK, STATUS )
        IF ( MINOK .AND. MAXOK ) THEN
          CALL ADI_CGET0R( LID, 'Min', MIN, STATUS )
          CALL ADI_CGET0R( LID, 'Max', MAX, STATUS )

        ELSE IF ( DPTR .NE. 0 ) THEN

*      Get list length
          CALL EDI_GETNS( FID, NEVENT, NLIST, STATUS )
          CALL ARR_RANG1R( NEVENT, %VAL(DPTR), MIN, MAX, STATUS )

        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Cannot get list range from extrema',
     :                  STATUS )

        END IF

*    Release the list
        CALL ADI_ERASE( LID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_RANGE', STATUS )

      END
