      SUBROUTINE SORT_MVIDXT( N, TYPE, IN, INDEX, OUT, STATUS )
*+
*  Name:
*     SORT_MVIDXT

*  Purpose:
*     Moves data of specified type using a supplied index

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL SORT_MVIDXT( N, TYPE, IN, INDEX, OUT, STATUS )

*  Description:
*     Moves the values in the array IN to the slots in the array OUT
*     given by the ordering array INDEX.

*  Arguments:
*     N = INTEGER (given)
*        Number of values in arrays
*     TYPE = CHARACTER*(*) (given)
*        The ADI type of the data
*     IN = <TYPE>[] (given)
*        Input values to be moved
*     INDEX = INTEGER[] (given)
*        The ordering list
*     OUT = <TYPE>[] (returned)
*        The output ordered array
*     STATUS = INTEGER (given)
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
*     SORT Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/sort.html

*  Keywords:
*     package:sort, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Jun 1995 (DJA):
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
      INTEGER                	N, INDEX(*)
      CHARACTER*(*)		TYPE
      BYTE       		IN(*)

*  Arguments Returned:
      BYTE       		OUT(*)

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on type
      IF ( TYPE .EQ. 'DOUBLE' ) THEN
        CALL SORT_MVIDXD( N, IN, INDEX, OUT, STATUS )
      ELSE IF ( TYPE .EQ. 'INTEGER' ) THEN
        CALL SORT_MVIDXI( N, IN, INDEX, OUT, STATUS )
      ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
        CALL SORT_MVIDXL( N, IN, INDEX, OUT, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( 'SORT_MVIDXT_1', 'Don''t know how to sort '/
     :                /'data of type ^T', STATUS )

      END IF

*  Report status
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'SORT_MVIDXT', STATUS )
      END IF

      END
