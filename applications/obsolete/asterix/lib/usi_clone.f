      SUBROUTINE USI_CLONE( INP, OUT, CLASS, ID, STATUS )
*+
*  Name:
*     USI_CLONE

*  Purpose:
*     Create a cloned copy of input, getting name from environment

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USI_CLONE( INP, OUT, CLASS, ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     INP = CHARACTER*(*) (given)
*        Name of environment parameter specifying clone
*     OUT = CHARACTER*(*) (given)
*        Name of environment parameter specifying new file
*     CLASS = CHARACTER*(*) (given)
*        Class of object to associate
*     ID = INTEGER (returned)
*        ADI identifier of opened object
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
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*
*
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		INP, OUT, CLASS

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTENRAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FNAME			! Input object

      INTEGER			EP, PPOS		! Character pointers
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does output parameter name include a representation code?
      PPOS = INDEX( OUT, '%' )
      IF ( PPOS .EQ. 0 ) THEN
        EP = LEN(OUT)
      ELSE
        EP = MAX(1,PPOS - 1)
      END IF

*  Get ADI identifier of already associated object
      CALL USI0_FNDADI( INP, IFID, STATUS )

*  Get output file name
      CALL USI_GET0C( OUT(:EP), FNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      If caller specified a representation on the parameter, glue it
*      on to the file name
        IF ( PPOS .EQ. 0 ) THEN
          CALL ADI_FCLONE( IFID, FNAME, CLASS, ID, STATUS )
        ELSE
          CALL ADI_FCLONE( IFID, FNAME(:MAX(1,CHR_LEN(FNAME)))/
     :                         /OUT(PPOS:), CLASS, ID, STATUS )
        END IF

*    Store in common
        CALL USI0_STOREI( OUT(:EP), ID, 'O', STATUS )

      END IF

      END
