      SUBROUTINE USI_CREAT( OUT, BASEID, ID, STATUS )
*+
*  Name:
*     USI_CREAT

*  Purpose:
*     Create a new dataset, using the supplied object as the basis

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USI_CREAT( OUT, BASEID, ID, STATUS )

*  Description:
*     Provides user interface wrap up of ADI_FCREAT

*  Arguments:
*     OUT = CHARACTER*(*) (given)
*        Name of environment parameter specifying new file
*     BASEID = INTEGER (given)
*        ADI identifier of object to be at head of chain
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
*     USI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/usi.html

*  Keywords:
*     package:usi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      CHARACTER*(*)		OUT
      INTEGER			BASEID

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FNAME,LFILE		! Input object

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

*  Get output file name
      CALL USI_GET0C( OUT(:EP), FNAME, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    If caller specified a representation on the parameter, glue it
*    on to the file name
        IF ( PPOS .EQ. 0 ) THEN
          CALL ADI_FCREAT( FNAME, BASEID, ID, STATUS )
        ELSE
          LFILE = FNAME(:MAX(1,CHR_LEN(FNAME)))//OUT(PPOS:)
          CALL ADI_FCREAT( LFILE, BASEID, ID, STATUS )
        END IF

*    Store in common
        IF ( BASEID .EQ. ADI__NULLID ) THEN
          CALL USI0_STOREI( OUT(:EP), ID, 'O', .FALSE., STATUS )
        ELSE
          CALL USI0_STOREI( OUT(:EP), BASEID, 'O', .FALSE., STATUS )
        END IF

      END IF

      END
