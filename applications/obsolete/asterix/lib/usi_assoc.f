      SUBROUTINE USI_ASSOC( PAR, CLASS, ACCESS, ID, STATUS )
*+
*  Name:
*     USI_ASSOC

*  Purpose:
*     Associate an ADI object with an environment parameter

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI_ASSOC( PAR, CLASS, ACCESS, ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        Name of environment parameter to use
*     CLASS = CHARACTER*(*) (given)
*        Class of object to associate
*     ACCESS = CHARACTER*(*) (given)
*        Access mode for association
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
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		PAR			! Parameter name
      CHARACTER*(*)		CLASS			! Data class required
      CHARACTER*(*)		ACCESS			! Access mode

*  Arguments Returned:
      INTEGER			ID			! ADI identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*200		FNAME			! Input object

      INTEGER			EP, PPOS		! Character pointers
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Does parameter name include a representation code?
      PPOS = INDEX( PAR, '%' )
      IF ( PPOS .EQ. 0 ) THEN
        EP = LEN(PAR)
      ELSE
        EP = MAX(1,PPOS - 1)
      END IF

*  Get file name
      CALL USI_GET0C( PAR(:EP), FNAME, STATUS )

*  Open the file
      IF ( STATUS .EQ. SAI__OK ) THEN

*    If caller specified a representation on the parameter, glue it
*    on to the file name
        IF ( PPOS .EQ. 0 ) THEN
          CALL ADI_FOPEN( FNAME, CLASS, ACCESS, ID, STATUS )
        ELSE
          CALL ADI_FOPEN( FNAME(:MAX(1,CHR_LEN(FNAME)))//PAR(PPOS:),
     :                                   CLASS, ACCESS, ID, STATUS )
        END IF
      END IF

*  Store in common
      CALL USI0_STOREI( PAR(:EP), ID, 'I', STATUS )

      END
