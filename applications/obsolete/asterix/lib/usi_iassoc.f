      SUBROUTINE USI_IASSOC( PAR, IDX, CLASS, ACCESS, ID, STATUS )
*+
*  Name:
*     USI_IASSOC

*  Purpose:
*     Associate an ADI object with an indexed environment parameter

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI_IASSOC( PAR, IDX, CLASS, ACCESS, ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        Root name of environment parameter to use
*     IDX = INTEGER (given)
*        Index to be appended to parameter root
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
*     21 Nov 1995 (DJA):
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
      CHARACTER*(*)		PAR, CLASS, ACCESS
      INTEGER			IDX

*  Arguments Returned:
      INTEGER			ID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*12		STR			! IDX in string

      INTEGER			NDIG 			! Digits in STR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct parameter index string
      CALL CHR_ITOC( IDX, STR, NDIG )

*  Invoke lower level routine
      CALL USI_ASSOC( PAR(:CHR_LEN(PAR))//STR(:NDIG), CLASS, ACCESS,
     :                ID, STATUS )

      END
