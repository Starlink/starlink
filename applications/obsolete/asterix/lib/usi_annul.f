      SUBROUTINE USI_ANNUL( PAR, STATUS )
*+
*  Name:
*     USI_ANNUL

*  Purpose:
*     Close the any file associated with the parameter PAR

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI_ANNUL( PAR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        The name of the environment parameter
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
*     20 Sep 1995 (DJA):
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

*  Global Variables:
      INCLUDE 'USI_CMN'                                 ! USI common block
*       USI_INIT = LOGICAL (given)
*         USI class definitions loaded?

*  Arguments Given:
      CHARACTER*(*)		PAR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      CHARACTER*1		BLANK
        PARAMETER		( BLANK = ' ' )

*  Local Variables:
      INTEGER 			N			! USI slot

      LOGICAL 			FOUND			! Found a parameter?
      LOGICAL 			VALID			! Locator valid?
*.

*  New error context
      CALL ERR_BEGIN( STATUS )

*  Look for parameter
      N = 1
      FOUND = .FALSE.
      DO WHILE ( .NOT. FOUND .AND. (N.LE.USI__NMAX) )
        IF ( DS(N).USED ) THEN
          IF (PAR.EQ.(DS(N).PAR)) THEN
            FOUND = .TRUE.
          ELSE
            N = N + 1
          END IF
        ELSE
          N = N + 1
        END IF
      END DO

*  Parameter was found?
      IF ( FOUND ) THEN

*    Opened by ADI compliant application?
        IF ( DS(N).ADIFPN ) THEN
          CALL ADI_FCLOSE( DS(N).ADI_ID, STATUS )

*    Otherwise old style HDS applicatiom
        ELSE IF ( DS(N).LOC .NE. DAT__NOLOC ) THEN
          CALL DAT_VALID( DS(N).LOC, VALID, STATUS )
          IF ( VALID ) THEN
            CALL DAT_ANNUL( DS(N).LOC, STATUS )
          END IF

        END IF

*    Reset slots for re-use
        DS(N).ADI_ID = ADI__NULLID
        DS(N).LOC = DAT__NOLOC
        DS(N).USED = .FALSE.
        DS(N).IO = BLANK

      END IF

*  Restore error context
      CALL ERR_END( STATUS )

      END
