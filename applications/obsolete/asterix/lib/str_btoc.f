      SUBROUTINE STR_BTOC( NUM, STRING, STATUS )
*+
*  Name:
*     STR_BTOC

*  Purpose:
*     Converts 8-bit binary number from BYTE to CHAR

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL STR_BTOC( NUM, STRING, STATUS )

*  Description:
*     Returns the binary representation of an 8-bit number in character form

*  Arguments:
*     NUM = BYTE (given)
*        The byte value to convert
*     STRING = CHARACTER*(*) (returned)
*        The string representation of NUM
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
*     STR Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/str.html

*  Keywords:
*     package:str, usage:public, bit operations

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     3 Jul 1995 (DJA):
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
      BYTE			NUM

*  Arguments Returned:
      CHARACTER*(*)		STRING

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			BIT_ANDUB
        BYTE			BIT_ANDUB

*  Local Variables:
      INTEGER 			IBIT,JBIT

      BYTE 			BIT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (LEN(STRING).LT.8) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( 'TOOSHORT', 'Character string too short', STATUS )

      ELSE
        STRING = '00000000'

*    Scan number bit by bit
        IBIT=0
        DO WHILE (IBIT.LE.7)
          IF (IBIT.LT.7) THEN
            BIT=2**IBIT
          ELSE
            BIT=-128
          END IF
          JBIT=8-IBIT
          IF (BIT_ANDUB(NUM,BIT)/BIT) THEN
            STRING(JBIT:JBIT)='1'
          ENDIF
          IBIT = IBIT + 1
        END DO

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'STR_BTOC', STATUS )

      END
