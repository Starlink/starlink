      SUBROUTINE STR_CTOB( ISTRING, NUM, STATUS )
*+
*  Name:
*     STR_CTOB

*  Purpose:
*     Converts 8-bit binary number from CHAR to BYTE

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL STR_CTOB( ISTRING, NUM, STATUS )

*  Description:
*     Converts an 8-bit binary number stored in character form to the
*     equivalent byte quantity. Invalid characters in the input string
*     cause bad status to be returned.  Numbers of less than 8-bits are
*     right adjusted.

*  Arguments:
*     ISTRING = CHARACTER*(*) (given)
*        The string to convert
*     NUM = BYTE (returned)
*        The converted value
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
*     package:str, usage:public, string conversion, bit operations

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RJV: Bob Vallance (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     3 Jul 1995 (RJV):
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
      CHARACTER*(*)		ISTRING

*  Arguments Returned:
      BYTE			NUM

*  Status:
      INTEGER			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
      EXTERNAL			BIT_ORUB
        BYTE			BIT_ORUB

*  Local Variables:
      CHARACTER*8 		STRING

      INTEGER 			L
      INTEGER 			IBIT,JBIT

      BYTE			BIT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NUM = 0
      STRING = '00000000'

*   Right adjust number if necessary
      L = CHR_LEN(ISTRING)
      STRING(9-L:) = ISTRING(:L)

*   Scan string bit by bit
      IBIT=0
      DO WHILE (IBIT.LE.7.AND.STATUS.EQ.SAI__OK)
        IF (IBIT.LT.7) THEN
          BIT = 2**IBIT
        ELSE
          BIT = -128
        ENDIF
        JBIT=8-IBIT
        IF (STRING(JBIT:JBIT).EQ.'1') THEN
          NUM=BIT_ORUB(NUM,BIT)
        ELSE IF (STRING(JBIT:JBIT).NE.'0' ) THEN
          STATUS=SAI__ERROR
          CALL ERR_REP('BADCHAR','Invalid character in binary string',
     :                                                           STATUS)
        END IF
        IBIT=IBIT+1

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'STR_CTOB', STATUS )

      END
