      SUBROUTINE IRG1_VMODE( IN, OUT, STATUS )
*+
*  Name:
*     IRG1_VMODE

*  Purpose:
*     Validate access mode.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRG1_VMODE( IN, OUT, STATUS )

*  Description:
*     The input string is converted to upper case, and a comparison made
*     between the non-blank section and each of the legal values READ,
*     WRITE and UPDATE. If the non-blank section of the string is an
*     abbreviation of one of these legal values, then the full value is
*     returned in the output string. If the input string is not an
*     abbreviation of a legal value, then an error is reported.

*  Arguments:
*     IN = CHARACTER*(*) (Given)
*        The input string, containing an access mode specifier.
*     OUT = CHARACTER*(*) (Given)
*        The output string, containing the full upper-case version of
*        the access mode specifier given in IN. This will be either
*        READ, WRITE or UPDATE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-MAY-1991 (DSB):
*        Original version.
*     27-FEB-1992 (PDRAPER):
*        Removed I90_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IRG_ERR'          ! IRG error values.

*  Arguments Given:
      CHARACTER IN*(*)

*  Arguments Returned:
      CHARACTER OUT*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER  END               ! Last non-blank character in string.
      INTEGER  START             ! First non-blank character in string.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the input to the output and convert to upper case.
      OUT = IN
      CALL CHR_UCASE( OUT )

*  Find the start and end of the non-blank section of the string.
      CALL CHR_FANDL( OUT, START, END )

*  Compare the resulting string with each of the legal values.
*  Abbreviations are acceptable.
      IF( INDEX( 'READ', OUT( START : END ) ) .EQ. 1 ) THEN
         OUT = 'READ'

      ELSE IF( INDEX( 'WRITE', OUT( START : END ) ) .EQ. 1 ) THEN
         OUT = 'WRITE'

      ELSE IF( INDEX( 'UPDATE', OUT( START : END ) ) .EQ. 1 ) THEN
         OUT = 'UPDATE'

*  If an unrecognised value was given, report an error.
      ELSE
         STATUS = IRG__AMODE
         CALL MSG_SETC( 'MODE', IN )
         CALL ERR_REP( 'IRG1_VMODE_ERR1',
     :                 'IRG1_VMODE: Invalid access mode given: ^MODE',
     :                 STATUS )
      END IF

      END
* $Id$
