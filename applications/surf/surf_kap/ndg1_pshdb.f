      SUBROUTINE NDG1_PSHDB( STR, DEF, VALUE, STATUS )
*+
*  Name:
*     NDG1_PSHDB

*  Purpose:
*     Parse an HDS object dimension bound.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG1_PSHDB( STR, DEF, VALUE, STATUS )

*  Description:
*     The routine parses a string representing an upper or lower
*     dimension bound of an HDS array object. If the string is blank,
*     then a default value is returned. Leading and trailing spaces are
*     ignored.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        String to be parsed.
*     DEF = INTEGER (Given)
*        Default value to be returned if the string is blank.
*     VALUE = INTEGER (Returned)
*        Dimension bound value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Find the first and last non-blank characters in the string.
*     -  If the input string is blank, then return the default value.
*     -  Otherwise, attempt to convert the string to an integer.
*     -  If the attempt fails, then report an error message.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     29-OCT-1990 (RFWS):
*        Original version.
*     7-DEC-1990 (RFWS):
*        Removed use of '*' to indicate the default bound; only a blank
*        is now used for this purpose.
*     15-FEB-1998 (DSB):
*        Brought into NDG from NDF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER DEF

*  Arguments Returned:
      INTEGER VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER L                  ! Position of last non-blank character

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters in the string.
      CALL CHR_FANDL( STR, F, L )

*  If the input string is blank, then return the default value.
      IF ( F .GT. L ) THEN
         VALUE = DEF

*  Otherwise, attempt to convert the string to an integer.
      ELSE
         CALL CHR_CTOI( STR( F : L ), VALUE, STATUS )

*  If the attempt fails, then report an error message.
         IF ( STATUS .NE. SAI__OK ) THEN
            STATUS = NDF__BNDIN
            CALL MSG_SETC( 'BADBOUND', STR )
            CALL ERR_REP( 'NDG1_PSHDB_SYN',
     :                    'Invalid dimension bound ''^BADBOUND'' ' //
     :                    'specified; bad syntax.', STATUS )
         END IF
      END IF
       
      END
