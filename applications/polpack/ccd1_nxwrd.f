      SUBROUTINE CCD1_NXWRD( STRING, OFFSET, FIRST, LAST, NOTFND,
     :                       STATUS )
*+
*  Name:
*     CCD1_NXWRD

*  Purpose:
*     To find the next word in a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_NXWRD( STRING, OFFSET, FIRST, LAST, NOTFND, STATUS )

*  Description:
*     The routine looks for the start of the next word, after OFFSET
*     characters, in the given string. Words are assumed to be
*     delimetered by spaces, commas or tabs. The routine is really a
*     wrap round calls to CHR_FIWE and CHR_FIWS trapping any status
*     returns.

*  Arguments:
*     STRING = CHARACTER * ( * ) (Given)
*        The string to be searched for a 'word'. The word is looked for
*        in STRING(OFFSET:).
*     OFFSET = INTEGER (Given)
*        The offset into the given string after which the word is to
*        located.
*     FIRST = INTEGER (Returned)
*        First character of the located word. Offset into STRING.
*     LAST = INTEGER (Returned)
*        Last character of the located word. Offset into STRING.
*     NOTFND = LOGICAL (Returned)
*        If a next word is not located then this is set true, otherwise
*        it is set false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If an error has already occurred, or if an error occurs during this
*     routine, NOTFND is returned .FALSE., and FIRST and LAST are returned
*     equal to one.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1991 (PDRAPER):
*        Original version.
*     5-FEB-1992 (PDRAPER):
*        Changed to CCD1_ routine from ARD original.
*     15-APR-1998 (DSB):
*        Changed so that words which end at the end of the string are
*        found without error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'          ! CHR status values.

*  Arguments Given:
      CHARACTER * ( * ) STRING
      INTEGER OFFSET

*  Arguments Returned:
      INTEGER FIRST
      INTEGER LAST
      LOGICAL NOTFND

*  Status:
      INTEGER STATUS             ! Global status

*  Initialise returned values.
      NOTFND = .TRUE.
      FIRST = 1
      LAST = 1
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for the first character of the word - first non delimiter.
      FIRST = OFFSET 
      CALL CHR_FIWS( STRING, FIRST, STATUS )

*  If no more words remain in the string, annul the error and set the
*  returned flag to indicate this.
      IF ( STATUS .EQ. CHR__WNOTF ) THEN
         CALL ERR_ANNUL( STATUS )
         NOTFND = .TRUE.

*  If a word start was found, indicate a word has been found and find the 
*  word end.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         NOTFND = .FALSE.

         LAST = FIRST 
         CALL CHR_FIWE( STRING, LAST, STATUS )

*  An error will be reported if the word end does not occur before the end
*  of the string. This is still a valid word though, so annul the error.
         IF( STATUS .EQ. CHR__EOSNT ) CALL ERR_ANNUL( STATUS )

      END IF

*  Return null values if an error occurred.
      IF( STATUS .NE. SAI__OK ) THEN
         NOTFND = .TRUE.
         FIRST = 1
         LAST = 1
      END IF

      END 
