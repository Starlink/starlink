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

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1991 (PDRAPER):
*        Original version.
*     5-FEB-1992 (PDRAPER):
*        Changed to CCD1_ routine from ARD original.
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

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for the first character of the word - first non delimeter.
      FIRST = OFFSET 
      CALL CHR_FIWS( STRING, FIRST, STATUS )
      LAST = FIRST 
      CALL CHR_FIWE( STRING, LAST, STATUS )

*  Check for error.
      IF ( STATUS .EQ. CHR__ENDOFSENT .OR. STATUS .EQ. CHR__WRDNOTFND )
     :   THEN
            CALL ERR_ANNUL( STATUS )
            NOTFND = .TRUE.
      ELSE
            NOTFND = .FALSE.
      END IF

      END 
* @(#)ccd1_nxwrd.f	2.1     11/30/93     2
