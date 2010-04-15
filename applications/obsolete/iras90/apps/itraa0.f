      SUBROUTINE ITRAA0( PRIOR, PARAM, STRING, STATUS )
*+
*  Name:
*     ITRAA0

*  Purpose:
*     Write a line of text to the screen and to the log file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ITRAA0( PRIOR, PARAM, STRING, STATUS )

*  Description:
*     Any tokens in the supplied string are expanded by calling
*     MSG_LOAD. The resulting string is then written to the screen so
*     long as the conditional message filter level is greater than or
*     equal to that that specified by argument PRIOR.  The screen
*     output is paged. If LOGPOS is true, then the text is also written
*     to the log file.

*  Arguments:
*     PRIOR = INTEGER (Given)
*        Message output priority. As for MSG_OUTIF (see SUN/104).
*     PARAM = CHARACTER * ( * ) (Given)
*        The message name. As for MSG_LOAD (see SUN/104).
*     STRING = CHARACTER * ( * ) ( Given )
*        The string to write out. May contain MSG tokens.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG constants.

*  Global Variables:
      INCLUDE 'TRA_COM'          ! IRASTRACE common blocks.
*        TRA_FD = INTEGER (Read)
*           The file descriptor for the log file.
*        TRA_FILT = INTEGER (Read)
*           The current conditional message filter level.
*        TRA_LOG = LOGICAL (Read)
*           True if displayed information is to be logged to a text
*           file.
*        TRA_NBL = INTEGER (Read)
*           No. of leading spaces to display in front of the supplied
*           text.

*  Arguments Given:
      INTEGER PRIOR
      CHARACTER PARAM*(*)
      CHARACTER STRING*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER IRM_PTOUT

*  Local Variables:
      INTEGER   BLEN             ! Used length of BUF.
      CHARACTER BUF*255          ! Buffer for a line of text
      INTEGER   ISTAT            ! Local status value.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the routine is to display the string.
      IF( .NOT. ( TRA_FILT .EQ. MSG__QUIET .AND.
     :            PRIOR .NE. MSG__QUIET ) .AND.
     :    .NOT. ( TRA_FILT .EQ. MSG__NORM .AND.
     :            PRIOR .EQ. MSG__VERB ) ) THEN

*  Ensure that BUF is blank.
         BUF = ' '

*  If so, expand any tokens, adding any requested leading blanks.
         IF( TRA_NBL .GT. 0 ) THEN
            CALL MSG_LOAD( PARAM, STRING, BUF( TRA_NBL + 1 : ), BLEN,
     :                     STATUS )
            BLEN = BLEN + TRA_NBL
         ELSE
            CALL MSG_LOAD( PARAM, STRING, BUF, BLEN, STATUS )
         END IF

*  Write the string to the screen.
         ISTAT = IRM_PTOUT( BUF( : BLEN ) )

*  Write the first 80 characters of the string to any log file.
         IF( TRA_LOG ) CALL FIO_WRITE( TRA_FD, BUF( : MIN( 80, BLEN ) ),
     :                                 STATUS )

      END IF

      END
