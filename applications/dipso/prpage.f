      SUBROUTINE PRPAGE( COMM, TEXT, INIT, STATUS )
*+
*  Name:
*     PRPAGE

*  Purpose:
*     Display screen output in pages

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PRPAGE( COMM, TEXT, INIT, STATUS )

*  Description:
*     PRPAGE should first be called with INIT set .TRUE. to start a new
*     page. Succesive calls display the supplied text until 20 lines
*     have been displayed. The next text string is then not displayed
*     until the user has been prompted for a logical value specifying
*     whether or not to continue. An error is reported if a FALSE
*     value is supplied by the user (this allows the user to abort the
*     paged output).

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command being obeyed.
*     TEXT = CHARACTER * ( * ) (Given)
*        The text to display. Thi may contain MSG tokens which will be
*        expanded.
*     INIT = INTEGER (Given)
*        If .TRUE. the current line count is reset to zero. In this case
*        the supplied text is NOT displayed!
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     -  This routine does not check the current screen size. It always
*     uses a page length of 20 lines, which may not be appropriate if,
*     for instance, an xterm window has been shrunk or expanded by the
*     user.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER TEXT*(*)
      LOGICAL INIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER PLEN               ! No. of lines per page
      PARAMETER ( PLEN = 20 )

*  Local Variables:
      INTEGER NL                 ! No. of lines displayed
      LOGICAL OK                 ! Continue?

      SAVE NL
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the page is to be initialised, reset the number of lines displayed.
      IF( INIT ) THEN
         NL = 0

*  Otherwise, if the page is full, ask the user whether to continue or
*  not, and then reset the number of lines displayed.
      ELSE
         IF( NL .EQ. PLEN ) THEN

	    CALL MSG_BLANK( STATUS )
            CALL GET0L( ' ', 1, .FALSE., COMM, 'Continue', .TRUE., OK,
     :                  STATUS )
	    CALL MSG_BLANK( STATUS )

            IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'PRPAGE_ERR', 'Paged screen output ' //
     :                       'interupted.', STATUS )
            END IF

            NL = 0

         END IF

*  Display the text.
         CALL MSG_OUT( 'PRPAGE_MSG1', TEXT, STATUS )

*  Incrment the number of lines displayed.
         NL = NL + 1

      END IF

      END
