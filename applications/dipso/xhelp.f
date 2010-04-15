      SUBROUTINE XHELP( SHOWME, HLPSTR, OK, STATUS )
*+
*  Name:
*     XHELP

*  Purpose:
*     Issue hypertext help

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL XHELP( SHOWME, HLPSTR, OK, STATUS )

*  Description:
*     An attempt is made to issue hypertext help so long as OK is
*     supplied .TRUE.. If the environment variable DISPLAY is not
*     defined, no hypertext help is issued and OK is returned .FALSE.
*     (although no error is reported). Otherwise, the SHOWME command is
*     used to access the hypertext version of SUN/50. The supplied help
*     string is used as a label within SUN/50, to which a jump is made.

*  Arguments:
*     SHOWME = CHARACTER (Given)
*        A string holding a bourne shell command which will run the
*        Starlink 'showme' command.
*     HLPSTR = CHARACTER * ( * ) (Given and Returned)
*        The help string. Returned in upper case.
*     OK = LOGICAL (Given and Returned)
*        If supplied .FALSE. an immediate return is made. Otherwise, OK
*        is returned set to .FALSE. if hypertext help could not be
*        issued.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-DEC-1995 (DSB):
*        Original version.
*     11-JUN-1996 (DSB):
*        Argument SHOWME added.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER SHOWME*(*)

*  Arguments Given and Returned:
      CHARACTER HLPSTR*(*)
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER DISP*80          ! Value of $DISPLAY
      CHARACTER LTEXT*512        ! Local text buffer

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Ensure the string is in upper case.
      CALL CHR_UCASE( HLPSTR )

*  The OK argument will be supplied .FALSE. if hypertext help is
*  not wanted. Return if this is the case.
      IF( .NOT. OK ) RETURN

*  Attempt to translate the environment variable DISPLAY to see if we
*  are at an X-window. If this cannot be done, then no hypertext help
*  is available
      CALL PSX_GETENV ( 'DISPLAY', DISP, STATUS )

*  If it was not defined, annul the error message and set OK .FALSE.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )
         OK = .FALSE.

*  Otherwise, set OK .FALSE. if the translation is blank.
      ELSE IF( DISP .EQ. ' ' ) THEN
         OK = .FALSE.

      END IF

*  If we are at an X-terminal...
      IF( OK ) THEN

*  Use the SHOWME command (see SUN/188) to create the hypertext help by
*  displaying the relevant part of the installed hypertext version of
*  sun50.
         LTEXT = SHOWME( : CHR_LEN( SHOWME ) )//' sun50 '//HLPSTR
         CALL SYSEXE( LTEXT, 0, STATUS )

*  If this is not an x-terminal, issue a warning.
      ELSE
         CALL MSG_OUT( 'XHELP_MSG1', 'WARNING: No translation for '//
     :                 '$DISPLAY, therefore hypertext help cannot be '//
     :                 'used.', STATUS )
      END IF

      END
