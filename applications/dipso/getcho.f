      SUBROUTINE GETCHO( PARAMS, POS, OPT, COMM, PROMPT, MENU, DEFIND,
     :                   INDEX, VALUE, STATUS )
*+
* Name:
*    GETCHO

*  Purpose:
*     Obtain a selection from a menu of choices

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GETCHO( PARAMS, POS, OPT, COMM, PROMPT, MENU, DEFIND,
*                  INDEX, VALUE, STATUS )

*  Description:
*     A value is obtained from the supplied list of parameter values, or
*     from the user. If this matches one of the words in MENU it is
*     returned in VALUE and the index of the match within MENU is
*     returned in INDEX. If the obtained value does not have a match in
*     MENU, or if an ambiguous abbreviation is obtained, a warning is
*     given and the user is re-prompted for a new value.

*  Arguments:
*     PARAMS = CHARACTER * ( * ) (Given)
*        The string containing the list of command parameters.
*     POS = INTEGER (Given)
*        The index of the required parameter within the list of all
*        possible parameters.
*     OPT = LOGICAL (Given)
*        Is the parameter an optional parameter? If so, then the
*        supplied default value will be returned if no value has
*        been supplied. Otherwise, the user is prompted if no value
*        has been supplied.
*     COMM = CHARACTER * ( * ) (Given)
*        The command name.
*     PROMPT = CHARACTER * ( * ) (Given)
*        The prompt string.
*     MENU = CHARACTER * ( * ) (Given)
*        A list of the menu options. These should be individual words
*        seperated by spaces, or strings enclosed within single quotes.
*        Quotes, spaces and "\" characters can be escaped by preceeding
*        them with a single "\" character.
*     DEFIND= INTEGER (Given)
*        The index of the default value within MENU. If no item exists
*        with the given index in MENU, then no default is used. For
*        instance, zero can be given to suppress the issuing of a
*        default value.
*     INDEX = INTEGER (Returned)
*        The index of the obtained value within MENU.
*     VALUE = CHARACTER * ( * ) (Returned)
*        The full menu item corresponding to INDEX.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Abbreviations may be used to match words in the menu list.
*     -  The match of the obtained value against the supplied menu list
*     is case insensetive, but the returned value always matches the
*     case of the matched menu item.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}
*
*  History:
*     19-AUG-1994 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAMS
      INTEGER POS
      LOGICAL OPT
      CHARACTER * ( * ) COMM
      CHARACTER * ( * ) PROMPT
      CHARACTER * ( * ) MENU
      INTEGER DEFIND

*  Arguments Returned:
      INTEGER INDEX
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_ABBRV          ! Abbreviation?

*  Local Variables:
      CHARACTER
     :        DEFVAL*255,        ! Default value
     :        ITEM*255,          ! Menu item
     :        TVAL*255           ! Temporary value

      INTEGER
     :        I,                 ! Menu item count
     :        LDEF,              ! Length of default value
     :        LITEM,             ! Length of menu item
     :        NMATCH             ! No. of matching menu items

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the menu is empty.
      IF( MENU .EQ. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'GETCHO_ERR1', 'GETCHO: Null menu supplied '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Obtain the text of the default menu item. Report an error if the
*  default value is not defined.
      CALL FWORD( MENU, DEFIND, DEFVAL, LDEF )

*  Get a value for the parameter.
      CALL GET0C( PARAMS, POS, OPT, COMM, PROMPT, DEFVAL, TVAL,
     :            STATUS )

*  Loop round until a value has been supplied which has a single match
*  in the menu, or an error occurs.
      NMATCH = 0
      DO WHILE( NMATCH .NE. 1 .AND. STATUS .EQ. SAI__OK )

*  Find the first item in the menu
         I = 1
         CALL FWORD( MENU, I, ITEM, LITEM )

*  Loop round each of the menu items, counting the matches.
         NMATCH = 0
         DO WHILE( LITEM .GT. 0 )

*  See if the parameter value is an abbreviation of the current menu
*  item, allowing for differences in case. If it is, store the current
*  word index, and increment the number of matches found.
            IF( CHR_ABBRV( TVAL, ITEM, 1 ) ) THEN
               INDEX = I
               VALUE = ITEM
               NMATCH = NMATCH + 1
            END IF

*  Increment the current item index, and get the next item in the menu.
            I = I + 1
            CALL FWORD( MENU, I, ITEM, LITEM )

         END DO

*  If no match was found, report an error and then flush it.
*  Prompt the user for a new value.
         IF( NMATCH .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'VAL', TVAL )
            CALL MSG_SETC( 'MENU', MENU )
            CALL ERR_REP( 'GETCHO_ERR2', 'Illegal value ''^VAL'' '//
     :                    'supplied. Legal values are ''^MENU''.',
     :                    STATUS )
            CALL ERR_FLUSH( STATUS )

            CALL RDSTR( COMM, PROMPT, DEFVAL, TVAL, STATUS )

*  If more than one match was found, report an error and then flush it.
*  Prompt the user for a new value.
         ELSE IF( NMATCH .GT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'VAL', TVAL )
            CALL MSG_SETC( 'MENU', MENU )
            CALL ERR_REP( 'GETCHO_ERR3', 'Ambiguous value ''^VAL'' '//
     :                    'supplied. Legal values are ''^MENU''.',
     :                    STATUS )
            CALL ERR_FLUSH( STATUS )

            CALL RDSTR( COMM, PROMPT, DEFVAL, TVAL, STATUS )

         END IF


      END DO

*  Jump to here if an error occurs.
 999  CONTINUE

      END
