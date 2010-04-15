      SUBROUTINE IRM_TMENU( INMENU, INTITL, PARAM, ITEMNO, STATUS )
*+
*  Name:
*     IRM_TMENU

*  Purpose:
*     Display a menu on user's terminal and get an item from it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_TMENU( INMENU, INTITL, PARAM, ITEMNO, STATUS )

*  Description:
*     This routine displays a menu on the user's terminal and let user
*     select an item from the menu. This menu is displayed an item per
*     line with each item has a sequence number. When selecting an item,
*     the user can key in its sequence number, or key in the item itself
*     in this case the user can abbreviate the item to an unambiguous
*     length. The Title are displayed at the top of the menu and
*     underlined. The first item is set as default selection.

*  Arguments:
*     INMENU = CHARACTER*(*) (Given)
*        The menu list of the items separated by comma.
*     INTITL = CHARACTER*(*) (Given)
*        The title of the menu.
*     PARAM = CHARACTER*(*) (Given)
*        The name of the parameter used to select one of the items from
*        the menu.
*     ITEMNO = INTEGER (Returned)
*        The selected item number.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     1-MAR-1991 (WG):
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
      CHARACTER*(*) INMENU
      CHARACTER*(*) INTITL
      CHARACTER*(*) PARAM

*  Arguments Returned:
      INTEGER ITEMNO

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      INTEGER MAXMEN             ! Max. length ot the menu
      PARAMETER ( MAXMEN = 260 )
      INTEGER MITMLN             ! Max. item length
      PARAMETER ( MITMLN = 60 )
      INTEGER MAXITM             ! Max. number of items in the menu
      PARAMETER ( MAXITM = 30 )
      INTEGER MTITLN             ! Max. length of the title
      PARAMETER ( MTITLN = 80 )

*  Local Variables:
      CHARACTER*( MITMLN ) ITEM( MAXITM )  ! Each item of the menu
      CHARACTER*( MAXMEN ) MENU            ! Menu string
      CHARACTER*( 2 ) NUM                  ! Number code of an item
      CHARACTER*( MITMLN ) SELITM          ! Selected item
      CHARACTER*( MTITLN ) TITLE           ! Title string
      CHARACTER*( MTITLN ) TMPLIN          ! Temporary underline
      CHARACTER*( MTITLN ) UNDLIN          ! Underline of the title

      INTEGER I                  ! Do loop index
      INTEGER ITMBGN             ! Begin position of an item
      INTEGER ITMEND             ! End position of an item
      INTEGER MENULN             ! The length of the menu
      INTEGER NITEM              ! Number of items in the menu
      INTEGER NSEL               ! Number of selected item
      INTEGER NUMLN              ! Length of a number code
      INTEGER SELLN              ! The length of selection
      INTEGER TITLN              ! The length of the title

      LOGICAL MORE               ! More item flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove the leading blank of MENU and get its used length.
      MENU = INMENU
      CALL CHR_LDBLK( MENU )
      MENULN = CHR_LEN( MENU )

*  If the used length is 0, set status to error, report and exit.
      IF ( MENULN .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_TMENU_ERR1',
     :                 'IRM_TMENU: A blank menu has been supplied.',
     :                 STATUS )
         GOTO 999
      END IF

*  Set the begin position of the first item, and initialise number of
*  items.
      ITMBGN =1
      NITEM = 0

*  Extract items from the menu until no item left.
      MORE = .TRUE.
      DO WHILE ( MORE )
         NITEM = NITEM + 1

*  Find the relative end position of this item.
         ITMEND = INDEX( MENU( ITMBGN : ), ',' )

*  Find the absolute end position of this item.
         IF ( ITMEND .NE. 0 ) THEN
            ITMEND = ITMBGN + ITMEND - 2

*  Since the last item does not end with comma, its end position should
*  be the end of the menu list. And in this case no more item left.
         ELSE
            ITMEND = MENULN
            MORE = .FALSE.
         END IF

*  Extract the item from the menu list, and remove leading blank.
         ITEM( NITEM ) = MENU( ITMBGN : ITMEND )
         CALL CHR_LDBLK( ITEM( NITEM ) )

*  Get the begin position of next item.
         ITMBGN = ITMEND + 2

*  Go back to extract another item if any item left.
      END DO

*  Remove the leading blank of the menu tile.
      TITLE = INTITL
      CALL CHR_LDBLK( TITLE )
      TITLN = CHR_LEN( TITLE )

*  If there is a title, write it.
      IF ( TITLN .GT. 0 ) THEN

*  Construct the underline for the menu title.
         TMPLIN = '-'
         UNDLIN = '-'
         DO I = 1, TITLN - 1
            UNDLIN = TMPLIN( : I )//'-'
            TMPLIN = UNDLIN( : I + 1 )
         END DO

*  Displace the menu title.
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_SETC( 'TITL', TITLE( :TITLN ) )
         CALL MSG_OUT( ' ', '   ^TITL ', STATUS )
         CALL MSG_SETC( 'UNDE', UNDLIN( :TITLN) )
         CALL MSG_OUT( ' ', '   ^UNDE ', STATUS )
      END IF

*  Displace the items of the menu.
      DO I = 1, NITEM
         CALL MSG_SETI( 'NUM', I )
         CALL MSG_SETC( 'ITM', ITEM( I ) )
         CALL MSG_OUT( ' ', '      ^NUM. ^ITM.', STATUS )
      END DO
      CALL MSG_OUT( ' ', ' ', STATUS )

*  Set the first item as the default.
      CALL PAR_DEF0C( PARAM, ITEM( 1 ), STATUS )

*  Enter a do loop until one and only one valid item is selected.
      NSEL = 0
      DO WHILE( NSEL .NE. 1 )

*  Get a value for the parameter from the environment.
         CALL PAR_GET0C( PARAM, SELITM, STATUS )

*  Remove all blanks in it, convert it to upper case and get its used length.
         CALL CHR_RMBLK( SELITM )
         CALL CHR_UCASE( SELITM )
         SELLN = CHR_LEN( SELITM )

*  Determine the item number of the selection.
         DO I = 1, NITEM

*  Remove all blanks of the ith item and convert it to upper case.
            CALL CHR_RMBLK( ITEM( I ) )
            CALL CHR_UCASE( ITEM( I ) )

*  Get its character item number code
            CALL CHR_ITOC( I, NUM, NUMLN )

*  Comparet with the selected one.
            IF ( INDEX( ITEM( I ), SELITM( :SELLN ) ) .EQ. 1
     :           .OR. NUM( : NUMLN ) .EQ. SELITM( :SELLN ) ) THEN
               NSEL = NSEL + 1
               ITEMNO = I
            END IF
         END DO

*  If none has been selected, report a message, canncel the parameter
*  for re-input
         IF ( NSEL .EQ. 0 ) THEN
            CALL MSG_OUT( 'IRM_TMENU_MSG1',
     :                    'None valid item selected, retry.',
     :                     STATUS )
            CALL PAR_CANCL( PARAM, STATUS )

* If more then one item have been selected report a message, canncel
* the parameter for re-input.
         ELSE IF ( NSEL .GT. 2 ) THEN
            CALL MSG_OUT( 'IRM_IMENU_MSG2',
     :                    'Ambiguous selection, retry.', STATUS )
            CALL PAR_CANCL( PARAM, STATUS )

*  And set the number of selected item back to 0.
            NSEL = 0

         END IF

*  Go back continue the selection if on valid or more than one
*  valid item are selected.
      END DO

 999  CONTINUE

      END
