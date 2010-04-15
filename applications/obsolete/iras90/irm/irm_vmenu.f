      SUBROUTINE IRM_VMENU( NITEM, INMENU, NROW, DEFNO, X1, X2, Y1, Y2,
     :                      INTITL, COLOUR, FONT, TXTHT, GTITM, ITEMNO,
     :                      STATUS )
*+
*  Name:
*     IRM_VMENU

*  Purpose:
*     Draw a vertical menu and select an item from it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_VMENU( NITEM, INMENU, NROW, DEFNO, X1, X2, Y1, Y2,
*                     INTITL, COLOUR, FONT, TXTHT, GTITM, ITEMNO,
*                     STATUS )

*  Description:
*     This routine draw a vertical menu in the current SGS zone,
*     and select one of item from the menu with cursor.
*
*     The extent of the menu are specified with world coordinate.
*     The menu is composed of a number of vertically arranged
*     identical boxes, each of which contains one item of the menu.
*
*     The routine is not clever enough to decide the best number of
*     lines when writing the content of an item into the box. So
*     the calling routine must specifies approximately how many
*     lines the content of an item should be split into when writing
*     the item into the box. The routine will find the best way to
*     split the content of the item into the approximately specified
*     number of lines. The actual number of line may be more or less
*     than the specified number.
*
*     If selecting an item is requested, the routine will loop until
*     one item is selected.  To selecting an item from the menu with
*     the cursor, put the cursor in the box of that item and press any
*     key. The routine will put cursor on the default item specified
*     by DEFNO after drawing the menu and after each invalid selection.
*
*     Before calling this routine a SGS device must be opened.
*
*  Arguments:
*     NITEM = INTEGER (Given)
*        The number of items in the menu.
*     INMENU = CHARACTER*(*) (Given)
*        The list of the items in the menu separated by comma. No space
*        is allowed after each comma.
*     NROW( NITEM ) = INTEGER (Given)
*        The approximate number of lines into which the content of
*        each item should be split when writing it into its menu box.
*     DEFNO = INTEGER (Given)
*        Specifies the item number which will be used as default item.
*     X1, X2, Y1, Y2 = REAL (Given)
*        The extent of menu bar in the current world coordinate system.
*     INTITL = CHARACTER*(*) (Given)
*        The title of the menu.
*     COLOUR = LOGICAL (Given)
*        If it is true, colour is available on the current graphic
*        device. The frame of the menu box will draw in green while the
*        contents of menu will written in white. Otherwise, the colour
*        is not available, both the frame and the contents will be in
*        white.
*     FONT = INTEGER (Given)
*        Font code, 1 is ordinary Roman letters and Arabic numbers. For
*        other available font code, see GKS User Guide.
*     TXTHT = REAL (Given)
*        The height of the text in the menu.
*     GTITM = LOGICAL (Given)
*        If true, an item will be selected from the menu, otherwise no
*        item will be selected before exit.
*     ITEMNO = INTEGER (Returned)
*        The item number which user selected from the menu.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     2-FEB-1991
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                  ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'              ! Standard SAE constants

*  Arguments Given:
      INTEGER NITEM
      CHARACTER*(*) INMENU
      INTEGER NROW( NITEM )
      INTEGER DEFNO
      REAL X1, X2, Y1, Y2
      CHARACTER*(*) INTITL
      LOGICAL COLOUR
      INTEGER FONT
      REAL TXTHT
      LOGICAL GTITM

*  Arguments Returned:
      INTEGER ITEMNO

*  Status:
      INTEGER STATUS                ! Global status

*  External Reference:
      INTEGER CHR_LEN

*  Local constants:
      REAL ASPCT                    ! Aspect ratio of the text.
      PARAMETER ( ASPCT = 0.6667 )
      INTEGER MAXLIN                ! Max. line number of an item
      PARAMETER ( MAXLIN = 5 )
      INTEGER MLINLN                ! Max. line length of an item
      PARAMETER ( MLINLN = 30 )
      INTEGER MXMULN                ! Max. length of the menu string
      PARAMETER ( MXMULN = 512 )

*  Local Variables:
      INTEGER I                     ! Do loop index
      INTEGER ITMBGN                ! Begin position of an item
      INTEGER ITMEND                ! End position of an item
      INTEGER MENULN                ! Used length of the menu list
      INTEGER NKEY                  ! Key number
      INTEGER NLIN                  ! Number of lines of an item
      INTEGER OLDNF                 ! Old fond code
      INTEGER OLDNPR                ! Old text precision
      INTEGER TITLN                 ! Used length of a title line

      REAL BOXHI                    ! Height of an item box
      REAL CURX                     ! Cursor x position
      REAL CURY                     ! Cursor y position
      REAL ITMY1, ITMY2             ! X extent of an item box
      REAL OLDHT                    ! Old text height
      REAL OLDAR                    ! Old text aspect ratio
      REAL OLDXU, OLDYU             ! Old text direction vector
      REAL OLDSP                    ! Old character spacing
      REAL TXTHI                    ! Text height of the title
      REAL XTMP                     ! X position when write title
      REAL YTMP                     ! Y position when write title

      LOGICAL SELITM                ! If true, continue selecting.
                                    ! Otherwise, stop selecting

      CHARACTER*( MXMULN ) MENU     ! Menu string
      CHARACTER*( MLINLN ) SUBSTR( MAXLIN )
                                    ! Each line of an item
      CHARACTER*( 2 ) OLDTXJ        ! Old text justification
      CHARACTER*( 80 ) TITLE        ! Title of the menu

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Remove the leading blank and get the used length of the menu list.
      MENU = INMENU
      CALL CHR_LDBLK( MENU )
      MENULN = CHR_LEN( MENU )

*  If the menu is blank, set status, report error and exit.
      IF ( MENULN .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'IRM_VMENU_ERR1',
     :                 'IRM_VMENU: A blank menu has been supplied.',
     :                 STATUS )
         GOTO 999
      END IF

*  Store the original text attributes
      CALL SGS_ITXA( OLDNF, OLDNPR, OLDHT, OLDAR, OLDXU, OLDYU,
     :               OLDSP, OLDTXJ )

*  Remove the leading blank of the title and get its used length.
      TITLE = INTITL
      CALL CHR_LDBLK( TITLE )
      TITLN = CHR_LEN( TITLE )
*
*  If there is menu title, write it
      IF ( TITLN .NE. 0 ) THEN

*  Set the text height and aspect ratio of the title.
         TXTHI = 0.3 * ( Y2 - Y1 ) / FLOAT( NITEM )
         CALL SGS_SHTX( TXTHI )
         CALL SGS_SARTX( ASPCT )

*  Set text font.
         CALL SGS_SFONT( FONT )

*  Set text justification for the title.
         CALL SGS_STXJ( 'TC' )

*  Calculate the position of the title line.
         XTMP = ( X1 + X2 ) * 0.5
         YTMP = Y2+ 1.5 * TXTHI

*  Write the title.
         CALL SGS_TX( XTMP, YTMP, TITLE( : TITLN ) )

      END IF

*  Calculate the height of each item box.
      BOXHI = ( Y2 - Y1 ) / FLOAT( NITEM )

*  Calculate the Y extent of the first item box.
      ITMY2 = Y2
      ITMY1 = Y2 - BOXHI + 0.05 * BOXHI

*  Extract the items from the menu list and draw them one by one.
*  Initialise the begin postion of an item.
      ITMBGN = 1
      DO I = 1, NITEM

*  Find the relative position of next comma.
         ITMEND = INDEX( MENU( ITMBGN : ), ',' )

*  Find the absolute end positon of next item.
         IF ( ITMEND .NE. 0 ) THEN
            ITMEND = ITMBGN + ITMEND - 2

*  Since the last item does not end with comma, its end should
*  be the end of the menu list.
         ELSE
            ITMEND = MENULN

*  If at this time the specified number of item have not been reached,
*  then set the status, report the error and exit.
            IF ( I .LT. NITEM ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'IRM_VMENU_ERR2',
     :               'IRM_VMENU: Not enought items given in menu list.',
     :                        STATUS )
               GOTO 999
            END IF
         END IF

*  Split the content of the Ith item into specified lines.
         CALL IRM1_SPLIT( MENU( ITMBGN: ITMEND ), NROW( I ), MAXLIN,
     :                    NLIN, SUBSTR, STATUS )

*  Draw the box of this item and write its contents into the box.
         CALL IRM1_WRTBX( X1, X2, ITMY1, ITMY2, NLIN, SUBSTR,
     :                    COLOUR, FONT, TXTHT, STATUS )

*  Calculate the extent of the next item box if the last item has not
*  been reached.
         IF ( I .LT. NITEM ) THEN
            ITMY2 = ITMY2 - BOXHI
            ITMY1 = ITMY2 - BOXHI + 0.05 * BOXHI
         END IF

*  Get the begin position of next item.
         ITMBGN = ITMEND + 2

*  Go back to write next item if there is any.
      END DO

*  Flush out all buffers.
      CALL SGS_FLUSH

*  If selecting item by cursor, set the cursor visible.
      IF ( GTITM ) THEN
         CALL SGS_CUVIS( .TRUE. )

*  Looping until a item is selected with the cursor.
         SELITM = .TRUE.
         DO WHILE( SELITM )

*  Set the cursor at the position of the DEFNO item.
            CALL SGS_SETCU( 0.5 * ( X1 + X2 ),
     :                      Y2 - ( DEFNO - 1 + 0.5 ) * BOXHI )

*  Get the cursor position
            CALL SGS_REQCU( CURX, CURY, NKEY )

*  If the cursor is in the menu, calculate which item it selects and
*  end the do loop.
            IF ( ( CURX .GE.X1 ) .AND. ( CURX .LE. X2 ) .AND.
     :           ( CURY .GE.Y1 ) .AND. ( CURY .LE. Y2 ) ) THEN
                 ITEMNO = 1 + INT( ( Y2 - CURY ) / BOXHI )
                 SELITM = .FALSE.
            END IF

*  Go back if no item is selected.
         END DO

*  Delect the title of the menu.
         IF ( TITLN .NE. 0 ) THEN
            CALL SGS_CLRBL( XTMP - 0.5 * TITLN * TXTHI,
     :                      XTMP + 0.5 * TITLN * TXTHI,
     :                      Y2 + 0.1 * TXTHI, YTMP )
         END IF
*  If the first item is selected, clear the items below it.
         IF ( ITEMNO .EQ. 1 ) THEN
            YTMP= Y2 - BOXHI + 0.025 * BOXHI
            CALL SGS_CLRBL( X1, X2, Y1, YTMP )

*  If the middle item is selected, clear the items above and below it.
         ELSE IF ( ITEMNO .GT. 1 .AND. ITEMNO .LT. NITEM ) THEN
            YTMP = Y2 - BOXHI * REAL(ITEMNO -1 ) + 0.025 * BOXHI
            CALL SGS_CLRBL( X1, X2, YTMP, Y2 )
            YTMP = YTMP - BOXHI
            CALL SGS_CLRBL( X1, X2, Y1, YTMP )

*  If the last item is selected, clear the items above it.
         ELSE IF ( ITEMNO .EQ. NITEM ) THEN
            XTMP = Y1 + BOXHI + 0.025 * BOXHI
            CALL SGS_CLRBL( X1, X2, YTMP, Y2 )
         END IF

*  Flush out the buffers.
         CALL SGS_FLUSH

      END IF

*  Recover the original text attributes
      CALL SGS_SFONT( OLDNF )
      CALL SGS_SHTX( OLDHT )
      CALL SGS_SARTX( OLDAR )
      CALL SGS_SUPTX( OLDXU, OLDYU )
      CALL SGS_STXJ( OLDTXJ )

 999  CONTINUE

      END

