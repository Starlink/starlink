      SUBROUTINE IRM_TABLE( TITLE, NROW, NCOL, TABLE, X1, X2,
     :                      Y1, Y2, COLOUR, FONT, YBTTM, STATUS )
*+
*  Name:
*     IRM_TABLE

*  Purpose:
*     Draw a table at specify position with specify size.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_TABLE( TITLE, NROW, NCOL, TABLE, X1, X2, Y1, Y2,
*                     COLOUR, FONT, STATUS )

*  Description:
*     This routine draw a table in the current SGS zone at the specified
*     position. The X extent of the table is specified by calling routine
*     while the Y extent will be determined by number of rows of the table.
*     with the max. limit given by the calling routine.
*     The columns in the table are separated by vertical lines.
*     The rows are separated by a space of a text height except the first
*     row which is separated from the second by a horizontal line.
*     Therefore it is suggested that using first item of each column as
*     the title of this column.
*
*     The size of the text is determined by the size of the table and
*     its contents.
*
*     The title of the table is put at the top of the table.
*
*     Before calling this routine the SGS device must be opened.
*
*  Arguments:
*     TITLE = CHARACTER*(*) (Given)
*        The title of the table, a blank string will suppress the title.
*     NROW = INTEGER (Given)
*        The number of rows of the table.
*     NCOL = INTEGER (Given)
*        The number of columns of the table.
*     TABLE( NROW, NCOL ) = CHARACTER*(*) (Given)
*        The contents of the table to be displayed.
*     X1, X2 = REAL (Given)
*        The X extent of the table in world coordinate.
*     Y1 = REAL (Given)
*        The lower limit of the bottom margin of the table.
*     Y2 = REAL (Given)
*        Y position of the Top margin of the table.
*     COLOUR = LOGICAL (Given)
*        If true, the frame of the table will be draw in green while
*        the contents of the table will be write in white. Otherwise
*        both will be in white.
*     FONT = INTEGER (Given)
*        Font code, 1 is ordinary Roman letters and Arabic numbers. For
*        other available font code, See GKS User Guide.
*     YBTTM = REAL (Returned)
*        The real Y position of the bottom margin of the table.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     7-Feb-1991 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                    ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                ! Standard SAE constants

*  Arguments Given:
      INTEGER NROW
      INTEGER NCOL
      INTEGER FONT

      CHARACTER*(*) TITLE
      CHARACTER*(*) TABLE( NROW, NCOL )

      LOGICAL COLOUR

      REAL X1, X2, Y1, Y2

*  Arguments Returned:
      REAL YBTTM

*  Status:
      INTEGER STATUS                  !Global status

*  External reference:
      INTEGER CHR_LEN                  ! The used length of a string

*  Local constants:
      REAL ASPCT                      ! The aspect ratio of the text
      PARAMETER ( ASPCT = 0.57 )
      INTEGER MAXCOL                  ! Max. number of columns
      PARAMETER ( MAXCOL = 100 )

*  Local variables:
      INTEGER I, J                    ! Do loop indice
      INTEGER ITMLN                   ! The used length of an item
      INTEGER MITMCL( MAXCOL )        ! Max item length in each column
      INTEGER NFOLD                   ! Old fond code
      INTEGER NPROLD                  ! Old text precision
      INTEGER PEN1, PEN2              ! First and second pen
      INTEGER TITLN                   ! The used length of the title

      REAl AROLD                      ! Old text aspect ratio
      REAL HI1, HI2                   ! Two possible text height
      REAL HTOLD                      ! Old text height
      REAL ROWLN                      ! Max. row length
      REAL SPOLD                      ! Old character spacing
      REAL TXTHI                      ! Text height
      REAL TXTWTH                     ! Text width
      REAL WDTH                       ! A possible text width
      REAL XTMP                       ! Temporary X position
      REAL XUOLD, YUOLD               ! Old text up direction vecter
      REAL YTMP                       ! Temporary Y position

      CHARACTER*( 2 ) TXJOLD          ! Old text justification

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Calculate the max. item length in each column.
      ROWLN = 0.0
      DO I = 1, NCOL
         MITMCL( I ) = 0
         DO J = 1, NROW

*  Get the used length of the item.
            ITMLN = CHR_LEN( TABLE( J, I ) )
            IF ( MITMCL( I ) .LT. ITMLN ) MITMCL( I ) = ITMLN
         END DO

*  If this column is empty, set status to error, report end exit.
         IF ( MITMCL( I ) .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'IRM_TABLE_ERR1',
     :                   'IRM_TABLE: An empty column has been supplied',
     :                     STATUS )
            GOTO 999
         END IF

* Count the total row length.
         ROWLN = ROWLN + 1.5 * FLOAT( MITMCL( I ) ) + 2.0
      END DO

*  Calculate the possible width of the text from the width of table.
      WDTH =  ( X2 - X1 ) / ROWLN

*  Calculate the height of the text from above width.
      HI1 = WDTH / ASPCT

*  Calculate the height of the text from the height of the table.
      HI2 = ( Y2 - Y1 ) / ( 2.0*REAL( NROW ) + 1.0 )

*  Get the text height from above two possible height
      TXTHI = MIN( HI1, HI2 )

*  Get the text width.
      TXTWTH = TXTHI * ASPCT

*  Store the original text attributes.
      CALL SGS_ITXA( NFOLD, NPROLD, HTOLD, AROLD, XUOLD, YUOLD,
     :               SPOLD, TXJOLD )

*  Set orientation, height, aspect ratio and font of the text.
      CALL SGS_SFONT( FONT )
      CALL SGS_SUPTX( 0.0, 1.0 )
      CALL SGS_SHTX( TXTHI )
      CALL SGS_SARTX( ASPCT )

*  If colour is available set pen 1 to white, pen 2 to green.
      IF ( COLOUR ) THEN
         PEN1 = 1
         PEN2 = 3

*  Otherwise, set both pens to white.
      ELSE
         PEN1 = 1
         PEN2 = 1
      END IF

*  Get the used length of the title
      TITLN = CHR_LEN( TITLE )

*  If the table has a title, display the title.
      IF ( TITLN .GT. 0 ) THEN

*  Select pen 1, and set text juxtification.
         CALL SGS_SPEN( PEN1 )
         CALL SGS_STXJ( 'CC' )

*  Calculate the position of the title and write the title.
         XTMP = ( X1 + X2 ) / 2.0
         YTMP = Y2 + TXTHI
         CALL SGS_TX( XTMP, YTMP, TITLE( : TITLN ) )
      END IF

*  Draw the top margin of the table.
      CALL SGS_SPEN( PEN2 )
      CALL SGS_LINE( X1, Y2, X2, Y2 )

*  Calculate the position of separate line for first row and draw it.
      YTMP = Y2 - 2.0 * TXTHI
      CALL SGS_LINE( X1, YTMP, X2, YTMP )

*  Calculate the position of first row of the table.
      YTMP = ( YTMP + Y2 ) /2.0

*  Select pen 1 and set text justification.
      CALL SGS_SPEN( PEN1 )
      CALL SGS_STXJ( 'CL' )

*  Draw the first item of each column.
      DO I = 1, NCOL

*  Calcuate the x position of this item.
         IF ( I .EQ. 1 ) THEN
            XTMP = X1 + TXTWTH
         ELSE
            XTMP = XTMP + ( X2 - X1 )
     :                  * ( 1.5 * FLOAT( MITMCL( I - 1 ) ) + 2.0 )
     :                  / ROWLN
         END IF

*  Get the used length of the table element
         ITMLN = CHR_LEN( TABLE( 1, I ) )

*  Display this item.
         CALL SGS_TX( XTMP, YTMP, TABLE( 1, I )( : ITMLN ) )
      END DO

*  Calculate y position of the second row.
      YTMP = YTMP - 2.0 * TXTHI

*  Draw remaining table contents.
      DO J = 2, NROW
         DO I = 1, NCOL

*  Calculate the X position of each item.
            IF ( I .EQ. 1 ) THEN
               XTMP = X1 + TXTWTH
            ELSE
               XTMP = XTMP + ( X2 - X1 )
     :                     * ( 1.5 * FLOAT( MITMCL( I - 1 ) ) + 2.0 )
     :                     / ROWLN
            END IF

*  Get the used length of this item.
            ITMLN = CHR_LEN( TABLE( J, I ) )

*  Write the item.
            CALL SGS_TX( XTMP, YTMP, TABLE( J, I )( : ITMLN ) )
          END DO

*  Get the Y position of the next row.
         YTMP = YTMP - 2.0 * TXTHI

*  Go back if there is any row remaining.
      END DO

*  Select pen 2 to draw other parts of the table frame.
      CALL SGS_SPEN( PEN2 )

*  Calculate the bottom margin position of the table.
      YTMP = YTMP + 0.5 * TXTHI

*  Draw the bottom margin of the table
      CALL SGS_LINE( X1, YTMP, X2, YTMP )

*  Draw left and right margin of the table.
      CALL SGS_LINE( X1, Y2, X1, YTMP )
      CALL SGS_LINE( X2, Y2, X2, YTMP )

*  Draw column separate lines.
      XTMP = X1
      DO I = 2, NCOL

*  Calculate x position of the line and draw it.
         XTMP = XTMP + ( X2 - X1 )
     :               * ( 1.5 * FLOAT( MITMCL( I - 1 ) ) + 2.0 )
     :               / ROWLN
         CALL SGS_LINE( XTMP, Y2, XTMP, YTMP )
      END DO

*  Return the real position of the bottom margin.
      YBTTM = YTMP

*  Flush out the table.
      CALL SGS_FLUSH

*  Recover the original text attributes.
      CALL SGS_SFONT( NFOLD )
      CALL SGS_SHTX( HTOLD )
      CALL SGS_SARTX( AROLD )
      CALL SGS_SUPTX( XUOLD, YUOLD )
      CALL SGS_STXJ( TXJOLD )

 999  CONTINUE

      END


