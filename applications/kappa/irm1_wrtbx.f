      SUBROUTINE IRM1_WRTBX( X1, X2, Y1, Y2, NLIN, LINSTR, COLOUR, 
     :                       FONT, TXTHT, STATUS )
*+
*  Name:
*     IRM1_WRTBX

*  Purpose:
*     Draw a box and write contents into it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM1_WRTBX( X1, X2, Y1, Y2, NLIN, LINSTR, COLOUR, FONT, 
*                      STATUS )

*  Description:
*     Draw a box with given extent and write specified strings into the
*     box. The text size is determined by the size of the box and the
*     number of the charaters written into the box. 

*  Arguments:
*     X1, X2, Y1, Y2 = REAL (Given)
*        The extent of the box in world coordinates.
*     NLIN = INTEGER (Given)
*        The number of lines of strings to be written into the box.
*     LINSTR( NLIN ) = CHARACTER*( * )
*        The strings to be written into the box.
*     COLOUR = LOGICAL (Given)
*        If true, the colour is available on current device, the box
*        will be draw in green. The strings will be written into the
*        box in white. Otherwise, both the box and the strings will be
*        written in white.
*     FONT = INTEGER (Given)
*        Font number, 1 is ordinary Roman letters and Arabic numbers.
*        For other available font code, see GKS User Guide.
*     TXTHT = REAL (Given)
*        The height of the text.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     2-FEB-1991 (WG):
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
      REAL X1, X2, Y1, Y2
      INTEGER NLIN
      CHARACTER*( * ) LINSTR( NLIN )
      LOGICAL COLOUR
      INTEGER FONT
      REAL TXTHT
      
*  Status:
      INTEGER STATUS             ! Global status

*  External Reference:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Constants:
      REAL ASPCT                 ! Text aspect ratio
      PARAMETER ( ASPCT = 0.667 )

*  Local Variables:
      INTEGER I                  ! Do loop idex.
      INTEGER STRLN              ! The used length of a string
      INTEGER PEN1               ! The first SGS pen
      INTEGER PEN2               ! The second SGS pen
      REAL XPSN                  ! X position of the text
      REAL YEXT                  ! Y extent of the text
      REAL YPSN                  ! Y position of a line

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If colour is available set pen 1 to white, pen2 to green.
      IF ( COLOUR ) THEN
         PEN1 = 1
         PEN2 = 3

*  Otherwise, set both pens to white
      ELSE
         PEN1 = 1
         PEN2 = 1
      END IF

*  Draw the box with pen 2
      CALL SGS_SPEN( PEN2 )
      CALL SGS_BOX( X1, X2, Y1, Y2 )

*  Set text height, aspect ratio, font and justification.
      CALL SGS_SHTX( TXTHT )
      CALL SGS_SARTX( ASPCT )
      CALL SGS_SFONT( FONT )
      CALL SGS_STXJ( 'TC' )

*  Select pen 1 to write the text into box.
      CALL SGS_SPEN( PEN1 )
      
*  Calculate the X position of the text.
      XPSN = X1 + ( X2 - X1 ) * 0.5
      
*  Calculate the Y extent of the text.
      YEXT = FLOAT( NLIN ) * TXTHT + FLOAT( NLIN -1 ) * 0.5 * TXTHT

*  Calculate the Y position of the first line
      YPSN = Y2 - ( ( Y2 - Y1 ) - YEXT ) * 0.5

*  Write string into box line by line
      DO I = 1, NLIN
         CALL CHR_LDBLK( LINSTR( I ) )
         STRLN = CHR_LEN( LINSTR( I ) )
         CALL SGS_TX( XPSN, YPSN, LINSTR( I )( : STRLN ) ) 

*  Calculate the Y position of the next line.
         YPSN = YPSN - 1.5 * TXTHT
      END DO

*  Flush out.
      CALL SGS_FLUSH

 999  CONTINUE

      END
