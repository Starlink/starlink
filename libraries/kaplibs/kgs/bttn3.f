      SUBROUTINE BTTN3( LB1, LB2, LBE, LTX, LTY, USECUR, STATUS )
*+
*  Name:
*     BTTN3

*  Purpose:
*     Displays trackerball box menu on an image display for.

*  Language:
*     VAX Fortran

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BTTN3( LB1, LB2, LBE, LTY, LTX, USECUR, STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This subroutine draws three boxes at the bottom of the image-
*     display screen in which the function of the first two buttons and
*     the escape button of the trackerball box are written.  If the
*     trackerball is also to be used then two arrows with circles around
*     them are plotted beneath the boxes.  One points in the y direction
*     (up and down), the message beside it indicating what moving the
*     trackerball in the y direction does; the other points in x (left
*     and right), the message beside it indicating the effect moving the
*     trackerball in x has.  If either label is set to '*' then the
*     message and the corresponding arrow will not be plotted.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     Save the input graphics parameters
*     Clear the current workstation
*     Set the world co-ordinates 0 to 1 in both axes
*     If there was a graphics error, report context and return
*     If the trackerball is to be used, then
*        Assign an offset in the y direction to provide space at the
*          bottom of the display for the arrows
*     End if
*     Draw three blue boxes
*     Set the text attributes
*     Annotate the boxes in green, white and red for buttons 1, 2 and
*       escape respectively
*     If the trackerball is to be used then
*        Draw yellow horizontal and vertical arrows within circles
*        Set text attributes
*        Annotate arrows if required
*     End if
*     Reset the graphics attributes to their entry values
*     If there was a graphics error, report context
*     End

*  Copyright:
*     Copyright (C) 1987, 1988, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     1987 Sep 3  : Original based on Ken Hartley's modified KFH_BTTN4
*        (BTTN4) (RL.STAR::CUR).
*     1988 Jun 23 : Added GKS status check (RL.STAR::CUR).
*     1990 Jan 9  : Corrected SGS status (RL.STAR::CUR).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      CHARACTER*(*)
     :    LB1,
     :    LBE,
     :    LTX,
     :    LTY,
     :    LB2

      LOGICAL USECUR


*  Status:
      INTEGER STATUS


*  Local Constants:
      INTEGER
     :    BLUE,                 ! Number of blue pen
     :    GREEN,                ! Number of green pen
     :    MAXLEN,               ! Maximum length of a label
     :    RED,                  ! Number of red pen
     :    WHITE,                ! Number of white pen
     :    YELLOW                ! Number of yellow pen
      PARAMETER ( BLUE = 4 )
      PARAMETER ( GREEN = 3 )
      PARAMETER ( MAXLEN = 10 )
      PARAMETER ( RED = 2 )
      PARAMETER ( WHITE = 1 )
      PARAMETER ( YELLOW = 5 )

      REAL
     :    XINDEN,               ! Indentation of box
     :    YOFFS,                ! Y offset of the box if cursor is used
     :    ARROWL,               ! Arrow height
     :    ARROW,                ! Arrow width
     :    ARTXHT,               ! Arrow annotation height
     :    TXHT,                 ! Box annotation height
     :    XARR,                 ! X start of left arrow
     :    XARRR,                ! X centre of right arrow
     :    YARR,                 ! Y start of right arrow
     :    YARRL,                ! Y centre of left arrow
     :    RADCIR,               ! Radius of circle enclosing an arrow
     :    BOXHT                 ! Box height
      PARAMETER( XINDEN = 0.15 )
      PARAMETER( YOFFS = 0.11 )
      PARAMETER( ARROWL = 0.06667 )
      PARAMETER( ARROW = 0.01667 )
      PARAMETER( TXHT = 0.021 )
      PARAMETER( ARTXHT = TXHT )
      PARAMETER( XARR = 0.21667 )
      PARAMETER( YARR = 0.02667 )
      PARAMETER( YARRL = 0.06 )
      PARAMETER( XARRR = 0.5833 )
      PARAMETER( RADCIR = 0.55 * ARROWL )
      PARAMETER( BOXHT = 0.1 )


*  Local Variables:
*        Stored input set up data:

      INTEGER
     :    FONT,                 ! Font number
     :    IZONE,                ! Zone identifier
     :    OLDPEN,               ! Pen number
     :    TXTPRC                ! Text precision

      CHARACTER*2 TXJ           ! Input text justification code

      REAL
     :    ASPRAT,               ! Aspect ratio
     :    HTCHR,                ! Character height
     :    TXTSPC,               ! Text spacing
     :    XU,                   ! X direction cosine of text orientation
     :    YU                    ! Y direction cosine of text orientation

*        Now the remainder:

      INTEGER
     :    SZONE,                ! Identifier of square zone
     :    WKID                  ! Workstation identifier

      REAL
     :    BOXTOP,               ! Y co-ordinate of the top of the box
     :    DELTX,                ! Space in x between boxes
     :    ENDAR,                ! End of an arrow
     :    YOFF                  ! Y offset of box

*.


*    If the status value is bad, then return to the calling program.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Save parameters defining the text and co-ordinates.

      CALL SGS_ICURZ( IZONE )
      CALL SGS_ITXA( FONT, TXTPRC, HTCHR, ASPRAT, XU, YU, TXTSPC, TXJ )
      CALL SGS_IPEN( OLDPEN )

*    Clear the current work station.

      CALL SGS_ICURW( WKID )
      CALL GCLRWK( WKID, 0 )

*    Define square zone

      CALL SGS_ZSHAP( 1.0, 'BC', SZONE, STATUS )

*    If the trackerball is not to be used then put the boxes at the
*    bottom of the screen, otherwise leave space for the arrows.

      CALL SGS_SW( 0.0, 1.0, 0.0, 1.0, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_BTTN3_ZONE',
     :     'BTTN3: Error while defining the shape or co-ordinates '/
     :     /'of the graphics zone.',
     :     STATUS )

         GOTO 999
      END IF

      IF ( USECUR ) THEN
         YOFF = YOFFS
      ELSE
         YOFF = 0.0
      END IF

*    Draw the boxes for the button labels in blue.
*
*    First the perimeter is drawn.

      CALL SGS_SPEN( BLUE )

      BOXTOP = YOFF + BOXHT
      CALL SGS_BOX( XINDEN, 1.0-XINDEN, YOFF, BOXTOP )

*    Draw in vertical bars to divide up the large box into three small
*    ones.

      DELTX = ( 1.0 - 2.0 * XINDEN ) / 3.
      CALL SGS_LINE( XINDEN + DELTX, YOFF, XINDEN + DELTX, BOXTOP )
      CALL SGS_LINE( XINDEN + 2 * DELTX, YOFF, XINDEN + 2 * DELTX,
     :               BOXTOP )

      CALL SGS_FLUSH

*    Set up the text parameters for the labels in the boxes.
*
*    Set the text height

      CALL SGS_SHTX( TXHT )

*    Set the aspect ratio to 5 to 1.

      CALL SGS_SARTX( 0.6 )

*    Set the text justification to the centre of the string.

      CALL SGS_STXJ( 'CC' )

*    Write the labels into the boxes.
*
*    Write the first button label in green (for historical reason).

      CALL SGS_SPEN( GREEN )

      CALL SGS_BTEXT( XINDEN + 0.5 * DELTX, YOFF + 0.5 * BOXHT )
      CALL SGS_ATXL( LB1( 1:MIN( MAXLEN, LEN(LB1) ) ) )

      CALL SGS_FLUSH

*    Write the second button label in white (for historical reason).

      CALL SGS_SPEN( WHITE )

      CALL SGS_BTEXT( XINDEN + 1.5 * DELTX, YOFF + 0.5 * BOXHT )
      CALL SGS_ATXL( LB2( 1:MIN( MAXLEN, LEN(LB2) ) ) )

*    Write the escape button label in red (for historical reason).

      CALL SGS_SPEN( RED )
      CALL SGS_BTEXT( XINDEN + 2.5 * DELTX, YOFF + 0.5 * BOXHT )
      CALL SGS_ATXL( LBE( 1:MIN( MAXLEN, LEN(LBE) ) ) )

      CALL SGS_FLUSH

*    If the trackerball is to be used then draw in the arrows and
*    their labels.
*    If a label is '*' then the arrow and the label are not drawn.

      IF ( USECUR ) THEN

*       Draw the arrows with circles around them.
*
*       Draw the horizontal arrow in yellow

         CALL SGS_SPEN( YELLOW )

         IF ( LTX .NE. '*' ) THEN
            ENDAR = XARR + ARROWL

            CALL SGS_LINE( XARR, YARRL, ENDAR, YARRL )
            CALL SGS_LINE( ENDAR, YARRL, ENDAR - ARROW, YARRL + ARROW )
            CALL SGS_LINE( ENDAR, YARRL, ENDAR - ARROW, YARRL - ARROW )
            CALL SGS_LINE( XARR, YARRL, XARR + ARROW, YARRL + ARROW )
            CALL SGS_LINE( XARR, YARRL, XARR + ARROW, YARRL - ARROW )

*          Draw a circle around the horizontal arrow.

            CALL SGS_CIRCL( XARR + 0.5 * ARROWL, YARRL, RADCIR )

         END IF

*       Draw the vertical arrow.

         IF ( LTY .NE. '*' ) THEN
            ENDAR = YARR + ARROWL

            CALL SGS_LINE( XARRR, YARR, XARRR, ENDAR )
            CALL SGS_LINE( XARRR, ENDAR, XARRR - ARROW, ENDAR - ARROW )
            CALL SGS_LINE( XARRR, ENDAR, XARRR + ARROW, ENDAR - ARROW )
            CALL SGS_LINE( XARRR, YARR, XARRR - ARROW, YARR + ARROW )
            CALL SGS_LINE( XARRR, YARR, XARRR + ARROW, YARR + ARROW )

*          Draw the circle around the vertical arrow.

            CALL SGS_CIRCL( XARRR, YARR + 0.5 * ARROWL, RADCIR )

         END IF

         CALL SGS_FLUSH


*       Set the text attributes for the labels.
*
*       Set the character height to 0.4 world co-ordinate units.

         CALL SGS_SHTX( ARTXHT )


*       Set the aspect ratio

         CALL SGS_SARTX( 0.7 )


*       Set the text justification to bottom left.

         CALL SGS_STXJ( 'BL' )


*       Set the pen to white.

         CALL SGS_SPEN( WHITE )


*       Write the first label.

         IF ( LTX .NE. '*' ) THEN

            CALL SGS_BTEXT( XARR + RADCIR + 0.05, YARRL - 0.01 )
            CALL SGS_ATXL( LTX( 1:MIN( MAXLEN, LEN(LTX) ) ) )

         END IF


*       Write the second label.

         IF ( LTY .NE. '*' ) THEN

            CALL SGS_BTEXT( XARRR + 0.5 * RADCIR + 0.05, YARR +
     :                      0.5 * ARROWL - 0.01 )
            CALL SGS_ATXL( LTY( 1:MIN( MAXLEN, LEN(LTY) ) ) )

         END IF

         CALL SGS_FLUSH
      END IF

*    Reset all the attributes to what they were on entry to the
*    subroutine.
*
*    Reset the zone.

      CALL SGS_SELZ( IZONE,  STATUS )

*    Inquire whether GKS/SGS has reported an error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_BTTN3_RZONE',
     :     'BTTN3: Error while resetting the zone of the graphics '/
     :     /'device.', STATUS )

      END IF

*    Reset the character height.

      CALL SGS_SHTX( HTCHR )

*    Reset the aspect ratio.

      CALL SGS_SARTX( ASPRAT )

*    Reset the text justification

      CALL SGS_STXJ( TXJ )

*    Reset the pen.

      CALL SGS_SPEN( OLDPEN )

 999  CONTINUE

      END

