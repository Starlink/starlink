      SUBROUTINE CCD1_DRAWI( ID, MEMID, XC, YC, SPAN, THICK, ICOL,
     :                       STATUS )
*+
*  Name:
*     CCD1_DRAWI

*  Purpose:
*     Draws an IDI cross.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DRAWI( ID, MEMID, XC, YC, SPAN, THICK, ICOL, STATUS )

*  Description:
*     Draws a cross on an IDI device. The cross is centred on the
*     position XC,YC. Each line of the cross is twice times the SPAN
*     value long. The cross is drawn with THICK lines offset by a
*     device pixel from each other and with the IDI colour ICOL.

*  Arguments:
*     ID = INTEGER (Given)
*        The IDI display identifier
*     MEMID = INTEGER (Given)
*        The IDI device memory plane identifier,
*     XC = INTEGER (Given)
*        The X centre of the cross (device pixels).
*     YC = INTEGER (Given)
*        The Y centre of the cross (device pixels).
*     SPAN = INTEGER (Given)
*        The number of pixels along a span (centre to end) of the cross.
*     THICK = INTEGER (Given)
*        The number of lines drawn one pixel apart to mark each line of
*        the cross.
*     ICOL = INTEGER (Given)
*        The IDI colour index to use when drawing the the lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'IDI_ERR'          ! IDI error constants

*  Arguments Given:
      INTEGER ID
      INTEGER MEMID
      INTEGER XC
      INTEGER YC
      INTEGER SPAN
      INTEGER THICK
      INTEGER ICOL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER XEND( 2 )          ! X end points of lines
      INTEGER YEND( 2 )          ! Y end points of lines
      INTEGER FIRST              ! First offset to draw thick line
      INTEGER LAST               ! Last offset to draw thick line

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( THICK .EQ. 1 ) THEN 
         XEND( 1 ) = XC - SPAN
         XEND( 2 ) = XC + SPAN
         YEND( 1 ) = YC
         YEND( 2 ) = YC
         CALL IIGPLY( ID, MEMID, XEND, YEND, 2, ICOL, 1, STATUS )
         XEND( 1 ) = XC
         XEND( 2 ) = XC
         YEND( 1 ) = YC - SPAN
         YEND( 2 ) = YC + SPAN
         CALL IIGPLY( ID, MEMID, XEND, YEND, 2, ICOL, 1, STATUS )
      ELSE

*  Set the range of lines to draw
         FIRST = - THICK / 2
         LAST = THICK + FIRST - 1

*  Draw the required number of lines.
         DO 2 I = FIRST, LAST, 1
            XEND( 1 ) = XC - SPAN
            XEND( 2 ) = XC + SPAN
            YEND( 1 ) = YC + I 
            YEND( 2 ) = YC + I
            CALL IIGPLY( ID, MEMID, XEND, YEND, 2, ICOL, 1, STATUS )
            XEND( 1 ) = XC + I
            XEND( 2 ) = XC + I
            YEND( 1 ) = YC - SPAN
            YEND( 2 ) = YC + SPAN
            CALL IIGPLY( ID, MEMID, XEND, YEND, 2, ICOL, 1, STATUS )
 2       CONTINUE
      END IF
      END
* $Id$
