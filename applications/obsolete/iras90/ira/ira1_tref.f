      SUBROUTINE IRA1_TREF( TEXT, TXJ, H, UX, UY, MARGIN, WKID, REFX,
     :                      REFY, STATUS )
*+
*  Name:
*     IRA1_TREF

*  Purpose:
*     Find the reference point at which to display text.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_TREF( TEXT, TXJ, H, UX, UY, MARGIN, WKID, REFX, REFY,
*                     STATUS )

*  Description:
*      IRA1_IDRVA displays text using CL justification, but the users
*      supplied reference position may refer to some other
*      justification. This routine modifies the supplied reference
*      position to make it refer to the centre left point of the box
*      encompassing the displayed text. Non-vertical up vectors (as set
*      by SGS_SUPTX) and non-constant character sizes are taken account
*      of.
*
*      The extent of the area covered by the text is written to common
*      for processing by IRA_DRVPO.

*  Arguments:
*     TEXT = CHARACTER * ( * ) (Given)
*        The text being displayed.
*     TXJ = CHARACTER * ( * ) (Given)
*        The text justification required by the user.
*     H = REAL (Given)
*        Character height, including the margins added by IRA1_IDRVA.
*     UX = REAL (Given)
*        The X component of the unit up vector.
*     UY = REAL (Given)
*        The Y component of the unit up vector.
*     MARGIN = REAL (Given)
*        The width of the blank margin to be left around the edges of
*        the text string.
*     WKID = INTEGER (Given)
*        The current GKS workstation identifier.
*     REFX = REAL (Given and Returned)
*        The refernce X position. On entry, it refers to the position
*        within the text string specified by the justification stored in
*        TXJ. The supplied reference position is a point on the box
*        which includes both the text and the margin specified by
*        MARGIN. On exit it refers to the centre left point of the text
*        string (excluding the margin).
*     REFY = REAL (Given and Returned)
*        The reference Y position.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'IRA_PAR'          ! IRA constants

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_DRVPO( 5 ) = REAL (Write)
*           Values defining the area occupied by the text, in the
*           order; (X,Y) at start of box, (X,Y) at end of box, height
*           of box (perpendicular to the line joing start and end of
*           box).

*  Arguments Given:
      CHARACTER TEXT*(*)
      CHARACTER TXJ*2
      REAL H
      REAL UX
      REAL UY
      REAL MARGIN
      INTEGER WKID

*  Arguments Given and Returned:
      REAL REFX
      REAL REFY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      REAL    CPX                ! X coordinate of the end of the
                                 ! plotted text string.
      REAL    CPY                ! Y coordinate of the end of the
                                 ! plotted text string.
      INTEGER ERRIND             ! GKS error status.
      REAL    L                  ! Length of plotted text.
      REAL    TXEXPX( 4 )        ! X coordinates of corners of box which
                                 ! encloses the printed text.
      REAL    TXEXPY( 4 )        ! Y coordinates of corners of box which
                                 ! encloses the printed text.
      REAL    XHIGH              ! Highest x position covered by text.
      REAL    XLOW               ! Lowest x position covered by text.
      REAL    YHIGH              ! Highest y position covered by text.
      REAL    YLOW               ! Lowest y position covered by text.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the extent in world coordinates of the character string to be
*  plotted.
      CALL GQTXX( WKID, REFX, REFY, TEXT, ERRIND, CPX, CPY, TXEXPX,
     :            TXEXPY )
      IF( ERRIND .NE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'ERRIND', ERRIND )
         CALL ERR_REP( 'IRA1_TREF_ERR1',
     : 'IRA1_TREF: GKS error no. ^ERRIND occurred while calling GQTXX.',
     :                       STATUS )
         GO TO 999
      END IF

*  Calculate the length of the string, including a margin at the start
*  and end.
      L = SQRT( ( CPX - REFX )**2 + (CPY - REFY )**2 ) + 2*MARGIN

*  The text will be printed with CL justification.  Modify the supplied
*  reference position to make the text appear as if it was printed with
*  the current SGS text justification. The unit up vector is (UX,UY).
*  The unit vector from the end to the start (right to left) is
*  (-UY,UX).
      IF( TXJ( 1:1 ) .EQ. 'T' ) THEN
         REFX = REFX - 0.5*H*UX
         REFY = REFY - 0.5*H*UY

      ELSE IF( TXJ( 1:1 ) .EQ. 'B' ) THEN
         REFX = REFX + 0.5*H*UX
         REFY = REFY + 0.5*H*UY

      END IF

      IF( TXJ( 2:2 ) .EQ. 'C' ) THEN
         REFX = REFX - 0.5*L*UY
         REFY = REFY + 0.5*L*UX

      ELSE IF( TXJ( 2:2 ) .EQ. 'R' ) THEN
         REFX = REFX - L*UY
         REFY = REFY + L*UX

      END IF

*  Store the coordinates of the start and end of the line joining the
*  CL and CR positions. Also store the current text height.
      ACM_DRVPO( 1 ) = REFX
      ACM_DRVPO( 2 ) = REFY
      ACM_DRVPO( 3 ) = REFX + UY*L
      ACM_DRVPO( 4 ) = REFY - UX*L
      ACM_DRVPO( 5 ) = H

*  Move the reference position to the right by the size of the margin.
      REFX = REFX + MARGIN*UY
      REFY = REFY - MARGIN*UX

*  Find the limits of the box which contains the text, excluding the
*  margin.
      TXEXPX( 1 ) = REFX + UX*( 0.49*H - MARGIN )
      TXEXPY( 1 ) = REFY + UY*( 0.49*H - MARGIN )
      TXEXPX( 2 ) = REFX - UX*( 0.49*H - MARGIN )
      TXEXPY( 2 ) = REFY - UY*( 0.49*H - MARGIN )
      TXEXPX( 3 ) = TXEXPX( 1 )  + UY*( 0.85*L - MARGIN )
      TXEXPY( 3 ) = TXEXPY( 1 )  - UX*( 0.85*L - MARGIN )
      TXEXPX( 4 ) = TXEXPX( 2 )  + UY*( 0.85*L - MARGIN )
      TXEXPY( 4 ) = TXEXPY( 2 )  - UX*( 0.85*L - MARGIN )

      XLOW = MIN( TXEXPX( 1 ), TXEXPX( 2 ), TXEXPX( 3 ), TXEXPX( 4 ) )
      XHIGH = MAX( TXEXPX( 1 ), TXEXPX( 2 ), TXEXPX( 3 ), TXEXPX( 4 ) )
      YLOW = MIN( TXEXPY( 1 ), TXEXPY( 2 ), TXEXPY( 3 ), TXEXPY( 4 ) )
      YHIGH = MAX( TXEXPY( 1 ), TXEXPY( 2 ), TXEXPY( 3 ), TXEXPY( 4 ) )

*  Clear the box containing the text.
      CALL SGS_CLRBL( XLOW, XHIGH, YLOW, YHIGH )

 999  CONTINUE

      END
