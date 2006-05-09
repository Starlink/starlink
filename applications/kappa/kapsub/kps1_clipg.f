      SUBROUTINE KPS1_CLIPG( PICID, XPIC, YPIC, IPIC, JPIC, MARGIN,
     :                       BOX, START, COMENT, MAKFRM, OUTLIN, PICIDE,
     :                       PICIDF, STATUS )
*+
*  Name:
*     KPS1_CLIPG

*  Purpose:
*     Creates an AGI DATA picture within a grid of pictures for
*     CLINPLOT.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLIPG( PICID, XPIC, YPIC, IPIC, JPIC, MARGIN, BOX,
*                      START, COMENT, MAKFRM, OUTLIN, PICIDE, PICIDF,
*                      STATUS )

*  Description:
*     This routine creates a DATA picture within a grid of pictures for
*     task CLINPLOT within the current picture, which should be the
*     spatial DATA picture, leaving a margin (see argument MARGIN) 
*     around the grid.  Each plot is labelled CLINPLOT_<number>, where
*     <number> is START for the lower-left picture, and increments by
*     one for each picture, the horizontal axis increasing faster.
*     The comment for the DATA picture is supplied through argument
*     COMENT.  An optional outline may be drawn (see argument OUTLIN).
*
*     If argument MAKFRM is TRUE, then a FRAME picture with a comment
*     set to "REFERENCE" is created first.  It spans from just inside 
*     the lower-left corner of the input picture and just beyond the
*     upper-right corner of the DATA picture, unless YPIC is 1, where it
*     extends almost to the top-right of the input picture.  The goal is
*     to avoid overprinting of other pictures by a title or top and 
*     right axis annotations.

*  Arguments:
*     PICID = INTEGER (Given)
*        The AGI picture identifier in which the grid of frames are to
*        be placed.  It is reset to be current on exit.
*     XPIC = _INTEGER (Read)
*        The number of new pictures placed horizontally within the 
*        current picture comprising the grid.  The total number of new
*        pictures in the grid is XPIC * YPIC.
*     YPIC = _INTEGER (Read)
*        The number of new pictures placed vertically in the
*        current picture comprising the grid.  
*     IPIC = _INTEGER (Read)
*        The horizontal index of the new picture.
*     JPIC = _INTEGER (Read)
*        The vertical index of the new picture.
*     MARGIN( 4 ) = REAL (Given)
*        The widths of the margins to leave for axis annotation around
*        the line plots, given as fractions of a single plot in the
*        grid.  The four values are in the order: bottom, right, top,
*        left.
*     BOX( 4 ) = DOUBLE PRECISION (Given)
*        The world co-ordinate bounds to give to the DATA picture.
*        These should normally be pixel co-ordinates.  The (x,y) 
*        co-ordinates of the bottom-left corner should be given in 
*        elements 1 and 2, and the (x,y) co-ordinates of the top-right
*        corner should be given in elements 3 and 4.  If the box has 
*        zero area, then world co-ordinates are set to millimetres from 
*        the bottom-left corner of the DATA picture.
*     START = INTEGER (Given)
*        Label number of the bottom-left picture.  Numbers increase
*        in Fortran order.
*     COMENT = CHARACTER * ( * ) (Given)
*        The comment to store with the DATA picture in the AGI database.
*     MAKFRM = LOGICAL (Given)
*        If TRUE, it requests that a FRAME picture enclosing the
*        DATA with a small additional trim to the top and right (to
*        ensure enclosure), and inclusing the bottom and left margins
*        at its lower bound.  The FRAME picture is constrained to
*        lie entirely within the input picture PICID.
*     OUTLIN = LOGICAL (Given)
*        Draw an outline
*     PICIDE = INTEGER (Returned)
*        The AGI picture identifier of the created DATA picture located
*        at (IPIC, JPIC) within the grid.  It is the current picture
*        on exit.
*     PICIDF = INTEGER (Returned)
*        The AGI picture identifier of the created FRAME picture when
*        MAKFRM is TRUE.  Otherwise -1 is returned.
*     STATUS = INTEGER (Given and Returned)
*        Global status value.

*  Prior Requirements:
*     -  The PGPLOT interface to the AGI library should be opened before
*     calling this routine.  

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics & Astronomy Research
*     Council. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2005 November 25 (MJC):
*        Original version.
*     2005 December 7 (MJC):
*        Added PICARR argument.
*     2005 December 12 (MJC):
*        Redesigned to return the picture identifier for only one
*        specified (via IPIC and JPIC arguments) element.  Add COMENT
*        argument.
*     2005 December 14 (MJC):
*        Merged functionality of KPS1_CLIPR, i.e. making the FRAME
*        picture.
*     2005 December 16 (MJC):
*        Added OUTLIN argument.
*     2005 December 20 (MJC):
*        Remove the XPIC=1 and YPIC=1 constraint on making the FRAME.
*        Set the bounds for the FRAME around the current DATA picture,
*        and not assume that the DATA picture is at the lower-left of
*        the grid.
*     2006 April 12 (MJC):
*        Remove unused variable.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER PICID
      INTEGER XPIC
      INTEGER YPIC
      INTEGER IPIC
      INTEGER JPIC
      REAL MARGIN( 4 )
      DOUBLE PRECISION BOX( 4 )
      INTEGER START
      CHARACTER*( * ) COMENT
      LOGICAL MAKFRM
      LOGICAL OUTLIN

*  Arguments Returned:
      INTEGER PICIDE
      INTEGER PICIDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ITOMM                 ! Factor for converting inches to 
      PARAMETER ( ITOMM = 25.4 ) ! millimetres

      REAL TRIM                  ! Viewport trim to avoid new pictures
      PARAMETER( TRIM = 0.001 )  ! extending beyond the current picture

*  Local Variables:
      CHARACTER CLBNUM*( 9 )     ! Label number
      REAL EXPIC                 ! Equivalent number of pictures in x
      REAL EYPIC                 ! Equivalent number of pictures in x
      CHARACTER LABEL*( DAT__SZNAM ) ! AGI picture label
      INTEGER LABNUM             ! Label number
      INTEGER NCLBNO             ! No. of characters in label number
      REAL SIZE( 2 )             ! Sizes of each picture array element
      REAL X1, X2                ! X bounds of the new picture in world
                                 ! co-ordinates
      REAL X1E, X2E              ! X bounds of the old picture
      REAL X1F, Y1F              ! Lower bounds of the FRAME picture
      REAL X2F, Y2F              ! Upper bounds of the FRAME picture
      REAL XORIG                 ! X origin of first picture
      REAL XR                    ! X extent of the old picture
      REAL XUPP                  ! X upper limit pictures
      REAL Y1, Y2                ! Y bounds of the new picture in world
                                 ! co-ordinates
      REAL Y1E, Y2E              ! Y bounds of the old picture
      REAL YORIG                 ! Y origin of first picture
      REAL YR                    ! Y extent of the old picture
      REAL YUPP                  ! Y upper limit pictures

*.

      PICIDE = -1
      PICIDF = -1

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure the current PGPLOT viewport matches the current AGI picture.
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds of this viewport, in inches from the bottom-left 
*  corner of the screen.  Save the size in inches.
      CALL PGQVP( 1, X1E, X2E, Y1E, Y2E )

*  Get its bounds in world co-ordinates.
*     CALL PGQWIN( X1E, X2E, Y1E, Y2E )

*  Derive the dimension of the current picture in world co-ordinates.
      XR = X2E - X1E
      YR = Y2E - Y1E

*  The margins are measured in terms of pictures.  Find how many
*  equivalent pictures are to subtend the current viewport.  Now
*  we use the full margins.
      EXPIC = REAL( XPIC ) + MARGIN( 2 ) + MARGIN( 4 )
      EYPIC = REAL( YPIC ) + MARGIN( 1 ) + MARGIN( 3 )

*  Calculate the dimensions of each new picture.
      SIZE( 1 ) = XR / EXPIC
      SIZE( 2 ) = YR / EYPIC

*  Set the origin of the lower-left picture.
      XORIG = X1E + XR * MARGIN( 4 ) / EXPIC
      YORIG = Y1E + YR * MARGIN( 1 ) / EYPIC

*  Set the upper limits.
      XUPP = X2E - XR * MARGIN( 2 ) / EXPIC
      YUPP = Y2E - YR * MARGIN( 3 ) / EYPIC

*  Set the label numbering.
      LABNUM = START + IPIC - 1 + ( JPIC - 1 ) * XPIC

*  Define the vertical bounds of the new DATA picture, ensuring that
*  the created picture does not lie outside the bounds of the
*  current picture.
      Y1 = MAX( YORIG, YORIG + YR * REAL( JPIC - 1 ) / REAL( EYPIC ) )
      Y2 = MIN( YUPP, Y1 + SIZE( 2 ) )

*  Define the horizontal bounds of the new DATA picture, ensuring that
*  the created picture does not lie outside the bounds of the
*  current picture.
      X1 = MAX( XORIG, XORIG + XR * REAL( IPIC - 1 ) / REAL( EXPIC ) )
      X2 = MIN( XUPP, X1 + SIZE( 1 ) )

*  Should we make a FRAME picture?  MAKFRM requests that a FRAME should
*  be created if the DATA picture is the lower-left in the grid.
      IF ( MAKFRM ) THEN

*  Define the bounds of the new FRAME picture, ensuring that the created
*  picture does not lie outside the bounds of the current picture.  Note
*  that the lower-left picture may may not be visible and another 
*  picture will have the FRAME for annotations around it.  Hence these 
*  calculations work from that DATA picture rather than assuming that
*  the lower-left bound of the input picture with a trim defines the 
*  same bound of the FRAME.
         X1F = MAX( X1E + YR * TRIM, X1 - XR * MARGIN( 4 ) / EXPIC )
         Y1F = MAX( Y1E + YR * TRIM, Y1 - YR * MARGIN( 1 ) / EYPIC )
         X2F = MIN( XUPP, X2 + TRIM * XR )
         Y2F = MIN( YUPP, Y2 + TRIM * YR )

*  Create the new viewport.
         CALL PGVSIZ( X1F, X2F, Y1F, Y2F )

*  Set its world co-ordinate bounds.
         CALL KPG1_GDWIN( -1, STATUS )

*  Save the viewport as a new FRAME picture.  This is now the current
*  picture.
         CALL AGP_SVIEW( 'FRAME', 'REFERENCE', PICIDF, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETI( 'X', IPIC )
            CALL MSG_SETI( 'Y', JPIC )
            CALL ERR_REP( 'ERR_KPS1_CLIPG_DBSP',
     :        'CLINPLOT: Error while storing the new FRAME picture '/
     :        /'(^X,^Y) in the graphics database.', STATUS )
            CALL AGI_SELP( PICID, STATUS )
         END IF

      END IF

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Create a PGPLOT viewport for the DATA picture, and set its world-
*  co-ordinate bounds to the supplied values.  If the box defined by
*  the bounds has zero size, use millimetres from the bottom-left
*  corner.

*  Report an error if any of the BOX values are bad.
      IF ( BOX( 1 ) .EQ. AST__BAD .OR.
     :     BOX( 2 ) .EQ. AST__BAD .OR.
     :     BOX( 3 ) .EQ. AST__BAD .OR.
     :     BOX( 4 ) .EQ. AST__BAD ) THEN
         STATUS = SAI__OK
         CALL ERR_REP( 'KPS1_CLIPG_BOX', 'The bounds of the new '/
     :                 /'DATA picture are undefined in AGI world '/
     :                 /'co-ordinates.', STATUS )
         GO TO 999
      END IF

      CALL PGVSIZ( X1, X2, Y1, Y2 )
      IF ( BOX( 1 ) .NE. BOX( 3 ) .AND. BOX( 2 ) .NE. BOX( 4 ) ) THEN
         CALL PGSWIN( REAL( BOX( 1 ) ), REAL( BOX( 3 ) ), 
     :                REAL( BOX( 2 ) ), REAL( BOX( 4 ) ) )
      ELSE
         CALL PGSWIN( 0.0, ( X2 - X1 ) * ITOMM, 0.0, 
     :                ( Y2 - Y1 ) * ITOMM )
      END IF

*  Save the current viewport and the Plot as a new DATA picture within
*  the FRAME picture.
      CALL AGI_SELP( PICIDF, STATUS )
      CALL AGP_SVIEW( 'DATA', COMENT, PICIDE, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETI( 'X', IPIC )
         CALL MSG_SETI( 'Y', JPIC )
         CALL ERR_REP( 'ERR_KPS1_CLIPG_DBSP',
     :     'CLINPLOT: Error while storing the new DATA picture '/
     :     /'(^X,^Y) in the graphics database.', STATUS )
         CALL AGI_SELP( PICID, STATUS )
      END IF

* Draw a box around the picture.
      IF ( OUTLIN ) THEN
         CALL PGSFS( 2 )
         CALL PGRECT( REAL( BOX( 1 ) ), REAL( BOX( 3 ) ),
     :                REAL( BOX( 2 ) ), REAL( BOX( 4 ) ) )
      END IF

*  Derive the label from the prefix and element number.
      CALL CHR_ITOC( LABNUM, CLBNUM, NCLBNO )
      LABEL = 'CLINPLOT_'//CLBNUM( :NCLBNO )
      CALL AGI_SLAB( -1, LABEL, STATUS )

 999  CONTINUE

      END
