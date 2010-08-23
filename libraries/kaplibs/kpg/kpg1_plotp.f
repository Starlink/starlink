      SUBROUTINE KPG1_PLOTP( IPICD, APP, MARGIN, NP, PNAME, PSIDE,
     :                       PSIZE, ASPECT, BOX, IPICD0, IPICF, IPIC,
     :                       STATUS )
*+
*  Name:
*     KPG1_PLOTP

*  Purpose:
*     Creates a new DATA picture, with optionally ancillary pictures.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PLOTP( IPICD, APP, MARGIN, NP, PNAME, PSIDE, PSIZE,
*                      ASPECT, BOX, IPICD0, IPICF, IPIC, STATUS )

*  Description:
*     This routine createa a new DATA picture, together with optional
*     ancillary pictures around the DATA picture. The new DATA picture
*     can be aligned with an existing DATA picture. A FRAME picture
*     enclosing the DATA picture and any ancillary pictures is created
*     if any ancillary pictures or non-zero margins were requested.
*
*     On exit, the current PGPLOT viewport corresponds to area occupied
*     by the new DATA picture. The bounds of the PGPLOT window produce a
*     world co-ordinate system within the viewport corresponding to
*     millimetres from the bottom-left corner of the view surface. Note,
*     this is different to the world co-ordinate system stored in the AGI
*     database with the new DATA picture.
*
*     Various environment parameters are used to obtain options, etc. The
*     names of these parameters are hard-wired into this subroutine in
*     order to ensure conformity between applications.

*  Environment Parameters:
*     FILL = _LOGICAL (Read)
*        TRUE if the supplied aspect ratio is to be ignored, creating the
*        largest possible DATA picture within the current picture.  When
*        FILL is FALSE, the DATA picture is created with the supplied
*        aspect ratio. Only used when creating a new DATA picture.

*  Arguments:
*     IPICD = INTEGER (Given)
*        The AGI identifier for an existing DATA picture. If this is
*        supplied equal to -1, the size and extent of the new DATA picture
*        are determined by BOX. Otherwise, the size and extent of the new
*        DATA picture are set equal to the existing DATA picture.
*     APP = CHARACTER * ( * ) (Given)
*        The name of the calling application in the form
*        <package>_<application> (eg"KAPPA_DISPLAY").
*     MARGIN( 4 ) = REAL (Given)
*        The width of the borders to leave round the new DATA picture, given
*        as fractions of the corresponding dimension of the DATA picture.
*        These should be supplied in the order bottom, right, top, left.
*     NP = INTEGER (Given)
*        The number of extra pictures to be included in the FRAME pictures
*        (the DATA picture itself is not included in this list). Margins are
*        left round the DATA picture with widths given by MARGIN. Any extra
*        pictures are placed outside these margins, in positions described by
*        PSIDE and PSIZE.
*     PNAME( NP ) = CHARACTER * ( * ) (Given)
*        The names to store in the AGI database with the NP extra pictures.
*     PSIDE( NP ) = CHARACTER * 1 (Given)
*        Each element of this array should be one of L, R, T or B. It
*        indicates which side of the FRAME picture an extra picture is to be
*        placed. For Left and Right, the extra picture occupies the full
*        height of the DATA picture, margins, and any previously created
*        extra pictures. The picture is placed at the far Left or Right of
*        all previously created pictures. For Top or Bottom, the extra picture
*        occupies the full width of the DATA picture, margins, and any
*        previously created extra pictures. The picture is placed at the top or
*        bottom of all previously created pictures. Ignored if NP is zero.
*     PSIZE( NP ) = REAL (Given)
*        The size of each extra picture. For Left and Right pictures, this is
*        the width of the picture, and the value is given as a fraction
*        of the width of the DATA picture. For Top and Bottom pictures, it is
*        the height of the picture, and it is given as a fraction of the
*        height of the DATA picture. Ignored if NP is zero.
*     ASPECT = REAL (Given)
*        The aspect ratio for the DATA picture. This is the height of the
*        DATA picture (in millimetres)  divided by the width of the DATA
*        picture (also in millimetres). The new DATA picture is created with
*        this aspect ratio unless the FILL parameter is given a TRUE value,
*        in which case the aspect ratio is adjusted to get the largest DATA
*        picture which can be created within the current picture. If a value
*        of zero is supplied, then the largest DATA picture is used
*        irrespective of FILL (which is then not accessed).
*     BOX( 4 ) = DOUBLE PRECISION (Given)
*        The co-ordinates to be assigned to the bottom-left, and top-right
*        corners of the DATA picture in the AGI database (the co-ordinate
*        system in defined by argument DOMAIN). Only used if the new DATA
*        picture is NOT being aligned with an existing DATA picture. Supplied
*        in the order XLEFT, YBOTTOM, XRIGHT, YTOP. Note, the supplied
*        bounds are stored in the AGI database, but do not effect the PGPLOT
*        window on exit, which always has a world co-ordinate system of
*        millimetres from the bottom-left corner of the view surface. If
*        the supplied box has zero area, then world co-ordinates for the
*        DATA picture in the AGI database will be centimetres from the
*        bottom-left corner of the DATA picture.
*     IPICD0 = INTEGER (Returned)
*        An AGI identifier for the new DATA picture.
*     IPICF = INTEGER (Returned)
*        An AGI identifier for the new FRAME picture. World co-ordinate system
*        is inherited from the current picture on entry. If no FRAME picture
*        is created then an identifier for the current picture on entry is
*        returned.
*     IPIC( NP ) = INTEGER (Returned)
*        An array of AGI identifiers corresponding to the extra pictures
*        requested in ZSIDE and PSIZE. The world co-ordinate system for each
*        picture is inherited from the FRAME picture. The actual size of a
*        picture may be less than the requested size if there is insufficient
*        room left in the FRAME picture to give it its requested size.
*        Identifiers for pictures which would have zero size (i.e. fall
*        completely outside the FRAME picture) are returned equal to -1, but
*        no error is reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Picture identifiers are returned equal to -1 if the picture cannot
*     be created (egdue to lack of room within the current picture).

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-SEP-1998 (DSB):
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
      INTEGER IPICD
      CHARACTER APP*(*)
      REAL MARGIN( 4 )
      INTEGER NP
      CHARACTER PNAME( NP )*(*)
      CHARACTER PSIDE( NP )*(*)
      REAL PSIZE( NP )
      REAL ASPECT
      DOUBLE PRECISION BOX( 4 )

*  Arguments Returned:
      INTEGER IPICD0
      INTEGER IPICF
      INTEGER IPIC( NP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      REAL DX1                   ! X coord at left of window
      REAL DX2                   ! X coord at right of window
      REAL DY1                   ! Y coord at bottom of window
      REAL DY2                   ! Y coord at top of window
      REAL X1                    ! X NDC coord at left of viewport
      REAL X2                    ! X NDC coord at right of viewport
      REAL Y1                    ! Y NDC coord at bottom of viewport
      REAL Y2                    ! Y NDC coord at top of viewport
*.

*  Initialise returned values.
      IPICF = -1
      IPICD0 = -1

      DO I = 1, NP
         IPIC( I ) = -1
      END DO

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First deal with cases where the new DATA picture is to be aligned with
*  an existing DATA picture.
      IF( IPICD .NE. -1 ) THEN

*  Create the required AGI pictures (DATA, FRAME and any requested
*  ancillary pictures), aligning the new DATA picture with an existing
*  DATA picture. On exit, the PGPLOT viewport matches the DATA picture,
*  and has the same (AGI) world co-ordinates.
         CALL KPG1_GDOLD( MARGIN, APP, NP, PNAME, PSIDE, PSIZE,
     :                    IPICD, IPICD0, IPICF, IPIC, STATUS )

*  If the new DATA picture is not to be aligned with an existing DATA
*  picture...
      ELSE

*  Create the required AGI pictures (DATA, FRAME and any requested
*  ancillary pictures). The DATA picture has the AGI world co-ordinate
*  bounds specified by BOX. On exit, the PGPLOT viewport matches the DATA
*  picture, and has the same (AGI) world co-ordinates.
         CALL KPG1_GDNEW( APP, MARGIN, NP, PNAME, PSIDE, PSIZE,
     :                    ASPECT, BOX, IPICD0, IPICF, IPIC, STATUS )

      END IF

*  Skip the rest if an error has occurred.
      IF( STATUS .EQ. SAI__OK ) THEN

*  Set the PGPLOT world co-ordinate system so that it corresponds to
*  millimetres from the bottom-left corner of the view surface.
         CALL PGQVP( 2, DX1, DX2, DY1, DY2 )
         CALL PGSWIN( DX1, DX2, DY1, DY2 )

*  Set the PGPLOT character size so that small text will be used in small
*  pictures by default. This can be overridden by the user setting
*  values for the AST "Size" attributes. Base the text size on the size
*  of the FRAME picture...

*  Make the FRAME Picture the current picture and set up the corresponding
*  PGPLOT viewport.
         CALL AGI_SELP( IPICF, STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )

*  Scale the PGPLOT character height by a square root of the minimum dimension
*  (in Normalised Device co-ordinates) of the FRAME picture. The square
*  root is used so that small pictures do not have unreadably small text.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL PGQVP( 0, X1, X2, Y1, Y2 )
            CALL PGSCH( 1.3*SQRT( MIN( X2 - X1, Y2 - Y1 ) ) )
         END IF

*  Re-instate the DATA picture, viewport, and window.
         CALL AGI_SELP( IPICD0, STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )
         IF( STATUS .EQ. SAI__OK ) CALL PGSWIN( DX1, DX2, DY1, DY2 )

      END IF

      END
