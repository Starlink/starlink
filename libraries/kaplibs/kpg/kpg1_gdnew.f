      SUBROUTINE KPG1_GDNEW( COMMNT, MARGIN, NP, PNAME, PSIDE,
     :                       PSIZE, SASPEC, BOX, IPICD, IPICF, IPIC,
     :                       STATUS )
*+
*  Name:
*     KPG1_GDNEW

*  Purpose:
*     Creates a new DATA picture with ancillary pictures.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDNEW( COMMNT, MARGIN, NP, PNAME, PSIDE, PSIZE,
*                      SASPEC, BOX, IPICD, IPICF, IPIC, STATUS )

*  Description:
*     This routine returns identifiers for a new DATA picture together
*     with various ancillary pictures in the graphics database. On exit,
*     the new DATA picture is the current AGI picture, and the PGPLOT
*     viewport matches the DATA picture.
*
*     A FRAME picture is only created if it would contain something other
*     than the DATA picture (this is assumed to be the case if any
*     ancillary pictures are requested, or if non-zero margins are requested
*     around the DATA picture). If created, the FRAME picture has the
*     maximum possible size. The DATA picture is then created with a size
*     which allows all the requested ancillary pictures to be created within
*     the FRAME picture.
*
*     Various environment parameters may be used to obtain options, etc. The
*     names of these parameters are hard-wired into this subroutine in
*     order to ensure conformity between application.

*  Environment Parameters:
*     FILL = _LOGICAL (Read)
*        TRUE if the supplied aspect ratio (SASPEC) is to be ignored,
*        creating the largest possible DATA picture within the current
*        picture.  When FILL is FALSE, the DATA picture is created with
*        the supplied aspect ratio. Not accessed if argument SASPEC is
*        supplied equal to zero (i.e. (SASPEC .EQ. 0.0) implies FILL=YES)

*  Arguments:
*     COMMENT = CHARACTER * ( * ) (Given)
*        A comment to store with the new pictures added to the AGI
*        database. This will usually by an indication of the application
*        being run (e.g. KAPPA_DISPLAY).
*     MARGIN( 4 ) = REAL (Given)
*        The width of the borders to leave round the DATA picture, given
*        as fractions of the corresponding dimension of the current picture.
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
*        Each element of this array should be one of L, R, T, B, or D. It
*        indicates which side of the FRAME picture an extra picture is to be
*        placed. For Left and Right, the extra picture occupies the full
*        height of the DATA picture, margins, and any previously created
*        extra pictures. The picture is placed at the far Left or Right of
*        all previously created pictures. For Top or Bottom, the extra picture
*        occupies the full width of the DATA picture, margins, and any
*        previously created extra pictures. The picture is placed at the top or
*        bottom of all previously created pictures.  D is a variant of
*        R, where the plot is to the right, but the vertical extent is
*        that of the DATA picture instead of the FRAME picture. This
*        argument is ignored if NP is zero.
*     PSIZE( NP ) = REAL (Given)
*        The size of each extra picture. For Left and Right pictures, this
*        is the width of the picture, and the value is given as a fraction
*        of the width of the current picture. For Top and Bottom pictures,
*        it is the height of the picture, and it is given as a fraction of
*        the height of the current picture. Ignored if NP is zero.
*     SASPEC = REAL (Given)
*        The aspect ratio with which a new DATA picture should be created.
*        This is the height divided by the width of the DATA picture,
*        assuming equal scales on each axis (e.g. metres). A value of zero
*        causes the DATA picture to have the aspect ratio which produces
*        the largest picture. The actual value used will depend on the
*        value supplied for the FILL parameter (see above).
*     BOX( 4 ) = DOUBLE PRECISION (Given)
*        The world co-ordinate bounds to give to the DATA picture if a new
*        DATA picture is created. These should normally be pixel
*        co-ordinates. The (x,y) co-ordinates of the bottom left corner should
*        be given in elements 1 and 2, and the (x,y) co-ordinates of the top
*        right corner should be given in elements 3 and 4. If the box has
*        zero area, then world co-ordinates are set to centimetres from the
*        bottom left corner of the DATA picture.
*     IPICD = INTEGER (Returned)
*        An AGI identifier for the DATA picture.
*     IPICF = INTEGER (Returned)
*        An AGI identifier for the FRAME picture. The world co-ordinate
*        system is inherited from the current picture on entry. If no
*        FRAME picture was created, then an AGI identifier for the current
*        picture is returned.
*     IPIC( NP ) = INTEGER (Returned)
*        An array of AGI identifiers corresponding to the extra pictures
*        requested in ZSIDE and PSIZE. The world co-ordinate system for each
*        picture is inherited from the FRAME picture. The actual size of a
*        picture may be less than the requested size if there is insufficient
*        room left in the FRAME picture to give it its requested size.
*        Identifiers for pictures which would have zero size (i.e. fall
*        completely outside the FRAME picture) are returned equal to -1,
*        but no error is reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Picture identifiers are returned equal to -1 if an error occurs,
*     or if the picture cannot be created.

*  Copyright:
*     Copyright (C) 1998, 1999, 2000 Central Laboratory of the Research Councils.
*     Copyright (C) 2006, 2020 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1998 (DSB):
*        Original version.
*     26-OCT-1999 (DSB):
*        Modified to make margin and picture sizes relative to the
*        original current picture rather than the new DATA picture.
*     10-AUG-2000 (DSB):
*        Modified to allow negative margins.
*     23-AUG-2006 (DSB):
*        When checking for zero-sized boxes, include effects of truncation
*        from double to single precision.
*     2020 July 30 (MJC):
*        Introduced D option in PSIDE.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      CHARACTER COMMNT*(*)
      REAL MARGIN( 4 )
      INTEGER NP
      CHARACTER PNAME( NP )*(*)
      CHARACTER PSIDE( NP )*(*)
      REAL PSIZE( NP )
      REAL SASPEC
      DOUBLE PRECISION BOX( 4 )

*  Arguments Returned:
      INTEGER IPICD
      INTEGER IPICF
      INTEGER IPIC( NP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ITOCM                 ! Factor for converting inches to centimetres
      PARAMETER ( ITOCM = 2.54 )

*  Local Variables:
      INTEGER I                  ! Ancillary picture index
      LOGICAL FILL               ! Create largest possible DATA picture?
      REAL ASPECT                ! The aspect ratio to use
      REAL CXI                   ! Current picture width in inches
      REAL CXL                   ! X at left of current picture in inches
      REAL CXR                   ! X at right of current picture in inches
      REAL CYB                   ! Y at bottom of current picture in inches
      REAL CYI                   ! Current picture height in inches
      REAL CYT                   ! Y at top of current picture in inches
      REAL DXI                   ! Width of DATA picture in inches
      REAL DXL                   ! X at left of DATA picture in inches
      REAL DXR                   ! X at right of DATA picture in inches
      REAL DYB                   ! Y at bottom of DATA picture in inches
      REAL DYI                   ! Height of DATA picture in inches
      REAL DYT                   ! Y at top of DATA picture in inches
      REAL FXI                   ! Width of FRAME picture in inches
      REAL FXL                   ! X at left of FRAME picture in inches
      REAL FXR                   ! X at right of FRAME picture in inches
      REAL FYB                   ! Y at bottom of FRAME picture in inches
      REAL FYI                   ! Height of FRAME picture in inches
      REAL FYT                   ! Y at top of FRAME picture in inches
      REAL GAP                   ! Gap to fill available space
      REAL PXI                   ! Width of ancillary picture in inches
      REAL PXL                   ! X at left of ancillary picture in inches
      REAL PXR                   ! X at right of ancillary picture in inches
      REAL PYB                   ! Y at bottom of ancillary picture in inches
      REAL PYI                   ! Height of ancillary picture in inches
      REAL PYT                   ! Y at top of ancillary picture in inches
      REAL RBOX1                 ! Single precision version of BOX(1)
      REAL RBOX2                 ! Single precision version of BOX(2)
      REAL RBOX3                 ! Single precision version of BOX(3)
      REAL RBOX4                 ! Single precision version of BOX(4)
      REAL SASP                  ! Aspect ratio of available space
      REAL SHIFT                 ! Shift required to centralise the area
      REAL SXL                   ! X at left of ancillary pictures in inches
      REAL SXR                   ! X at right of ancillary pictures in inches
      REAL SYB                   ! Y at bottom of ancillary pictures in inches
      REAL SYT                   ! Y at top of ancillary pictures in inches
*.

*  Initialise returned values.
      IPICF = -1
      IPICD = -1

      DO I = 1, NP
         IPIC( I ) = -1
      END DO

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure the current PGPLOT viewport matches the current AGI picture.
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the bounds of this viewport, in inches from the bottom left corner
*  of the screen. Save the size in inches.
      CALL PGQVP( 1, CXL, CXR, CYB, CYT )
      CXI = CXR - CXL
      CYI = CYT - CYB

*  Set the required aspect ratio for the new DATA picture. If the user
*  has set the FILL parameter TRUE, then use the aspect ratio which gives
*  the largest DATA picture. Otherwise, use the supplied aspect ratio.
      IF( SASPEC .NE. 0.0 ) THEN

         CALL PAR_GET0L( 'FILL', FILL, STATUS )
         IF( FILL ) THEN
            ASPECT = 0.0
         ELSE
            ASPECT = SASPEC
         END IF

      ELSE
         ASPECT = 0.0
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the bounds of the area available for the DATA picture after
*  taking account of the extra pictures and margins. To begin with, we
*  have the whole of the current picture available. FXL, etc, record
*  the bounds of the area still available.
      FXL = CXL
      FYB = CYB
      FXR = CXR
      FYT = CYT

*  Loop round the extra pictures in reverse order. Reduce the available
*  space to take account of each one.
      DO I = NP, 1, -1

         IF( PSIDE( I ) .EQ. 'L' ) THEN
            FXL = MIN( FXR, FXL + ABS( PSIZE( I )*CXI ) )
         ELSE IF( PSIDE( I ) .EQ. 'R' .OR. PSIDE( I ) .EQ. 'D' ) THEN
            FXR = MAX( FXL, FXR - ABS( PSIZE( I )*CXI ) )
         ELSE IF( PSIDE( I ) .EQ. 'T' ) THEN
            FYT = MAX( FYB, FYT - ABS( PSIZE( I )*CYI ) )
         ELSE
            FYB = MIN( FYT, FYB + ABS( PSIZE( I )*CYI ) )
         END IF

      END DO

*  Now remove the required margins from the available space.
      FYB = MIN( FYT, FYB + MARGIN( 1 )*CYI )
      FXR = MAX( FXL, FXR - MARGIN( 2 )*CXI )
      FYT = MAX( FYB, FYT - MARGIN( 3 )*CYI )
      FXL = MIN( FXR, FXL + MARGIN( 4 )*CXI )

*  Store the dimensions of the available space.
      FXI = FXR - FXL
      FYI = FYT - FYB

*  Report an error if the whole current picture has been used.
      IF( FXI .LE. 0.1 .OR. FYI .LE. 0.1 ) THEN

         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GDNEW_2', 'No room left for a '//
     :                    'DATA picture. Margins or extra pictures '//
     :                    '(e.g. a KEY picture) need to be made '//
     :                    'smaller.', STATUS )
         END IF

         GO TO 999

      END IF

*  Store the aspect ratio of the available space.
      SASP = FYI/FXI

*  If no aspect ratio has been given, use the whole of the available
*  space for the DATA Picture.
      IF( ASPECT .LE. 0.0 ) THEN
         DXL = FXL
         DXR = FXR
         DYB = FYB
         DYT = FYT
         DXI = DXR - DXL
         DYI = DYT - DYB

*  If the DATA picture is "taller" than the available space, use the full
*  height of the available space, but only use the width required to give
*  the correct aspect ratio.
      ELSE IF( ASPECT .GT. SASP ) THEN
         DYB = FYB
         DYT = FYT
         DYI = DYT - DYB
         DXI = DYI/ASPECT
         GAP = 0.5*( FXI - DXI )
         DXR = FXR - GAP
         DXL = FXL + GAP

*  If the DATA picture is "wider" than the available space, use the full
*  width of the available space, but only use the height required to give
*  the correct aspect ratio.
      ELSE
         DXR = FXR
         DXL = FXL
         DXI = DXR - DXL
         DYI = DXI*ASPECT
         GAP = 0.5*( FYI - DYI )
         DYT = FYT - GAP
         DYB = FYB + GAP

      END IF

*  Get the bounds of the area enclosing the ancillary pictures. To begin with,
*  set the area bounds equal to the DATA picture bounds.
      SXL = DXL
      SXR = DXR
      SYB = DYB
      SYT = DYT

*  Now add on the margins.
      SYB = SYB - MARGIN( 1 )*CYI
      SXR = SXR + MARGIN( 2 )*CXI
      SYT = SYT + MARGIN( 3 )*CYI
      SXL = SXL - MARGIN( 4 )*CXI

*  Now add on each of the extra pictures.
      DO I = 1, NP

         IF( PSIDE( I ) .EQ. 'L' ) THEN
            SXL = SXL - ABS( PSIZE( I )*CXI )
         ELSE IF( PSIDE( I ) .EQ. 'R' .OR. PSIDE( I ) .EQ. 'D' ) THEN
            SXR = SXR + ABS( PSIZE( I )*CXI )
         ELSE IF( PSIDE( I ) .EQ. 'T' ) THEN
            SYT = SYT + ABS( PSIZE( I )*CYI )
         ELSE
            SYB = SYB - ABS( PSIZE( I )*CYI )
         END IF

      END DO

*  These are the outer bounds of the outer ancillary pictures. If there
*  are any large negative margins, it is possible for the DATA picture
*  to extend outside the ancillary pictures. The FRAME picture needs to
*  enclose the DATA picture in all cases.
      FXL = MIN( DXL, SXL )
      FXR = MAX( DXR, SXR )
      FYB = MIN( DYB, SYB )
      FYT = MAX( DYT, SYT )

*  Now modify all bounds to centre the FRAME picture within the current
*  picture.
      SHIFT = 0.5*( ( FXR + FXL ) - ( CXR + CXL ) )
      DXL = DXL - SHIFT
      DXR = DXR - SHIFT
      FXL = FXL - SHIFT
      FXR = FXR - SHIFT
      SXL = SXL - SHIFT
      SXR = SXR - SHIFT

      SHIFT = 0.5*( ( FYT + FYB ) - ( CYT + CYB ) )
      DYB = DYB - SHIFT
      DYT = DYT - SHIFT
      FYB = FYB - SHIFT
      FYT = FYT - SHIFT
      SYB = SYB - SHIFT
      SYT = SYT - SHIFT

*  Ensure the FRAME picture does not extend beyond the current picture.
      FXL = MAX( CXL + 0.001, FXL )
      FXR = MIN( CXR - 0.001, FXR )
      FYB = MAX( CYB + 0.001, FYB )
      FYT = MIN( CYT - 0.001, FYT )

*  Ensure the area enclosing the ancillary pictures does not extend beyond
*  the current picture.
      SXL = MAX( CXL + 0.001, SXL )
      SXR = MIN( CXR - 0.001, SXR )
      SYB = MAX( CYB + 0.001, SYB )
      SYT = MIN( CYT - 0.001, SYT )

*  Store the FRAME picture, but only if the FRAME picture would contain
*  something other than the DATA picture. This is assumed to be so if
*  any ancillary pictures are being created, or if the margins around the
*  DATA picture are positive.
      IF( NP .GT. 0 .OR. MARGIN( 1 ) .GT. 0.0 .OR.
     :    MARGIN( 2 ) .GT. 0.0 .OR. MARGIN( 3 ) .GT. 0.0 .OR.
     :    MARGIN( 4 ) .GT. 0.0 ) THEN

*  Create the new viewport.
         CALL PGVSIZ( FXL, FXR, FYB, FYT )

*  Set its world co-ordinate bounds.
         CALL KPG1_GDWIN( -1, STATUS )

*  Save the viewport as a new picture.
         CALL AGP_SVIEW( 'FRAME', COMMNT, IPICF, STATUS )

*  This is now the current picture. Update the bounds of the current
*  picture.
         CXL = FXL
         CXR = FXR
         CYT = FYT
         CYB = FYB

*  If the FRAME and DATA pictures would be equivalent, use the current
*  picture as the "FRAME" picture.
      ELSE
         CALL AGI_ICURP( IPICF, STATUS )
      END IF

*  Now go round and create the ancillary pictures in reverse order.
*  Initially the entire area found above is available.
      DO I = NP, 1, -1

         IF( PSIDE( I ) .EQ. 'L' ) THEN
            PXL = SXL
            PXR = MIN( SXR, SXL + ABS( PSIZE( I )*CXI ) )
            PYB = SYB
            PYT = SYT

            SXL = PXR

         ELSE IF( PSIDE( I ) .EQ. 'R' ) THEN
            PXL = MAX( SXL, SXR - ABS( PSIZE( I )*CXI ) )
            PXR = SXR
            PYB = SYB
            PYT = SYT

            SXR = PXL

         ELSE IF( PSIDE( I ) .EQ. 'D' ) THEN
            PXL = MAX( SXL, SXR - ABS( PSIZE( I )*CXI ) )
            PXR = SXR
            PYB = DYB
            PYT = DYT

            SXR = PXL

         ELSE IF( PSIDE( I ) .EQ. 'T' ) THEN
            PXL = SXL
            PXR = SXR
            PYB = MAX( SYB, SYT - ABS( PSIZE( I )*CYI ) )
            PYT = SYT

            SYT = PYB

         ELSE
            PXL = SXL
            PXR = SXR
            PYB = SYB
            PYT = MIN( SYT, SYB + ABS( PSIZE( I )*CYI ) )

            SYB = PYT

         END IF

*  Ensure the picture does not extend beyond the current picture.
         PXL = MAX( CXL + 0.001, PXL )
         PXR = MIN( CXR - 0.001, PXR )
         PYB = MAX( CYB + 0.001, PYB )
         PYT = MIN( CYT - 0.001, PYT )

*  Create a PGPLOT viewport for this picture and save it as a picture
*  within the FRAME picture (but only if it has non-zero size). The world
*  co-ordinate system is inherited from the FRAME picture.
         PXI = PXR - PXL
         PYI = PYT - PYB
         IF( PXI .GT. 0.0 .AND. PYI .GT. 0.0 .AND.
     :       STATUS .EQ. SAI__OK ) THEN

            CALL PGVSIZ( PXL, PXR, PYB, PYT )
            CALL KPG1_GDWIN( IPICF, STATUS )

            CALL AGI_SELP( IPICF, STATUS )
            CALL AGP_SVIEW( PNAME( I ), COMMNT, IPIC( I ), STATUS )

*  Return a picture identifier of -1 if the picture would have had zero
*  size.
         ELSE
            IPIC( I ) = -1
         END IF

      END DO

*  Ensure the DATA picture does not extend beyond the current picture.
      DXL = MAX( CXL + 0.001, DXL )
      DXR = MIN( CXR - 0.001, DXR )
      DYB = MAX( CYB + 0.001, DYB )
      DYT = MIN( CYT - 0.001, DYT )

*  Create a PGPLOT viewport for the DATA picture, and set its world
*  co-ordinate bounds to the supplied values. If the box defined by
*  the bounds has zero size, use cm from the bottom left corner.
      IF( STATUS .EQ. SAI__OK ) THEN

*  Report an error if any of the BOX values are bad.
         IF( BOX( 1 ) .EQ. AST__BAD .OR.
     :       BOX( 2 ) .EQ. AST__BAD .OR.
     :       BOX( 3 ) .EQ. AST__BAD .OR.
     :       BOX( 4 ) .EQ. AST__BAD ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPG1_GDNEW_4', 'The bounds of the new '//
     :                    'DATA picture are undefined in AGI world '//
     :                    'co-ordinates.', STATUS )
            GO TO 999
         END IF

         CALL PGVSIZ( DXL, DXR, DYB, DYT )

         RBOX1 = REAL( BOX( 1 ) )
         RBOX2 = REAL( BOX( 2 ) )
         RBOX3 = REAL( BOX( 3 ) )
         RBOX4 = REAL( BOX( 4 ) )

         IF( RBOX1 .NE. RBOX3 .AND.
     :       RBOX2 .NE. RBOX4 ) THEN
            CALL PGSWIN( RBOX1, RBOX3, RBOX2, RBOX4 )

*  Report an error if the zero box size was a consequence of the
*  truncation from double to single precision.
         ELSE IF( RBOX1 .EQ. RBOX3 .AND.
     :            BOX( 1 ) .NE. BOX( 3 ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'L', BOX( 1 ) )
            CALL MSG_SETD( 'H', BOX( 3 ) )
            CALL ERR_REP( 'KPG1_GDNEW_5', 'The dynamic range spanned '//
     :                    'by the horizontal axis (^L to ^H) is '//
     :                    'too small.', STATUS )
            GO TO 999

         ELSE IF( RBOX2 .EQ. RBOX4 .AND.
     :            BOX( 2 ) .NE. BOX( 4 ) ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETD( 'L', BOX( 2 ) )
            CALL MSG_SETD( 'H', BOX( 4 ) )
            CALL ERR_REP( 'KPG1_GDNEW_6', 'The dynamic range spanned '//
     :                    'by the vertical axis (^L to ^H) is '//
     :                    'too small.', STATUS )
            GO TO 999

         ELSE
            CALL PGSWIN( 0.0, ( DXR - DXL )*ITOCM, 0.0,
     :                   ( DYT - DYB )*ITOCM )
         END IF

*  Save the current viewport and the Plot as a new DATA picture within the
*  FRAME picture.
         CALL AGI_SELP( IPICF, STATUS )
         CALL AGP_SVIEW( 'DATA', COMMNT, IPICD, STATUS )

      END IF

 999  CONTINUE

      END
