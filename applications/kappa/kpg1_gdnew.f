      SUBROUTINE KPG1_GDNEW( COMMNT, MARGIN, NP, PNAME, PSIDE,
     :                       PSIZE, SASPEC, BOX, IPICD, IPICF, IPIC, 
     :                       STATUS )
*+
*  Name:
*     KPG1_GDNEW

*  Purpose:
*     Create a new DATA picture with ancillary pictures.

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
*     Various environment parameters are used to obtain options, etc. The
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
*        being run (eg KAPPA_DISPLAY).
*     MARGIN( 4 ) = REAL (Given)
*        The width of the borders to leave round the DATA picture, given
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
*     SASPEC = REAL (Given)
*        The aspect ratio with which a new DATA picture should be created.
*        This is the height divided by the width of the DATA picture,
*        assuming equal scales on each axis (eg metres). A value of zero
*        causes the DATA picture to have the aspect ratio which produces
*        the largest picture. The actual value used will depend on the
*        value supplied for the FILL parameter (see above).
*     BOX( 4 ) = DOUBLE PRECISION (Given)
*        The world coordinate bounds to give to the DATA picture if a new 
*        DATA picture is created. These should normally be pixel
*        coordinates. The (x,y) coordinates of the bottom left corner should 
*        be given in elements 1 and 2, and the (x,y) coordinates of the top 
*        right corner should be given in elements 3 and 4. If the box has 
*        zero area, then world coordinates are set to centimetres from the 
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
*        requested in ZSIDE and PSIZE. The world coordinate system for each
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

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1998 (DSB):
*        Original version.
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
      REAL ALPHA                 ! Normalised width excluding the DATA pic
      REAL ASPECT                ! The aspect ratio to use
      REAL BETA                  ! Normalised height excluding the DATA pic
      REAL CXL                   ! X at left of current picture in inches
      REAL CXR                   ! X at right of current picture in inches
      REAL CYB                   ! Y at bottom of current picture in inches
      REAL CYT                   ! Y at top of current picture in inches
      REAL DXI                   ! Width of DATA picture in inches
      REAL DXL                   ! X at left of DATA picture in inches
      REAL DXR                   ! X at right of DATA picture in inches
      REAL DYB                   ! Y at bottom of DATA picture in inches
      REAL DYI                   ! Height of DATA picture in inches
      REAL DYT                   ! Y at top of DATA picture in inches
      REAL FH                    ! Used height within FRAME picture
      REAL FW                    ! Used width within FRAME picture
      REAL FXI                   ! Width of FRAME picture in inches
      REAL FXL                   ! X at left of FRAME picture in inches
      REAL FXR                   ! X at right of FRAME picture in inches
      REAL FYB                   ! Y at bottom of FRAME picture in inches
      REAL FYI                   ! Height of FRAME picture in inches
      REAL FYT                   ! Y at top of FRAME picture in inches
      REAL PXI                   ! Width of ancillary picture in inches
      REAL PXL                   ! X at left of ancillary picture in inches
      REAL PXR                   ! X at right of ancillary picture in inches
      REAL PYB                   ! Y at bottom of ancillary picture in inches
      REAL PYI                   ! Height of ancillary picture in inches
      REAL PYT                   ! Y at top of ancillary picture in inches
      REAL SZHP                  ! Total normalised height of horizontal pic.s
      REAL SZVP                  ! Total normalised width of vertical pic.s
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
      FXI = CXR - CXL
      FYI = CYT - CYB

*  Store the current viewport as a FRAME picture, but only if the FRAME
*  picture would contain something other than the DATA picture. This is
*  assumed to be so if any ancillary pictures are being created, or if the 
*  margins around the DATA picture are not zero.
      IF( NP .GT. 0 .OR. MARGIN( 1 ) .GT. 0.0 .OR. 
     :    MARGIN( 2 ) .GT. 0.0 .OR. MARGIN( 3 ) .GT. 0.0 .OR.
     :    MARGIN( 4 ) .GT. 0.0 ) THEN

         CALL AGP_SVIEW( 'FRAME', COMMNT, IPICF, STATUS )

*  If the FRAME and DATA pictures would be equivalent, use the current
*  picture as the "FRAME" picture.
      ELSE
         CALL AGI_ICURP( IPICF, STATUS )
      END IF

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

*  Find the total width of extra pictures to be created which run
*  vertically, and the width of those which run horizontally. These
*  "widths" are in units of the corresponding dimension of the DATA picture
*  (which has not yet been decided).
      SZVP = 0
      SZHP = 0
      DO I = 1, NP

         IF( PSIDE( I ) .EQ. 'R' .OR. PSIDE( I ) .EQ. 'L' ) THEN
            SZVP = SZVP + ABS( PSIZE( I ) )

         ELSE IF( PSIDE( I ) .EQ. 'B' .OR. PSIDE( I ) .EQ. 'T' ) THEN
            SZHP = SZHP + ABS( PSIZE( I ) )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', PNAME( I ) )
            CALL MSG_SETC( 'SIDE', PSIDE( I ) )
            CALL ERR_REP( 'KPG1_GDNEW_1', 'KPG1_GDNEW: Invalid '//
     :                    'side ''^SIDE'' requested for ^NAME '//
     :                    'picture (programming error).', STATUS )
            GO TO 999
         END IF
      END DO

*  Add on the width of of the margins around the DATA picture.
      ALPHA = MARGIN( 2 ) + MARGIN( 4 ) + SZVP
      BETA = MARGIN( 1 ) + MARGIN( 3 ) + SZHP

*  Calculate the size of the DATA picture (in inches) which leaves room for
*  the extra pictures and margins. If a zero or negative aspect ratio has 
*  been given, choose an aspect ratio which gives the largest DATA picture.
      IF( ASPECT .LE. 0.0 ) THEN
         DXI = FXI/( 1.0 + ALPHA )
         DYI = FYI/( 1.0 + BETA )

*  If a positive aspect ratio was given, use it.
      ELSE 
         DXI = MIN( FXI/( 1.0 + ALPHA ), 
     :              FYI/( ASPECT*( 1.0 + BETA ) ) ) 
         DYI = DXI*ASPECT
      END IF

*  Find the width and height which will actually be used in the Frame
*  picture.
      FW = MIN( FXI, DXI*( 1 + ALPHA ) )
      FH = MIN( FYI, DYI*( 1 + BETA ) ) 

*  Initialise the bounds of the area within the FRAME picture which is still 
*  available for extra pictures to be created in. Centre the used area of
*  the Frame within the whole Frame.
      FXL = CXL + 0.5*( FXI - FW )
      FXR = CXR - 0.5*( FXI - FW )
      FYB = CYB + 0.5*( FYI - FH )
      FYT = CYT - 0.5*( FYI - FH )

*  Loop round creating the extra pictures in reverse order. Each is created 
*  within the remaining area in the FRAME box. Change the bounds of the 
*  remaining area to exclude the area occupied by each picture as it is 
*  created. Limit each picture to be totally within the current picture.
      DO I = NP, 1, -1

         IF( PSIDE( I ) .EQ. 'L' ) THEN
            PXL = FXL
            PXR = MIN( CXR, FXL + ABS( PSIZE( I )*DXI ) )
            PYB = FYB 
            PYT = FYT

            FXL = PXR

         ELSE IF( PSIDE( I ) .EQ. 'R' ) THEN
            PXL = MAX( CXL, FXR - ABS( PSIZE( I )*DXI ) )
            PXR = FXR
            PYB = FYB 
            PYT = FYT

            FXR = PXL

         ELSE IF( PSIDE( I ) .EQ. 'T' ) THEN
            PXL = FXL
            PXR = FXR
            PYB = MAX( CYB, FYT - ABS( PSIZE( I )*DYI ) )
            PYT = FYT

            FYT = PYB

         ELSE 
            PXL = FXL
            PXR = FXR
            PYB = FYB
            PYT = MIN( CYT, FYB + ABS( PSIZE( I )*DYI ) )

            FYB = PYT

         END IF

*  Create a PGPLOT viewport for this picture and save it as a picture 
*  within the FRAME picture (but only if it has non-zero size). The world
*  coordinate system is inherited from the FRAME picture.
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

*  Get the bounds of the DATA picture, using the specified margins.
*  Limit to the bounds of the current picture.
      DXL = MAX( CXL, FXL + MARGIN( 4 )*DXI )
      DXR = MIN( CXR, DXL + DXI )
      DYB = MAX( CYB, FYB + MARGIN( 1 )*DYI )
      DYT = MIN( CYT, DYB + DYI )

*  Report an error if the DATA pcture has zero area.
      IF( ( DXL .GE. DXR .OR. DYB .GE. DYT ) .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_GDNEW_2', 'DATA picture would have zero '//
     :                 'size.', STATUS )
      
*  Otherwise, create a PGPLOT viewport for the DATA picture, and set its 
*  world coordinate bounds to the supplied values. If the box defined by
*  the bounds has zero size, use cm from the bottom left corner.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN

*  Report an error if any of the BOX values are bad.
         IF( BOX( 1 ) .EQ. AST__BAD .OR.
     :       BOX( 2 ) .EQ. AST__BAD .OR.
     :       BOX( 3 ) .EQ. AST__BAD .OR.
     :       BOX( 4 ) .EQ. AST__BAD ) THEN
            STATUS = SAI__OK
            CALL ERR_REP( 'KPG1_GDNEW_ERR', 'The bounds of the new '//
     :                    'DATA picture are undefined in AGI world '//
     :                    'co-ordinates.', STATUS )
            GO TO 999
         END IF

         CALL PGVSIZ( DXL, DXR, DYB, DYT )
         IF( BOX( 1 ) .NE. BOX( 3 ) .AND.
     :       BOX( 2 ) .NE. BOX( 4 ) ) THEN
            CALL PGSWIN( REAL( BOX( 1 ) ), REAL( BOX( 3 ) ), 
     :                      REAL( BOX( 2 ) ), REAL( BOX( 4 ) ) )
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
