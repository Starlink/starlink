      SUBROUTINE KPG1_GDOLD( MARGIN, COMMNT, NP, PNAME, PSIDE, PSIZE, 
     :                       IPICD, IPICD0, IPICF, IPIC, STATUS )
*+
*  Name:
*     KPG1_GDOLD

*  Purpose:
*     Create a new DATA picture with ancillary pictures aligned with 
*     an existing DATA picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_GDOLD( MARGIN, COMMNT, NP, PNAME, PSIDE, PSIZE, 
*                      IPICD, IPICD0, IPICF, IPIC, STATUS )

*  Description:
*     This routine creates a new DATA picture aligned with an existing 
*     DATA picture, together with any requested ancillary pictures. On
*     exit, the new DATA picture is the current picture, and the current 
*     PGPLOT viewport corresponds to this picture.
*
*     A FRAME picture is only created if it would contain something other
*     than the DATA picture (this is assumed to be the case if any
*     ancillary pictures are requested, or if non-zero margins are requested
*     around the DATA picture). Ancillary pictures are given their 
*     requested sizes except that they are clipped at the bounds of the 
*     original current picture. The FRAME picture is also clipped at the 
*     bounds of the original current picture.

*  Arguments:
*     MARGIN( 4 ) = REAL (Given)
*        The width of the borders to leave round the DATA picture, given
*        as fractions of the corresponding dimension of the DATA picture.
*        These should be supplied in the order bottom, right, top, left.
*     COMMNT = CHARACTER * ( * ) (Given)
*        A comment to store with the new pictures added to the AGI
*        database. This will usually by an indication of the application
*        being run (eg KAPPA_DISPLAY).
*     NP = INTEGER (Given)
*        The number of extra pictures to be included in the FRAME picture
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
*     IPICD = INTEGER (Given)
*        An AGI identifier for the existing DATA picture.
*     IPICD0 = INTEGER (Returned)
*        An AGI identifier for the new DATA picture.
*     IPICF = INTEGER (Returned)
*        An AGI identifier for the FRAME picture. Thw world cooridnate
*        system is inherited form the current picture on entry. If no 
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
      INCLUDE 'AGI_PAR'          ! AGI constants

*  Arguments Given:
      REAL MARGIN( 4 )
      CHARACTER COMMNT*(*)
      INTEGER NP
      CHARACTER PNAME( NP )*(*)
      CHARACTER PSIDE( NP )*(*)
      REAL PSIZE( NP )
      INTEGER IPICD

*  Arguments Returned:
      INTEGER IPICD0
      INTEGER IPICF
      INTEGER IPIC( NP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL ITOCM                 ! Factor for converting inches to centimetres
      PARAMETER ( ITOCM = 2.54 )

*  Local Variables:
      INTEGER I                  ! Ancillary picture index
      INTEGER IPIC0              ! AGI identifier for original current picture
      REAL CXL                   ! X at left of current picture in inches
      REAL CXR                   ! X at right of current picture in inches
      REAL CYB                   ! Y at bottom of current picture in inches
      REAL CYT                   ! Y at top of current picture in inches
      REAL DXI                   ! X size of DATA picture in inches
      REAL DXL                   ! X at left of DATA picture in inches
      REAL DXR                   ! X at right of DATA picture in inches
      REAL DYB                   ! Y at bottom of DATA picture in inches
      REAL DYI                   ! Y size of DATA picture in inches
      REAL DYT                   ! Y at top of DATA picture in inches
      REAL FXL                   ! X at left of FRAME picture in inches
      REAL FXR                   ! X at right of FRAME picture in inches
      REAL FYB                   ! Y at bottom of FRAME picture in inches
      REAL FYT                   ! Y at top of FRAME picture in inches
      REAL PXI                   ! Width of ancillary picture in inches
      REAL PXL                   ! X at left of ancillary picture in inches
      REAL PXR                   ! X at right of ancillary picture in inches
      REAL PYB                   ! Y at bottom of ancillary picture in inches
      REAL PYI                   ! Height of ancillary picture in inches
      REAL PYT                   ! Y at top of ancillary picture in inches
*.

*  Initialise returned values.
      IPICF = -1

      DO I = 1, NP
         IPIC( I ) = -1
      END DO

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an identifier for the current picture.
      CALL AGI_ICURP( IPIC0, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the bounds of the current viewport (i.e. the current picture). The 
*  values returned are inches from the bottom left corner of the screen.
      CALL PGQVP( 1, CXL, CXR, CYB, CYT )

*  Make the existing DATA picture current.
      CALL AGI_SELP( IPICD, STATUS )

*  Create a PGPLOT viewport from the DATA picture. The viewport covers the 
*  entire picture (no border is left).
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Find the bounds of the viewport (i.e. the DATA picture). The values 
*  returned are inches from the bottom left corner of the screen.
      CALL PGQVP( 1, DXL, DXR, DYB, DYT )

*  Find the dimensions of the DATA picture, in inches.
      DXI = DXR - DXL
      DYI = DYT - DYB

*  Find the bounds of the area containing the DATA picture and the margins.
      FYB = DYB - DYI * MARGIN( 1 )
      FXR = DXR + DXI * MARGIN( 2 )
      FYT = DYT + DYI * MARGIN( 3 )
      FXL = DXL - DXI * MARGIN( 4 )

*  Loop round each required extra picture, extending the above bounds to 
*  enclose it. Note, the pictures are not actually created yet since
*  we do not yet have a FRAME picture to contain them. This pass through
*  the list of extra pictures is done in order to determine the size of 
*  the required FRAME picture. The extra pictures will be created once
*  the FRAME picture has been created.
      DO I = 1, NP

*  Find the bounds of this extra picture.
         IF( PSIDE( I ) .EQ. 'R' ) THEN
            PXL = FXR
            PXR = FXR + ABS( PSIZE( I )*DXI )
            PYB = FYB
            PYT = FYT

         ELSE IF( PSIDE( I ) .EQ. 'L' ) THEN
            PXL = FXL - ABS( PSIZE( I )*DXI )
            PXR = FXL
            PYB = FYB
            PYT = FYT

         ELSE IF( PSIDE( I ) .EQ. 'T' ) THEN
            PXL = FXL
            PXR = FXR
            PYB = FYT
            PYT = FYT + ABS( PSIZE( I )*DYI )

         ELSE IF( PSIDE( I ) .EQ. 'B' ) THEN
            PXL = FXL
            PXR = FXR
            PYB = FYB - ABS( PSIZE( I )*DYI )
            PYT = FYB

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'NAME', PNAME( I ) )
            CALL MSG_SETC( 'SIDE', PSIDE( I ) )
            CALL ERR_REP( 'KPG1_GDDAT_1', 'KPG1_GDDAT: Invalid '//
     :                    'side ''^SIDE'' requested for ^NAME '//
     :                    'picture (programming error).', STATUS )
            GO TO 999
         END IF

*  Clip the bounds of the new picture so that it does not extend outside the 
*  original current picture.
         PXL = MAX( CXL, MIN( CXR, PXL ) )
         PXR = MAX( CXL, MIN( CXR, PXR ) )
         PYB = MAX( CYB, MIN( CYT, PYB ) )
         PYT = MAX( CYB, MIN( CYT, PYT ) )

*  Extend the Frame box to enclose it.
         FXL = MIN( FXL, PXL )
         FXR = MAX( FXR, PXR )
         FYB = MIN( FYB, PYB )
         FYT = MAX( FYT, PYT )

      END DO

*  Clip the bounds so that the box does not extend outside the original 
*  current picture.
      FXL = MAX( CXL, MIN( CXR, FXL ) )
      FXR = MAX( CXL, MIN( CXR, FXR ) )
      FYB = MAX( CYB, MIN( CYT, FYB ) )
      FYT = MAX( CYB, MIN( CYT, FYT ) )

*  Create a PGPLOT viewport with these bounds and save it as a FRAME picture 
*  within the original current picture (but only if some ancillary pictures
*  or non-zero margins were requested). World co-ordinates in the FRAME
*  picture are inherited from the current picture on entry.
      IF( ( NP .GT. 0 .OR. MARGIN( 1 ) .GT. 0.0 .OR. 
     :    MARGIN( 2 ) .GT. 0.0 .OR. MARGIN( 3 ) .GT. 0.0 .OR.
     :    MARGIN( 4 ) .GT. 0.0 ) .AND. STATUS .EQ. SAI__OK ) THEN

*  Set the viewport for the FRAME picture (in inches).
         CALL PGVSIZ( FXL, FXR, FYB, FYT )

*  Set the bounds of the PGPLOT window so that they correspond to world
*  co-ordinates in the original current picture.
         CALL KPG1_GDWIN( IPIC0, STATUS )

*  Select the original current picture, and create the FRAME picture
*  within it from the current viewport and window.
         CALL AGI_SELP( IPIC0, STATUS )
         CALL AGP_SVIEW( 'FRAME', COMMNT, IPICF, STATUS )

*  If the FRAME and DATA pictures would be equivalent, use the current
*  picture as the "FRAME" picture.
      ELSE
         IPICF = IPIC0
         CALL AGI_SELP( IPICF, STATUS )
         CALL AGP_NVIEW( .FALSE., STATUS )
      END IF

*  We now go back and create any extra required pictures within the FRAME
*  picture just created. Restore the bounds of the area containing the 
*  DATA picture and the margin.
      FYB = DYB - DYI * MARGIN( 1 )
      FXR = DXR + DXI * MARGIN( 2 )
      FYT = DYT + DYI * MARGIN( 3 )
      FXL = DXL - DXI * MARGIN( 4 )

*  Loop round each required extra picture, creating it if possible.
      DO I = 1, NP

*  Find the bounds of this extra picture.
         IF( PSIDE( I ) .EQ. 'R' ) THEN
            PXL = FXR
            PXR = FXR + ABS( PSIZE( I )*DXI )
            PYB = FYB
            PYT = FYT

         ELSE IF( PSIDE( I ) .EQ. 'L' ) THEN
            PXL = FXL - ABS( PSIZE( I )*DXI )
            PXR = FXL
            PYB = FYB
            PYT = FYT

         ELSE IF( PSIDE( I ) .EQ. 'T' ) THEN
            PXL = FXL
            PXR = FXR
            PYB = FYT
            PYT = FYT + ABS( PSIZE( I )*DYI )

         ELSE 
            PXL = FXL
            PXR = FXR
            PYB = FYB - ABS( PSIZE( I )*DYI )
            PYT = FYB

         END IF

*  Clip the bounds of the new picture so that it does not extend outside the 
*  original current picture.
         PXL = MAX( CXL, MIN( CXR, PXL ) )
         PXR = MAX( CXL, MIN( CXR, PXR ) )
         PYB = MAX( CYB, MIN( CYT, PYB ) )
         PYT = MAX( CYB, MIN( CYT, PYT ) )

*  Create a PGPLOT viewport with these bounds and save it as a picture 
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

*  Extend the Frame box to enclose the new picture.
            FXL = MIN( FXL, PXL )
            FXR = MAX( FXR, PXR )
            FYB = MIN( FYB, PYB )
            FYT = MAX( FYT, PYT )

*  Return a picture identifier of -1 if the picture wouild have had zero
*  size.
         ELSE
            IPIC( I ) = -1
         END IF

      END DO

*  Create a viewport corresponding to the existing DATA picture.
      CALL AGI_SELP( IPICD, STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Save this viewport as the new DATA picture within the Frame picture.
      CALL AGI_SELP( IPICF, STATUS )
      CALL AGP_SVIEW( 'DATA', COMMNT, IPICD0, STATUS )

 999  CONTINUE

      END
