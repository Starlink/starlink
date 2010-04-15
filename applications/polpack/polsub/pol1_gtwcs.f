      SUBROUTINE POL1_GTWCS( INDF, TR, LBND, IWCS, STATUS )
*+
*  Name:
*     POL1_GTWCS

*  Purpose:
*     Get a FrameSet describing the WCS information to be stored with the
*     output 2D (or 3D) images created by polvec.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL POL1_GTWCS( INDF, TR, LBND, IWCS, STATUS )

*  Description:
*     This routine gets the FrameSet from the WCS component of the
*     supplied NDF. The Base Frame of this FrameSet (describing GRID
*     coordinates in the supplied NDF) is 3 (or 4) dimensional. A new 2D
*     (or 3D) Frame is added to the FrameSet which represents GRID coordinates
*     in the binned 2D (or 3D) images created by polvec. This Frame is
*     connected to the original GRID Frame by scaling and shifting
*     axes 1 and 2 according to the values supplied in TR. Since all
*     the planes in the input cube are presumed to be aligned, axes 3
*     and 4 are ignored. A new 2D (or 3D) PIXEL Frame is added to the
*     FrameSet using the supplied pixel bounds. The original GRID and
*     PIXEL Frames are removed.
*
*     If the Current Frame contains an axis with Symbol "STOKES", a new
*     Frame is created without this axis, and is added to the FrameSet as
*     the Current Frame.

*  Arguments:
*     INDF = INTEGER (Given)
*        The 3D (or 4D) input NDF.
*     TR( 4 ) = DOUBLE PRECISION (Given)
*        The coefficients of the linear mapping produced by the binning:
*           X' = TR1 + TR2*X
*           Y' = TR3 + TR4*Y
*           (F' = F)
*           Z' = Z
*        where (X,Y,F,Z) are GRID coordinates in the input cube, and
*        (X',Y',F',Z') are GRID coordinates in the binned cube. "F" is
*        the "frequecny" axis if the input cube is 4D. If the input is 4D
*        the Stokes axis is axis 4, otherwise it is axis 3.
*     LBND( 4 ) = INTEGER (Given)
*        The lower pixel bounds of the grid.
*     IWCS = INTEGER (Returned)
*        An AST identifier for the FrameSet to be stored with the output
*        2D (or 3D) images created by polvec.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-JAN-1998 (DSB):
*        Original version.
*     2-JUL-1998 (DSB):
*        Changed the corners of the box used to define the WinMap so
*        that it never has zero area. Supplied "1.0D0" instead of "1.0"
*        as constant value to AST_PERMMAP.
*     5-AUG-1998 (DSB):
*        Re-instate original Current Frame after adding new PIXEL Frame.
*     11-MAY-1999 (DSB):
*        Added arguments LBND.
*     2-FEB-2001 (DSB):
*        Modified to support 4D input NDFs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST__ constants and functions
      INCLUDE 'NDF_PAR'          ! NDF__ constants

*  Arguments Given:
      INTEGER INDF
      DOUBLE PRECISION TR( 4 )
      INTEGER LBND( 4 )

*  Arguments Returned:
      INTEGER IWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*15          ! Attribute name
      CHARACTER DOM*50           ! Domain name
      DOUBLE PRECISION INA( 3 )  ! Old GRID coords at point A
      DOUBLE PRECISION INB( 3 )  ! Old GRID coords at point B
      DOUBLE PRECISION OUTA( 3 ) ! New GRID coords at point A
      DOUBLE PRECISION OUTB( 3 ) ! New GRID coords at point B
      INTEGER AXES( NDF__MXDIM ) ! Axis indices
      INTEGER CMP                ! Pointer to a CmpMap
      INTEGER FRM                ! Pointer to a Frame
      INTEGER I                  ! Axis index
      INTEGER IAT                ! Location within string
      INTEGER IBASE              ! Index of original Base Frame
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER IFRM               ! Frame index
      INTEGER INEW               ! Index of new Frame
      INTEGER INPERM( 4 )        ! O/p axis mapping to each i/p axis
      INTEGER IPIX               ! Index of old PIXEL Frame
      INTEGER NAX                ! Number of axes
      INTEGER NDIMG              ! No. of pixel axes in the input NDF
      INTEGER NDIMO              ! No. of pixel axes in the output NDFs
      INTEGER NEWFRM             ! Pointer to a Frame
      INTEGER OUTPERM( 3 )       ! I/p axis mapping to each o/p axis
      INTEGER PERM               ! Pointer to a PermMap
      INTEGER STAXIS             ! Index of Stokes axis
      INTEGER TEMP               ! Pointer to FrameSet
      INTEGER WIN                ! Pointer to a WinMap
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the WCS information from the supplied 3D (or 4D) NDF (or equivalent
*  info from the FITS or IRAS90 extension).
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the number of axes in the GRID Frame (3 or 4), and the number of
*  axes in the output NDFs (2 or 3).
      NDIMG = AST_GETI( IWCS, 'NIN', STATUS )
      NDIMO = NDIMG - 1

*  Create a PermMap which will create a 2D (or 3D) Frame from the first 2
*  (or 3) axes of the 3D (or 4D) GRID Frame included in the above FrameSet.
*  The constant value 1.0 is assigned to the final axis when doing an inverse
*  transformation (i.e. from 2/3D Frame to 3/4D Frame).
      INPERM( 1 ) = 1
      INPERM( 2 ) = 2
      INPERM( 3 ) = 3
      INPERM( 4 ) = 4
      INPERM( NDIMG ) = -1
      OUTPERM( 1 ) = 1
      OUTPERM( 2 ) = 2
      OUTPERM( 3 ) = 3

      PERM = AST_PERMMAP( NDIMG, INPERM, NDIMO, OUTPERM, 1.0D0, ' ',
     :                    STATUS )

*  Now create a WinMap which scales and shifts the first two axes in the
*  2/3D Frame according to the values supplied in TR.
      INA( 1 ) = 0.0D0
      INA( 2 ) = 0.0D0
      INA( 3 ) = 0.0D0
      INB( 1 ) = 1.0D0
      INB( 2 ) = 1.0D0
      INB( 3 ) = 1.0D0

      OUTA( 1 ) = TR( 1 )
      OUTA( 2 ) = TR( 3 )
      OUTA( 3 ) = 0.0D0
      OUTB( 1 ) = TR( 1 ) + TR( 2 )
      OUTB( 2 ) = TR( 3 ) + TR( 4 )
      OUTB( 3 ) = 1.0D0

      WIN = AST_WINMAP( NDIMO, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Concatenate and simplify these two mappings.
      CMP = AST_SIMPLIFY( AST_CMPMAP( PERM, WIN, .TRUE., ' ', STATUS ),
     :                    STATUS )

*  Create the new 2/3D GRID Frame.
      FRM = AST_FRAME( NDIMO, 'DOMAIN=GRID', STATUS )
      CALL AST_SETC( FRM, 'Title', 'Data grid indices', STATUS )
      CALL AST_SETC( FRM, 'Label(1)', 'Data grid index 1', STATUS )
      CALL AST_SETC( FRM, 'Label(2)', 'Data grid index 2', STATUS )
      CALL AST_SETC( FRM, 'Symbol(1)', 'g1', STATUS )
      CALL AST_SETC( FRM, 'Symbol(2)', 'g2', STATUS )
      CALL AST_SETC( FRM, 'Unit(1)', 'pixel', STATUS )
      CALL AST_SETC( FRM, 'Unit(2)', 'pixel', STATUS )
      IF( NDIMO .EQ. 3 ) THEN
         CALL AST_SETC( FRM, 'Label(3)', 'Data grid index 3', STATUS )
         CALL AST_SETC( FRM, 'Symbol(3)', 'g3', STATUS )
         CALL AST_SETC( FRM, 'Unit(3)', 'pixel', STATUS )
      END IF

*  Save the indices of the Base (i.e. GRID) and Current Frames.
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Add the new 2/3D GRID Frame into the FrameSet, connecting it to
*  the base (i.e. GRID) Frame using the mapping created above.
      CALL AST_ADDFRAME( IWCS, AST__BASE, CMP, FRM, STATUS )

*  The above call will have changed the Current Frame to be the new Frame.
*  Get its index.
      INEW = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Re-instate the original Current Frame, and make the new Frame the base
*  Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )
      CALL AST_SETI( IWCS, 'BASE', INEW, STATUS )

*  Find the 3/4D PIXEL Frame within the WCS FrameSet. It becomes the
*  Current Frame.
      TEMP = AST_FINDFRAME( IWCS, AST_FRAME( NDIMG, ' ', STATUS ),
     :                      'PIXEL', STATUS )

*  Report an error if no PIXEL Frame was found. Otherwise, annul the
*  returned FrameSet.
      IF( TEMP .EQ. AST__NULL .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'POL1_GTWCS_1', 'No PIXEL Frame found in WCS '//
     :                'component of ^NDF (possible programming error).',
     :                STATUS )
      ELSE
         CALL AST_ANNUL( TEMP, STATUS )
      END IF

*  Get the index of the 3/4D PIXEL Frame, and re-instate the original
*  Current Frame.
      IPIX = AST_GETI( IWCS, 'CURRENT', STATUS )
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Create a new 2/3D PIXEL Frame.
      FRM = AST_FRAME( NDIMO, 'DOMAIN=PIXEL', STATUS )
      CALL AST_SETC( FRM, 'Title', 'Pixel coordinates', STATUS )
      CALL AST_SETC( FRM, 'Label(1)', 'Pixel coordinate 1', STATUS )
      CALL AST_SETC( FRM, 'Label(2)', 'Pixel coordinate 2', STATUS )
      CALL AST_SETC( FRM, 'Symbol(1)', 'p1', STATUS )
      CALL AST_SETC( FRM, 'Symbol(2)', 'p2', STATUS )
      CALL AST_SETC( FRM, 'Unit(1)', 'pixel', STATUS )
      CALL AST_SETC( FRM, 'Unit(2)', 'pixel', STATUS )
      IF( NDIMO .EQ. 3 ) THEN
         CALL AST_SETC( FRM, 'Label(3)', 'Pixel coordinate 3', STATUS )
         CALL AST_SETC( FRM, 'Symbol(3)', 'p3', STATUS )
         CALL AST_SETC( FRM, 'Unit(3)', 'pixel', STATUS )
      END IF

*  Create a WinMap which shifts each axis in the 2/3D GRID Frame
*  onto the 2/3D PIXEL Frame.
      INA( 1 ) = 0.5D0
      INA( 2 ) = 0.5D0
      INA( 3 ) = 0.5D0
      INB( 1 ) = 1.5D0
      INB( 2 ) = 1.5D0
      INB( 3 ) = 1.5D0

      OUTA( 1 ) = DBLE( LBND( 1 ) - 1 )
      OUTA( 2 ) = DBLE( LBND( 2 ) - 1 )
      OUTA( 3 ) = DBLE( LBND( 3 ) - 1 )
      OUTB( 1 ) = OUTA( 1 ) + 1.0D0
      OUTB( 2 ) = OUTA( 2 ) + 1.0D0
      OUTB( 3 ) = OUTA( 3 ) + 1.0D0

      WIN = AST_WINMAP( NDIMO, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Add the new 2/3D PIXEL Frame into the FrameSet, connecting it to
*  the base (i.e. GRID) Frame using the mapping created above.
      CALL AST_ADDFRAME( IWCS, AST__BASE, WIN, FRM, STATUS )

*  If the original Current Frame was the 3/4D Grid Frame, make the 2/3D GRID
*  Frame the Current Frame.
      IF( ICURR .EQ. IBASE ) THEN
         CALL AST_SETI( IWCS, 'CURRENT', INEW, STATUS )

*  If the original Current Frame was the 3/4D PIXEL Frame, leave the 2/3D
*  PIXEL as the Current Frame. Otherwise, reinstate the original Current
*  Frame.
      ELSE IF( ICURR .NE. IPIX ) THEN
         CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )
      END IF

*  Remove the original 3/4D GRID and PIXEL Frames, highest index first.
      CALL AST_REMOVEFRAME( IWCS, MAX( IBASE, IPIX ), STATUS )
      CALL AST_REMOVEFRAME( IWCS, MIN( IBASE, IPIX ), STATUS )

*  Now remove the Stokes axis from all Frames.
*  ==================================================

*  Note the index of the original current frame.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Search all frames
      DO IFRM = 1, AST_GETI( IWCS, 'NFRAME', STATUS )

*  Get this frame.
         FRM = AST_GETFRAME( IWCS, IFRM, STATUS )

*  Get a list of axis indices within this Frame, excluding any
*  STOKES axis.
         NAX = 0
         STAXIS = 0
         DO I = 1, AST_GETI( FRM, 'NAXES', STATUS )
            ATTR = 'Symbol('
            IAT = 7
            CALL CHR_PUTI( I, ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            IF( AST_GETC( FRM, ATTR( : IAT ), STATUS ) .NE.
     :          'STOKES' ) THEN
               NAX = NAX + 1
               AXES( NAX ) = I
            ELSE
               STAXIS = I
            END IF
         END DO

*  If there is a Stokes axis...
         IF( STAXIS .GT. 0 ) THEN

*  Create a new Frame from the Current Frame excluding the Stokes axis.
            NEWFRM = AST_PICKAXES( FRM, NAX, AXES, PERM, STATUS )

*  Modify its Domain to exclude the string "STOKES", potentially with
*  hyphens before or after.
            DOM = AST_GETC( NEWFRM, 'DOMAIN', STATUS )
            IAT = INDEX( DOM, '-STOKES' )
            IF( IAT .NE. 0 ) THEN
               DOM( IAT : IAT + 6 ) = ' '
            ELSE
               IAT = INDEX( DOM, 'STOKES-' )
               IF( IAT .NE. 0 ) THEN
                  DOM( IAT : IAT + 6 ) = ' '
               ELSE
                  IAT = INDEX( DOM, 'STOKES' )
                  IF( IAT .NE. 0 ) DOM( IAT : IAT + 5 ) = ' '
               END IF
            END IF

*  Clean and store the new Domain name.
            CALL CHR_RMBLK( DOM )
            CALL AST_SETC( NEWFRM, 'DOMAIN', DOM, STATUS )

*  Add this new Frame into the FrameSet.
            CALL AST_ADDFRAME( IWCS, IFRM, PERM, NEWFRM, STATUS )

*  If we have just checked the original Current Frame, the new Frame
*  should become the current Frame at the end.
            IF( IFRM .EQ. ICURR ) ICURR = AST_GETI( IWCS, 'CURRENT',
     :                                              STATUS )
         END IF

      END DO

*  Establish the correct current Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Export the identifier for the returned FrameSets to the next higher
*  context level. This means it will not be annulled by the following
*  call to AST_END.
      CALL AST_EXPORT( IWCS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
