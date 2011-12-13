      SUBROUTINE KPS1_CLPA2( INDF, STATUS )
*+
*  Name:
*     KPS1_CLPA2

*  Purpose:
*     Replace any current Frame GRID axes with corresponding PIXEL axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CLPA2( INDF, STATUS )

*  Description:
*     This routine searches the current Frame in the WCS FrameSet of the
*     supplied NDF, looking for duplicated GRID axes (these may be added
*     by KPS1_CLPA0). If any are found, a new Frame is created which is a
*     copy of the original current Frame, except that the GRID axes are
*     replaced by the correspsonding PIXEL axes. A Mapping is created
*     which maps positions from the original current Frame to this new
*     Frame (this Mapping is a UnitMap for the non-GRID axes in the
*     current Frame, and a shift of origin for the GRID axes). The new
*     Frame is added into the WCS FrameSet, using the above Mapping to
*     connect it to the original current Frame. This new Frame is left as
*     the current WCS Frame, and the original current Frame is removed from
*     the FrameSet.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the output NDF created by COLLAPSE.
*     STATUS = INTEGER (Given)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-2007 (DSB):
*        Original version.
*     5-MAR-2007 (DSB):
*        Correct logic for deciding whether to convert GRID axes to PIXEL
*        axes.
*     7-AUG-2007 (DSB):
*        Check that there are some non-GRID axes in the current Frame
*        before using the non-GRID axes.
*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10 ! WCS attribute name
      DOUBLE PRECISION SHIFT    ! Shift from GRID to PIXEL axis value
      INTEGER AX( NDF__MXDIM )  ! Base frm index of each cur frm axis
      INTEGER AXG( NDF__MXDIM ) ! Base frm index of each GRID axis
      INTEGER GAXID( NDF__MXDIM ) ! Cur frm index of each GRID axis
      INTEGER GFRM      ! Pointer to base (GRID) Frame
      INTEGER I         ! Loop count
      INTEGER IAT       ! Used length of string
      INTEGER ICUR      ! Index of original current Frame in IWCS
      INTEGER INAX      ! Index of the i/p axis to split off
      INTEGER IPFRM     ! Index of PIXEL Frame in IWCS
      INTEGER IWCS      ! Pointer to NDF WCS FrameSet
      INTEGER LBND( NDF__MXDIM )! NDF pixel index lower bounds
      INTEGER MAP       ! Pointer to a Mapping
      INTEGER NDIM      ! Number of pixel axes in NDF (=NGRID)
      INTEGER NEWFRM    ! Pointer to new current Frame
      INTEGER NG        ! Number of duplicated GRID axes
      INTEGER NGAXID( NDF__MXDIM )! Cur frm index of each non-GRID axis
      INTEGER NGFRM     ! Pointer to Frame of non-GRID cur frm axes
      INTEGER NGRID     ! Number of base Frame (GRID) axes
      INTEGER NNONG     ! Number of genuine (i.e. non-GRID) WCS axes
      INTEGER NWCS      ! Number of current Frame (WCS) axes
      INTEGER OUTAX( NDF__MXDIM ) ! O/p axes fed by the i/p axis
      INTEGER PERM( NDF__MXDIM )  ! Axis permutation array
      INTEGER PFRM      ! Pointer to Frame of duplicated PIXEL axes
      INTEGER TMAP      ! Pointer to a Mapping
      INTEGER UBND( NDF__MXDIM )! NDF pixel index upper bounds
      INTEGER WFRM      ! Pointer to original current Frame
      LOGICAL OK        ! Were all duplicated GRID axes identified?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the NDF WCS FrameSet.
      CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Get pointers to the GRID (base) Frame and WCS (current) Frame.
      GFRM = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      WFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Get the number of GRID axes and the number of WCS axes.
      NGRID = AST_GETI( GFRM, 'Naxes', STATUS )
      NWCS = AST_GETI( WFRM, 'Naxes', STATUS )

*  Get the Mapping from current Frame (WCS) to base Frame (GRID).
      MAP = AST_GETMAPPING( IWCS, AST__CURRENT, AST__BASE, STATUS )

*  Indicate that we have not yet found any unidentifiable GRID axes in
*  the current Frame.
      OK = .TRUE.

*  Initialise the number of grid and non-grid axes found in the current
*  Frame.
      NG = 0
      NNONG = 0

*  Check each current Frame axis in turn.
      DO I = 1, NWCS

*  See if its Domain is GRID.
         ATTR = 'Domain('
         IAT = 7
         CALL CHR_PUTI( I, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )
         IF( AST_GETC( WFRM, ATTR( : IAT ), STATUS ) .EQ.
     :       'GRID' ) THEN

*  If it is, get the index of the associated base Frame axis. Set a flag
*  if this is not possible.
            INAX = I
            CALL AST_MAPSPLIT( MAP, 1, INAX, OUTAX, TMAP, STATUS )
            IF( TMAP .NE. AST__NULL ) THEN
               IF( AST_GETI( TMAP, 'Nout', STATUS ) .EQ. 1 ) THEN
                  NG = NG + 1
                  GAXID( NG ) = I
                  AXG( NG ) = OUTAX( 1 )
                  AX( I ) = OUTAX( 1 )
               ELSE
                  OK = .FALSE.
               END IF

            ELSE
               OK = .FALSE.
            END IF

         ELSE
            NNONG = NNONG + 1
            NGAXID( NNONG ) = I
            AX( I ) = 0
         END IF

      END DO

*  Do nothing more if unidentifiable GRID axes were found in the current
*  Frame, or if no GRID axes were found.
      IF( OK .AND. NG .GT. 0 ) THEN

*  Create a Frame containing just the non-GRID current Frame axes.
         IF( NNONG .GT. 0 ) THEN
            NGFRM = AST_PICKAXES( WFRM, NNONG, NGAXID, TMAP, STATUS )
         ELSE
            NGFRM = AST__NULL
         END IF

*  Find the index of the PIXEL Frame.
         CALL KPG1_ASFFR( IWCS, 'PIXEL', IPFRM, STATUS )

*  Create a Frame containing the PIXEL axes that correspond to the GRID
*  axes found in the current Frame.
         PFRM = AST_PICKAXES( AST_GETFRAME( IWCS, IPFRM, STATUS ),
     :                        NG, AXG, TMAP, STATUS )

*  Combine these two Frames, and then permute the axes in the combined
*  Frame to put them back into the order used by the original current Frame.
         IF( NGFRM .NE. AST__NULL ) THEN
            NEWFRM = AST_CMPFRAME( NGFRM, PFRM, ' ', STATUS )
         ELSE
            NEWFRM = AST_CLONE( PFRM, STATUS )
         END IF

         DO I = 1, NNONG
            PERM( NGAXID( I ) ) = I
         END DO

         DO I = 1, NG
            PERM( GAXID( I ) ) = I + NNONG
         END DO

         CALL AST_PERMAXES( NEWFRM, PERM, STATUS )

*  Now create a Mapping form the original current Frame to this new Frame.
*  Values for non-GRID axes are just copied using a UnitMap. Values for
*  GRID axes are converted into the corresponding PIXEL axis value using
*  a ShiftMap. First get the pixel origin from the NDF.
         CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Clear the MAP pointer so that we can use it as a pointer to a parallel
*  CmpMap that combines 1D Mappings for all the current Frame axes.
         CALL AST_ANNUL( MAP, STATUS )

*  Now loop round each current Frame axis.
         DO I = 1, NWCS

*  If this is a non-GRID axis, create a UnitMap.
            IF( AX( I ) .EQ. 0 ) THEN
               TMAP = AST_UNITMAP( 1, ' ', STATUS )

*  If this is a GRID axis, create a ShiftMap that shifts a GRID value
*  into a PIXEL value on the current axis.
            ELSE
               SHIFT = DBLE( LBND( AX( I ) ) ) - 1.5D0
               TMAP = AST_SHIFTMAP( 1, SHIFT, ' ', STATUS )
            END IF

*  Add the above Mapping into the running parallel CmpMap.
            IF( MAP .EQ. AST__NULL ) THEN
               MAP = TMAP
            ELSE
               MAP = AST_CMPMAP( MAP, TMAP, .FALSE., ' ', STATUS )
            END IF

         END DO

*  Now add the new Frame into the WCS FrameSet, using the above Mapping
*  to connect it to the original current Frame. It becomes the new
*  current Frame, so first record the index of the original current Frame.
         ICUR = AST_GETI( IWCS, 'Current', STATUS )
         CALL AST_ADDFRAME( IWCS, AST__CURRENT, MAP, NEWFRM, STATUS )

*  Remove the original current Frame (unless it was also the base Frame).
         IF( AST_GETI( IWCS, 'Base', STATUS ) .NE. ICUR ) THEN
            CALL AST_REMOVEFRAME( IWCS, ICUR, STATUS )
         END IF

*  Simplify the Mappings in the FrameSet.
         CALL KPG1_ASSIM( IWCS, STATUS )

*  Store the new WCS FrameSet in the NDF.
         CALL NDF_PTWCS( IWCS, INDF, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
