      SUBROUTINE KPG1_ASPRP( NDIM, INDF1, INDF2, MATRIX, OFFSET,
     :                       STATUS )
*+
*  Name:
*     KPG1_ASPRP

*  Purpose:
*     Propagates the WCS component from one NDF to another with the same
*     number of axes, allowing for a linear mapping of the pixel
*     co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPRP( NDIM, INDF1, INDF2, MATRIX, OFFSET, STATUS )

*  Description:
*     This routine copies the WCS FrameSet from INDF1, re-mapping the
*     GRID Frame in the process so that pixel co-ordinates in the output
*     NDF are related to pixel co-ordinates in the input NDF by the
*     supplied linear transformation. The mapping from pixel
*     co-ordinates in INDF1 ("PIX1") to the corresponding pixel
*     co-ordinates in INDF2 ("PIX2") is:
*
*        PIX2 = MATRIX . PIX1 + OFFSET
*
*     For instance, for NDIM = 2:
*
*        X2 = MATRIX( 1, 1 ).X1 + MATRIX( 2, 1 ).Y1 + OFFSET( 1 )
*        Y2 = MATRIX( 1, 2 ).X1 + MATRIX( 2, 2 ).Y1 + OFFSET( 2 )

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions. This should be the value returned by
*        NDF_BOUND for INDF2.
*     INDF1 = INTEGER (Given)
*        An identifier for the source NDF. If this does not have NDIM
*        pixel axes, a NDF section with NDIM axes will be obtained from
*        the supplied NDF.
*     INDF2 = INTEGER (Given)
*        An identifier for the destination NDF. This must have NDIM
*         pixel axes.
*     MATRIX( NDIM, NDIM ) = DOUBLE PRECISION (Given)
*        The matrix connecting PIX1 and PIX2.
*     OFFSET( NDIM ) = DOUBLE PRECISION (Given)
*        The offset vector for PIX2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     10-JUN-1998 (DSB):
*        Original version.
*     24-AUG-2005 (DSB):
*        Allow the source NDF to have a different number of pixel axes.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER NDIM
      INTEGER INDF1
      INTEGER INDF2
      DOUBLE PRECISION MATRIX( NDIM, NDIM )
      DOUBLE PRECISION OFFSET( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION INA( NDF__MXDIM ) ! Corner "A" of window in input Frame
      DOUBLE PRECISION INB( NDF__MXDIM ) ! Corner "B" of window in input Frame
      DOUBLE PRECISION OUTA( NDF__MXDIM )! Corner "A" of window in output Frame
      DOUBLE PRECISION OUTB( NDF__MXDIM )! Corner "B" of window in output Frame
      INTEGER G12MAP             ! Mapping from i/p GRID to o/p GRID
      INTEGER GP1MAP             ! Mapping from i/p GRID to i/p PIXEL
      INTEGER FRM                ! Frame pointer
      INTEGER I                  ! Loop count
      INTEGER IBASE              ! Index of original Base Frame
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER INDF1S             ! Identifier for a section of the input NDF
      INTEGER IPIX1              ! Index of PIXEL Frame in input WCS
      INTEGER IPIX2              ! Index of PIXEL Frame in output WCS
      INTEGER IWCS1              ! AST pointer to input WCS FrameSet
      INTEGER IWCS2              ! AST pointer to output WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel index bounds in INDF1
      INTEGER MTRMAP             ! AST pointer to MatrixMap
      INTEGER ND                 ! No. of dimensions in NDF1
      INTEGER P12MAP             ! Mapping from i/p PIXEL to o/p PIXEL
      INTEGER PG2MAP             ! Mapping from o/p PIXEL to o/p GRID
      INTEGER SMAP               ! Simplified Mapping from i/p GRID to o/p GRID
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel index bounds in INDF1
      INTEGER WINMAP             ! AST pointer to WinMap
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that the input NDF has the specified number of dimensions.
      CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, ND, STATUS )
      CALL NDF_SECT( INDF1, NDIM, LBND, UBND, INDF1S, STATUS )

*  Check that the output NDF has the specified number of dimensions.
      CALL NDF_BOUND( INDF2, NDF__MXDIM, LBND, UBND, ND, STATUS )
      IF( ND .NE. NDIM .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL MSG_SETI( 'NDIM', NDIM )
         CALL MSG_SETI( 'ND', ND )
         CALL ERR_REP( 'KPG1_ASPRP_2', 'KPG1_ASPRP: Programming error'//
     :                 ' - argument NDIM specifies ^NDIM axes, but '//
     :                 '''^NDF'' (INDF2) has ^ND axes.', STATUS )
      END IF

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Create a MatrixMap from the supplied MATRIX array.
      MTRMAP = AST_MATRIXMAP( NDIM, NDIM, 0, MATRIX, ' ', STATUS )

*  Create a WinMap which gives the required shift of pixel origin.
*  Map a window in pixel co-ordinates covering 1000 pixels on each
*  axis (a typical image size).
      DO I = 1, NDIM
         INA( I ) = 0.0D0
         INB( I ) = 1.0D3
         OUTA( I ) = INA( I ) + OFFSET( I )
         OUTB( I ) = INB( I ) + OFFSET( I )
      END DO

      WINMAP = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Concatenate these two mappings in series to get the mapping from
*  pixel co-ords in INDF1 to pixel co-ords in INDF2.
      P12MAP = AST_CMPMAP( MTRMAP, WINMAP, .TRUE., ' ', STATUS )

*  Get the WCS FrameSet from the input NDF. If no WCS component is
*  present in the NDF, an attempt is made to create a WCS FrameSet from
*  any IRAS90 astrometry structure, or FITS extension in the NDF.
      CALL KPG1_GTWCS( INDF1S, IWCS1, STATUS )

*  Loop round Frames in the FrameSet, until the PIXEL Frame is found
*  with the specified number of axes.
      IPIX1 = AST__NOFRAME
      DO I = 1, AST_GETI( IWCS1, 'Nframe', STATUS )

         FRM = AST_GETFRAME( IWCS1, I, STATUS )

         IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
            IF( AST_GETI( FRM, 'NAXES', STATUS ) .EQ. NDIM ) THEN
               IPIX1 = I
               GO TO 10
            END IF
         END IF

      END DO

 10   CONTINUE

*  Report an error if the PIXEL Frame was not found.
      IF( STATUS .EQ. SAI__OK .AND. IPIX1 .EQ. AST__NOFRAME ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF1 )
         CALL NDF_MSG( 'NDIM', NDIM )
         CALL ERR_REP( 'KPG1_ASPRP_3', 'No PIXEL Frame with ^NDIM '//
     :                 'axes found in the WCS component of ''^NDF''.',
     :                 STATUS )
      END IF

*  Get the mapping from the GRID to the PIXEL Frame.
      GP1MAP = AST_GETMAPPING( IWCS1, AST__BASE, IPIX1, STATUS )

*  Get the WCS FrameSet from the output NDF. We only need the PIXEL and
*  GRID Frames which are available even if the NDF has no WCS component.
*  For this reason we do not use KPG1_GTWCS which attempts to create a
*  FRAMESET from IRAS90 or FITS extensions if no WCS component is available.
      CALL NDF_GTWCS( INDF2, IWCS2, STATUS )

*  Loop round Frames in the FrameSet, until the PIXEL Frame is found with
*  NDIM axes.
      IPIX2 = AST__NOFRAME
      DO I = 1, AST_GETI( IWCS2, 'Nframe', STATUS )

         FRM = AST_GETFRAME( IWCS2, I, STATUS )

         IF( AST_GETC( FRM, 'DOMAIN', STATUS ) .EQ. 'PIXEL' ) THEN
            IF( AST_GETI( FRM, 'NAXES', STATUS ) .EQ. NDIM ) THEN
               IPIX2 = I
               GO TO 20
            END IF
         END IF

      END DO

 20   CONTINUE

*  Report an error if the PIXEL Frame was not found.
      IF( STATUS .EQ. SAI__OK .AND. IPIX2 .EQ. AST__NOFRAME ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL NDF_MSG( 'NDIM', NDIM )
         CALL ERR_REP( 'KPG1_ASPRP_4', 'No PIXEL Frame with ^NDIM '//
     :                 'axes found in the WCS component of ''^NDF''.',
     :                 STATUS )
      END IF

*  Get the mapping from the PIXEL to the GRID Frame.
      PG2MAP = AST_GETMAPPING( IWCS2, IPIX2, AST__BASE, STATUS )

*  Concatenate the mappings "GRID(in)->PIXEL(in)", "PIXEL(in)->PIXEL(out)",
*  and "PIXEL(out)->GRID(out)" to get the mapping "GRID(in)->GRID(out)".
      G12MAP = AST_CMPMAP( AST_CMPMAP( GP1MAP, P12MAP, .TRUE., ' ',
     :                                STATUS ),
     :                     PG2MAP, .TRUE., ' ', STATUS )

*  Simplify the mapping.
      SMAP = AST_SIMPLIFY( G12MAP, STATUS )

*  We now remap the Base (GRID) Frame in the input WCS FrameSet.
*  We should be able to just use AST_REMAPFRAME for this, but it seems
*  not to work for some reason.
*  ===================================================================

*  Note the indices of the Current and Base Frames from the input NDF.
      ICURR = AST_GETI( IWCS1, 'Current', STATUS )
      IBASE = AST_GETI( IWCS1, 'Base', STATUS )

*  Add a copy of the Base (GRID) Frame from the output NDF into the input
*  NDFs FrameSet. Connect it to the input NDFs Base (GRID) Frame using the
*  mapping found above. It becomes the Current Frame in IWCS1.
      CALL AST_ADDFRAME( IWCS1, AST__BASE, SMAP,
     :                   AST_GETFRAME( IWCS2, AST__BASE, STATUS ),
     :                   STATUS )

*  Make this new Frame the Base Frame.
      CALL AST_SETI( IWCS1, 'Base', AST__CURRENT, STATUS )

*  Re-instate the original Current Frame.
      CALL AST_SETI( IWCS1, 'Current', ICURR, STATUS )

*  Remove the original Base (GRID) Frame.
      CALL AST_REMOVEFRAME( IWCS1, IBASE, STATUS )

*  Store the FrameSet as the WCS component in the output NDF.
      CALL NDF_PTWCS( IWCS1, INDF2, STATUS )

*  Free resources.
      CALL NDF_ANNUL( INDF1S, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error has occurred, flush it, delete the output WCS component,
*  and then carry on since an output NDF with no WCS component may still
*  be useful.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL ERR_REP( 'KPG1_ASPRP_5', 'The output ''^NDF'' will have'//
     :                 ' no World co-ordinate System information.',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL NDF_RESET( INDF2, 'WCS', STATUS )
      END IF

      END
