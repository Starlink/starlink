      SUBROUTINE KPS1_WALA7( INDF1, IWCSR, MAP, MAP4, ORIGIN, STATUS )
*+
*  Name:
*     KPS1_WALA7

*  Purpose:
*     Get the Mapping from input to reference pixel co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_WALA7( INDF1, IWCSR, MAP, MAP4, ORIGIN, STATUS )

*  Description:
*     This routine finds the Mapping from input pixel co-ordinates to
*     reference (i.e. output) pixel co-ordinates. It determines if the
*     Mapping is a simple shift of origin with integer shifts on each axis,
*     and if so, returns the pixel origin for the output NDF.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the input NDF.
*     IWCSR = INTEGER (Given)
*        AST pointer for the WCS FrameSet from the reference NDF.
*     MAP = INTEGER (Returned)
*        A pointer to an AST Mapping from input pixel co-ordinates to
*        reference pixel co-ordinates.
*     MAP4 = INTEGER (Returned)
*        AST pointer to the Mapping from input grid coords to input
*        pixel coords.
*     ORIGIN( NDF__MXDIM ) = INTEGER (Return)
*        If the returned Mapping is a simple translation, then ORIGIN
*        is returned holding the result of transforming the pixel origin
*        of the input NDF using the Mapping. If the Mapping is not just a
*        simple translation, or if the translation is not an integer on
*        every axis, or if the input and output NDFs have different numbers
*        of pixel axes, all elements are returned holding VAL__BADI.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2004 (DSB):
*        Original version
*     6-JAN-2008 (DSB):
*        Correct conversion from pixel index limits to pixel coordinate
*        limits before checking for a linear mapping.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER IWCSR

*  Arguments Returned:
      INTEGER MAP
      INTEGER MAP4
      INTEGER ORIGIN( NDF__MXDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOMLST*50        ! List of preferred alignment domains
      DOUBLE PRECISION DIM( NDF__MXDIM )! Upper bounds of input NDF
      DOUBLE PRECISION DLBND( NDF__MXDIM )! Lower bounds of input NDF
      DOUBLE PRECISION DUBND( NDF__MXDIM )! Upper bounds of input NDF
      DOUBLE PRECISION FIT( NDF__MXDIM*( 1 + NDF__MXDIM ) )! Linear fit
      DOUBLE PRECISION V         ! Target value for fit coefficient
      INTEGER I                  ! Loop count
      INTEGER IAT                ! No. of characters in string
      INTEGER IIN                ! Input pixel axis index
      INTEGER IOUT               ! Output pixel axis index
      INTEGER IPIX1              ! Index of PIXEL Frame in input NDF FrameSet
      INTEGER IPIXR              ! Index of PIXEL Frame in ref. NDF FrameSet
      INTEGER IWCS1              ! AST pointer to input WCS FrameSet
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of input NDF
      INTEGER N                  ! Nearest integer to fit coefficient
      INTEGER NDIM1              ! No. of pixel axes in input NDF
      INTEGER NFRM               ! No. of Frames in input NDF FrameSet
      INTEGER NIN                ! No. of pixel axes in input NDF
      INTEGER NOUT               ! No. of pixel axes in output NDF
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of input NDF
*.

*  Initialise returned values.
      MAP = AST__NULL
      DO I = 1, NDF__MXDIM
         ORIGIN( I ) = VAL__BADI
      END DO

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Find the index of the PIXEL Frame in the reference NDF.
      CALL KPG1_ASFFR( IWCSR, 'PIXEL', IPIXR, STATUS )

*  Get the WCS FrameSet from the input NDF.
      CALL KPG1_GTWCS( INDF1, IWCS1, STATUS )

*  Find the index of the PIXEL Frame in the input NDF.
      CALL KPG1_ASFFR( IWCS1, 'PIXEL', IPIX1, STATUS )

*  Get the Mapping from the input GRID Frame to the input PIXEL Frame
      MAP4 = AST_GETMAPPING( IWCS1, AST__BASE, IPIX1, STATUS )

*  Save the number of Frames in the input WCS FrameSet.
      NFRM = AST_GETI( IWCS1, 'NFRAME', STATUS )

*  Store the list of preferences for the alignment Frame Domain (current
*  FRAME in the input NDF, followed by PIXEL). KPG1_ASMRG always uses the
*  Domain of the second FrameSet (IWCSR) first, so we do not need to include
*  it in this list.
      DOMLST = ' '
      IAT = 0
      CALL CHR_APPND( AST_GETC( IWCS1, 'DOMAIN', STATUS ), DOMLST, IAT )
      CALL CHR_APPND( ',PIXEL', DOMLST, IAT )

*  Merge the reference WCS FrameSet into this NDFs WCS FrameSet, aligning
*  them in a suitable Frame (the current Frame of IWCSR by preference, or
*  the first possible domain in the above list otherwise).
      CALL KPG1_ASMRG( IWCS1, IWCSR, DOMLST( : IAT ), .FALSE., 4,
     :                 STATUS )

*  Get the simplified Mapping from input pixel Frame to reference (i.e.
*  output) pixel Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS1, IPIX1, IPIXR + NFRM,
     :                                    STATUS ), STATUS )

*  If the input and output NDFs have the same number of pixel axes, see
*  if the Mapping is a simple translation.
      NOUT = AST_GETI( MAP, 'Nout', STATUS )
      NIN = AST_GETI( MAP, 'Nin', STATUS )
      IF( NIN .EQ. NOUT ) THEN

*  Get the pixel bounds of the input NDF and convert to double precision.
*  Also get the total axis dimensions.
         CALL NDF_BOUND( INDF1, NDF__MXDIM, LBND, UBND, NDIM1, STATUS )
         DO I = 1, NIN
            DLBND( I ) = DBLE( LBND( I ) ) - 1.0D0
            DUBND( I ) = DBLE( UBND( I ) )
            DIM( I ) = DUBND( I ) - DLBND( I )
         END DO

*  See if the returned Mapping is linear to within 0.1 of a pixel, and if so,
*  get a linear approximation to the Mapping.
         IF( AST_LINEARAPPROX( MAP, DLBND, DUBND, 0.1D0, FIT,
     :                         STATUS ) ) THEN

*  The first NOUT terms in the FIT array are the constant offsets for the
*  NOUT output axes. Skip over these to get the index of the first
*  element of the rotation matrix.
            I = NOUT + 1

*  Check each pixel axis in the output NDF.
            DO IOUT = 1, NOUT

*  Check the offset is an integer value on this output axis.
               N = NINT( FIT( IOUT ) )
               IF( ABS( DBLE( N ) - FIT( IOUT ) ) .GT. 0.01 ) GO TO 999

*  Check all elements in the current row of the rotation matrix.
               DO IIN = 1, NIN

*  Diagonal elements should be close to 1.0. Off-diagonal elments should
*  be close to zero.
                  IF( IIN .EQ. IOUT ) THEN
                     V = FIT( I ) - 1.0
                  ELSE
                     V = FIT( I )
                  END IF

*  Check the closeness.
                  IF( ABS( DIM( IIN )*V ) .GT. 0.01 ) GO TO 999

                  I = I + 1
               END DO
            END DO

* Translate the origin of the input NDF using the Mapping.
            DO I = 1, NOUT
               ORIGIN( I ) = NINT( FIT( I ) ) + LBND( I )
            END DO

         END IF
      END IF

*  Tidy up.
 999  CONTINUE

*  Export the Mappings from the current AST context into the parent context.
      CALL AST_EXPORT( MAP4, STATUS )
      CALL AST_EXPORT( MAP, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
