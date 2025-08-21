      SUBROUTINE KPS1_SQALS( INDF, IWCS, AXIS, ALIGN, NEWSCL, SHIFT,
     :                       STATUS )
*+
*  Name:
*     KPS1_SQALS

*  Purpose:
*     Determines the pixel shifts to align with multiples of the
*     compressed scale.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SQALS( INDF, IWCS, AXIS, ALIGN, NEWSCL, SHIFT, STATUS )

*  Description:
*     This routine finds the lower bounds of the bounding box of the
*     compressed NDF constructed by SQORST, transforms the first GRID
*     location along each axis to the current WCS and determines the
*     shifts needed to make the compressed bins align with multiples of
*     the compressed pixel/channel width.

*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier of the compressed (output) NDF.
*     IWCS = INTEGER (Given)
*        The WCS FrameSet associated with the compressed NDF.
*     AXIS = INTEGER (Given)
*        The index of the axis to be aligned.  If it is 0, all axes
*        are adjusted.
*     ALIGN = CHARACTER * ( * ) (Given)
*        The co-ordinate alignment of the channels in the compressed
*        array.  Allowed alignment options are "Middle" or "Edge",
*        which may be abbreviated to "M" or "E", respectively.  The
*        former makes the channel centres an integer multiple of the
*        new scale, while the latter sets the channel edge to be a
*        multiple of the new scale.
*     NEWSCL( * ) = DOUBLE PRECISION (Given)
*        The new channel sizes after compression.  The array dimension
*         should be at least the number of axes in the FrameSet IWCS.
*     SHIFT( * ) = DOUBLE PRECISION (Returned)
*        The co-ordinate shifts to be applied to the output bounds to
*        align with the channel centres or edges, as selected by ALIGN.
*        It is set to zero for all axes except those chosen by AXIS.
*        Its array dimension should be at least the number of axes in
*        the FrameSet IWCS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2025 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 3 of
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
*     MJC: Malcolm J. Currie  (RAL)
*     {enter_new_authors_here}

*  History:
*     2025 July 21 (MJC):
*        Original version.
*   {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF
      INTEGER IWCS
      INTEGER AXIS
      CHARACTER*(*) ALIGN
      DOUBLE PRECISION NEWSCL( * )

*  Arguments Returned:
      DOUBLE PRECISION SHIFT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION ASIGN     ! Sign of lower bound
      INTEGER AXHIGH             ! Index of highest axis for alignment
      INTEGER AXLOW              ! Index of lowest axis for alignment
      DOUBLE PRECISION CFIRST( 2, NDF__MXDIM ) ! World co-ords of first
      DOUBLE PRECISION CSIGN     ! Co-ord direction from edge to centre
      INTEGER DIM( NDF__MXDIM )! Dimension sizes
      INTEGER FRMNAX             ! Frame dimensionality
      DOUBLE PRECISION FSHIFT    ! Fractional shift
      DOUBLE PRECISION GFIRST( 2, NDF__MXDIM ) ! GRID co-ords of first
                                 ! element
      DOUBLE PRECISION HAVECO    ! Current lower bound
      INTEGER IAXIS              ! Loop counter for axes
      INTEGER LBND( NDF__MXDIM ) ! Dimension lower bounds
      INTEGER MAP                ! Mapping from GRID to Current Frame
      INTEGER NDIM               ! Number of dimensions
      DOUBLE PRECISION NOCHAN    ! Number of channels at new scale
      INTEGER UBND( NDF__MXDIM ) ! Dimension upper bounds
      DOUBLE PRECISION WANTCO    ! Wanted bound

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Assign default returned values.
      FRMNAX = AST_GETI( IWCS, 'NAXES', STATUS )

      DO IAXIS = 1, FRMNAX
         SHIFT( IAXIS ) = 0.0D0
      END DO

*  Determine the axis WCS lower bound.
*  ===================================
      IF ( ALIGN( 1:1 ) .EQ. 'M' .OR. ALIGN( 1:1 ) .EQ. 'E' ) THEN

*  Obtain the pixel-index bounds.
         CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

         IF ( AXIS .EQ. 0 ) THEN
            AXLOW = 1
            AXHIGH = FRMNAX
         ELSE
            AXLOW = AXIS
            AXHIGH = AXIS
         END IF

*  Store the GRID co-ordinates of the edge and centre of the first
*  element.  This is defined to be (0.5,0.5,,...) and (1.0,1.0,...),
*  respectively.  This position will be mapped into the current Frame,
*  to find the edge and centre co-ordinates of the first pixel.
         DO IAXIS = 1, NDIM
            GFIRST( 1, IAXIS ) = 0.5D0
            GFIRST( 2, IAXIS ) = 1.0D0
         END DO

*  Extract the Mapping from GRID Frame to Current Frame.
         MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Map the GRID co-ordinates at the centre of the first pixel to obtain
*  the corresponding co-ordinates in the Frame.
         CALL AST_TRANN( MAP, 2, NDIM, 2, GFIRST, .TRUE., FRMNAX,
     :                   2, CFIRST, STATUS )

         DO IAXIS = AXLOW, AXHIGH
            CSIGN = SIGN( 0.5D0,
     :                    CFIRST( 2, IAXIS ) - CFIRST( 1, IAXIS ) )

*  Set the "have" co-ordinate.  CIRST( 1, * ) contains the centre of
*  each axis in the uncompressed array.  To move to the edge
*  offset by the differential half widths of the array elements.
            IF ( ALIGN( 1:1 ) .EQ. 'M' ) THEN
               HAVECO = CFIRST( 2, IAXIS )
            ELSE
               HAVECO = CFIRST( 2, IAXIS ) + CSIGN * NEWSCL( IAXIS )
            END IF

*  Find the fraction of a new-scaled channel of the "have" co-ordinate.
            NOCHAN = HAVECO / NEWSCL( IAXIS )
            FSHIFT = NOCHAN - INT( NOCHAN )
            ASIGN = SIGN( 1.0D0, FSHIFT )

*  Set the desired or "want" co-ordinate.
            WANTCO = INT( NOCHAN + 0.5D0 ) * NEWSCL( IAXIS )

*  Pick the smaller shift.
            SHIFT( IAXIS ) = HAVECO - WANTCO
            IF ( ABS( SHIFT( IAXIS ) ) .GT. 0.5 * NEWSCL( IAXIS ) ) THEN
               WANTCO = WANTCO + ASIGN * NEWSCL( IAXIS )
               SHIFT( IAXIS ) = HAVECO - WANTCO
            END IF
         END DO

      END IF

*  Tidy up.
*  =======

      END
