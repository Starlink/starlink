      SUBROUTINE KPG1_ASPCL( IPLOT, LBND, UBND, STATUS )
*+
*  Name:
*     KPG1_ASPCL

*  Purpose:
*     Applies clipping to a Plot so that border and co-ordinate grid are
*     restricted to the well bahaved regions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPCL( IPLOT, LBND, UBND, STATUS )

*  Description:
*     This routine imposes clipping on a Plot (using the AST_CLIP routine)
*     that restricts drawing to the regions that seem well bahaved.
*     Specifically, drawing is restricted to a rectangular region within
*     the base Frame of the Plot that is determined by transforming the
*     supplied PIXEL bounding box into the current Frame, then converting
*     it to the base Frame.

*  Arguments:
*     IPLOT = INTEGER (Given)
*        An AST pointer to the Plot.
*     LBND( * ) = INTEGER (Given)
*        The lower pixel index bounds of the Plot in the PIXEL Frame.
*     UBND( * ) = INTEGER (Given)
*        The upper pixel index bounds of the Plot in the PIXEL Frame.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-2006 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER IPLOT
      INTEGER LBND( * )
      INTEGER UBND( * )

*  Status:
      INTEGER STATUS               ! Global status

*  Local Variables:
      DOUBLE PRECISION DLBND( NDF__MXDIM )
      DOUBLE PRECISION DUBND( NDF__MXDIM )
      DOUBLE PRECISION BLBND( NDF__MXDIM )
      DOUBLE PRECISION BUBND( NDF__MXDIM )
      DOUBLE PRECISION PLBND( NDF__MXDIM )
      DOUBLE PRECISION PUBND( NDF__MXDIM )
      DOUBLE PRECISION CLBND( NDF__MXDIM )
      DOUBLE PRECISION CUBND( NDF__MXDIM )
      DOUBLE PRECISION XL( NDF__MXDIM )
      DOUBLE PRECISION XU( NDF__MXDIM )
      INTEGER IPIX
      INTEGER MAP
      INTEGER NAXC
      INTEGER NAXP
      INTEGER I
      LOGICAL CLIP
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Find the index of the PIXEL Frame.
      CALL KPG1_ASFFR( IPLOT, 'PIXEL', IPIX, STATUS )
      IF( IPIX .NE. AST__NOFRAME ) THEN

*  Get the Mapping from PIXEL Frame to current Frame.
         MAP = AST_GETMAPPING( IPLOT, IPIX, AST__CURRENT, STATUS )

*  Get the number of pixel axes, and the number of current Frame axes
         NAXP = AST_GETI( MAP, 'Nin', STATUS )
         NAXC = AST_GETI( MAP, 'Nout', STATUS )

*  Convert the pixel index bounds to double precision pixel co-ordinate
*  bounds.
         DO I = 1, NAXP
            DLBND( I ) = DBLE( LBND( I ) ) - 1.0D0
            DUBND( I ) = DBLE( UBND( I ) )
         END DO

*  Find the bounding box in the current Frame.
         DO I = 1, NAXC
            CALL AST_MAPBOX( MAP, DLBND, DUBND, .TRUE., I, CLBND( I ),
     :                       CUBND( I ), XL, XU, STATUS )
         END DO

*  Indicate that we do not as yet have any need to apply any clipping.
         CLIP = .FALSE.

*  Transform this bounding box back into the PIXEL Frame.
         DO I = 1, NAXP
            CALL AST_MAPBOX( MAP, CLBND, CUBND, .FALSE., I, PLBND( I ),
     :                       PUBND( I ), XL, XU, STATUS )

*  Ensure it does not extend beyond the supplied bounds. Also note if
*  it is any smaller than the supplied bounds (we do not need to apply
*  any clipping otherwise).
            IF( PLBND( I ) .LT. DLBND( I ) ) THEN
               PLBND( I ) = DLBND( I )

            ELSE IF( PLBND( I ) .GT. DLBND( I ) ) THEN
               CLIP = .TRUE.

            END IF

            IF( PUBND( I ) .GT. DUBND( I ) ) THEN
               PUBND( I ) = DUBND( I )

            ELSE IF( PUBND( I ) .LT. DUBND( I ) ) THEN
               CLIP = .TRUE.

            END IF
         END DO

*  If any clipping is needed, convert it to the base Frame of the Plot and
*  then apply it.
         IF( CLIP ) THEN
            MAP = AST_GETMAPPING( IPLOT, IPIX, AST__BASE, STATUS )
            DO I = 1, 2
               CALL AST_MAPBOX( MAP, PLBND, PUBND, .TRUE., I,
     :                          BLBND( I ), BUBND( I ), XL, XU, STATUS )
            END DO
            CALL AST_CLIP( IPLOT, AST__BASE, BLBND, BUBND, STATUS )
         END IF
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

      END
