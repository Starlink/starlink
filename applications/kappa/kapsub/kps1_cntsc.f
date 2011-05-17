      SUBROUTINE KPS1_CNTSC( INDF, IPLOT, IGRID, SDIM, SLBND, SUBND,
     :                       INDFS, STATUS )
*+
*  Name:
*     KPS1_CNTSC

*  Purpose:
*     Return an identifier for the visible section of the NDF being
*     contoured.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_CNTSC( INDF, IPLOT, IGRID, SDIM, SLBND, SUBND, INDFS, STATUS )

*  Description:
*     This routine returns an NDF identifier for the section of the
*     supplied NDF which is visible within the current PGPLOT viewport.

*  Arguments:
*     INDF = INTEGER (Given)
*        The supplied NDF.
*     IPLOT = INTEGER (Given)
*        An AST pointer to a Plot. The Base Frame should correspond to
*        the current PGPLOT viewport and window.
*     IGRID = INTEGER (Given)
*        The index of the Frame within IPLOT which describes GRID
*        co-ordinates in the supplied NDF. This Frame is re-mapped
*        on exit, so that it refers to GRID coordinats within the
*        returned section.
*     SDIM( 2 ) = INTEGER (Given)
*        The indices of the two significant axes in the supplied NDF>
*     SLBND( 2 ) = INTEGER (Given and Returned)
*        Supplied holding the lower pixel index bounds on the two
*        significant axes in the supplied NDF. Returned holding the
*        lower pixel index bounds on the two significant axes in the
*        returned NDF section.
*     SUBND( 2 ) = INTEGER (Given and Returned)
*        Supplied holding the upper pixel index bounds on the two
*        significant axes in the supplied NDF. Returned holding the
*        upper pixel index bounds on the two significant axes in the
*        returned NDF section.
*     INDFS = INTEGER (Returned)
*        An NDF identifier for the visible section of the supplied NDF.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Copyright:
*     Copyright (C) 1998, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-AUG-1998 (DSB):
*        Original version.
*     9-FEB-2001 (DSB):
*        Added check that the section has some area.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER INDF
      INTEGER IPLOT
      INTEGER IGRID
      INTEGER SDIM( 2 )

*  Arguments Given and Returned:
      INTEGER SLBND( 2 )
      INTEGER SUBND( 2 )

*  Arguments Returned:
      INTEGER INDFS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER KPG1_CEIL          ! Smallest integer .GE. a given floating value
      INTEGER KPG1_FLOOR         ! Largest integer .LE. a given floating value

*  Local Variables:
      DOUBLE PRECISION GLBND( 2 )! Lower bounds of section in GRID Frame
      DOUBLE PRECISION GUBND( 2 )! Upper bounds of section in GRID Frame
      DOUBLE PRECISION INA( 2 )  ! OLD_GRID coords at point A
      DOUBLE PRECISION INB( 2 )  ! OLD_GRID coords at point B
      DOUBLE PRECISION LBNDG( 2 )! Lower bounds of section in GRAPHICS Frame
      DOUBLE PRECISION OUTA( 2 ) ! NEW_GRID coords at point A
      DOUBLE PRECISION OUTB( 2 ) ! NEW_GRID coords at point B
      DOUBLE PRECISION UBNDG( 2 )! Upper bounds of section in GRAPHICS Frame
      DOUBLE PRECISION XL( 2 )   ! GRAPHICS coords at lowest GRID position
      DOUBLE PRECISION XU( 2 )   ! GRAPHICS coords at highest GRID position
      INTEGER I                  ! Axis index
      INTEGER LBND( NDF__MXDIM ) ! Pixel index lower bounds of supplied NDF
      INTEGER MAP                ! Pointer to GRAPHICS -> GRID mapping
      INTEGER NDIM               ! No. of axes in supplied NDF
      INTEGER UBND( NDF__MXDIM ) ! Pixel index upper bounds of supplied NDF
      INTEGER WMAP               ! Pointer to OLD_GRID -> NEW_GRID mapping
      REAL PCLBND                ! Pixel coord lower bounds of section
      REAL PCUBND                ! Pixel coord upper bounds of section
      REAL X1                    ! Lower X bounds of section in GRAPHICS Frame
      REAL X2                    ! Upper X bounds of section in GRAPHICS Frame
      REAL Y1                    ! Lower Y bounds of section in GRAPHICS Frame
      REAL Y2                    ! Upper Y bounds of section in GRAPHICS Frame
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an ast context
      CALL AST_BEGIN( STATUS )

*  Get the bounds of the current PGPLOT window, and store double
*  precision equivalents.
      CALL PGQWIN( X1, X2, Y1, Y2 )
      LBNDG( 1 ) = DBLE( X1 )
      UBNDG( 1 ) = DBLE( X2 )
      LBNDG( 2 ) = DBLE( Y1 )
      UBNDG( 2 ) = DBLE( Y2 )

*  Get the pixel index bounds of the supplied NDF.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Get the Mapping from the Base (GRAPHICS) Frame in the Plot to the GRID
*  Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE, IGRID,
     :                                    STATUS ), STATUS )

*  Use this Mapping to determine the bounds of the PGPLOT window in
*  GRID coordinates on axis 1.
      CALL AST_MAPBOX( MAP, LBNDG, UBNDG, .TRUE., 1, GLBND( 1 ),
     :                 GUBND( 1 ), XL, XU, STATUS )

*  Convert these into pixel co-ordinate bounds.
      PCLBND = REAL( GLBND( 1 ) ) - 1.5 + REAL( SLBND( 1 ) )
      PCUBND = REAL( GUBND( 1 ) ) - 1.5 + REAL( SLBND( 1 ) )

*  Find the pixel index bounds of the NDF section, limit the bounds to the
*  dimensions of the supplied NDF. If the limits returned by AST_MAPBOX
*  look bad (e.g. if they are the wrong way round), retain the original
*  bounds.
      IF( PCUBND .GT. PCLBND ) THEN
         LBND( SDIM( 1 ) ) = MAX( SLBND( 1 ), KPG1_FLOOR( PCLBND ) + 1 )
         UBND( SDIM( 1 ) ) = MIN( SUBND( 1 ), KPG1_CEIL( PCUBND ) )
      END IF

*  Do the same for axis 2.
      CALL AST_MAPBOX( MAP, LBNDG, UBNDG, .TRUE., 2, GLBND( 2 ),
     :                 GUBND( 2 ), XL, XU, STATUS )

      PCLBND = REAL( GLBND( 2 ) ) - 1.5 + REAL( SLBND( 2 ) )
      PCUBND = REAL( GUBND( 2 ) ) - 1.5 + REAL( SLBND( 2 ) )

      IF( PCUBND .GT. PCLBND ) THEN
         LBND( SDIM( 2 ) ) = MAX( SLBND( 2 ), KPG1_FLOOR( PCLBND ) + 1 )
         UBND( SDIM( 2 ) ) = MIN( SUBND( 2 ), KPG1_CEIL( PCUBND ) )
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Check the section has some area.
      DO I = 1, NDIM
         IF( LBND( I ) .GT. UBND( I ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'KPS1_CNTSC_ERR1', 'There is no overlap '//
     :                    'with the existing plot.', STATUS )
            GO TO 999
         END IF
      END DO

*  Extract the required section from the NDF.
      CALL NDF_SECT( INDF, NDIM, LBND, UBND, INDFS, STATUS )

*  Create a WinMap which maps GRID co-ordinates in the supplied NDF into
*  GRID coordinates in the NDF section. Point "A" is the centre of the
*  lower right pixel in the section, point "B" is one pixel to the left
*  and one pixel up from point "A".
      INA( 1 ) = DBLE( LBND( SDIM( 1 ) ) - SLBND( 1 ) ) + 0.5D0
      INA( 2 ) = DBLE( LBND( SDIM( 2 ) ) - SLBND( 2 ) ) + 0.5D0
      INB( 1 ) = INA( 1 ) + 1.0D0
      INB( 2 ) = INA( 2 ) + 1.0D0

      OUTA( 1 ) = 0.5D0
      OUTA( 2 ) = 0.5D0
      OUTB( 1 ) = 1.5D0
      OUTB( 2 ) = 1.5D0

      WMAP = AST_WINMAP( 2, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Re-map the GRID Frame in the supplied Plot using this WinMap.
      CALL AST_REMAPFRAME( IPLOT, IGRID, WMAP, STATUS )

*  Return the bounds of the section.
      SLBND( 1 ) = LBND( SDIM( 1 ) )
      SUBND( 1 ) = UBND( SDIM( 1 ) )
      SLBND( 2 ) = LBND( SDIM( 2 ) )
      SUBND( 2 ) = UBND( SDIM( 2 ) )

 999  CONTINUE

*  End the ast context
      CALL AST_END( STATUS )

      END
