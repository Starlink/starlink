      SUBROUTINE KPS1_GRDSC( IWCS, AT, PIXSC, NEWSC, FACTS, STATUS )
*+
*  Name:
*     KPS1_GRDSC

*  Purpose:
*     Determine new pixel scales at a given grid position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_GRDSC( IWCS, AT, PIXSC, NEWSC, FACTS, STATUS )

*  Description:
*     This routine determines the expansion factors to apply to each GRID
*     axis in order to achieve a given increase in WCS axis pixel scales.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet.
*     AT( * ) = DOUBLE PRECISION (Given)
*        The position in GRID coords at which the pixel scales are to be
*        determined. Note, the pixel scales may vary across the data
*        array if the WCS Mappings are non-linear. The array should have
*        one element for each GRID axis.
*     PIXSC( * ) = DOUBLE PRECISION (Given)
*        The original pixel scales, as returned by KPG1_PIXSC. One
*        element for each WCS axis.
*     NEWSC( * ) = DOUBLE PRECISION (Given)
*        The new pixel scales. One element for each WCS axis.
*     FACTS( * ) = DOUBLE PRECISION (Returned)
*        The expansion factors for each GRID axis. This array should have
*        one element for each grid axis in the NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-MAY-2007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER IWCS
      DOUBLE PRECISION AT( * )
      DOUBLE PRECISION PIXSC( * )
      DOUBLE PRECISION NEWSC( * )

*  Arguments Returned:
      DOUBLE PRECISION FACTS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AXDIST
      DOUBLE PRECISION IN( 2, NDF__MXDIM )
      DOUBLE PRECISION OUT( 2, NDF__MXDIM )
      INTEGER FGRID
      INTEGER FWCS
      INTEGER I
      INTEGER MAP
      INTEGER NPIX
      INTEGER NWCS
      INTEGER OUTAX( NDF__MXDIM )
      INTEGER OMAP
      LOGICAL OK
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the number of grid axes.
      NPIX = AST_GETI( IWCS, 'Nin', STATUS )

*  Get the number of WCS axes.
      NWCS = AST_GETI( IWCS, 'Nout', STATUS )

*  Get pointers to the base and current Frames, and the base to current
*  Mapping.
      FGRID = AST_GETFRAME( IWCS, AST__BASE, STATUS )
      FWCS = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      MAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Check the Mapping has an inverse transformation.
      IF( AST_GETL( MAP, 'TranInverse', STATUS ) ) THEN

*  Store the supplied AT position and another point that is 1 pixel away
*  from AT along each grid axis.
         DO I = 1, NPIX
            IN( 1, I ) = AT( I )
            IN( 2, I ) = AT( I ) + 1.0
         END DO

*  Transform them into WCS coords.
         CALL AST_TRANN( MAP, 2, NPIX, 2, IN, .TRUE., NWCS, 2, OUT,
     :                   STATUS )

*  Apply the required scaling to each WCS axis. This modifies the second
*  of the about points.
         DO I = 1, NWCS

*  Get the increment between the two points on the current WCS axis.
            AXDIST = AST_AXDISTANCE( FWCS, I, OUT( 1, I ), OUT( 2, I ),
     :                               STATUS )

*  Scale this distance by the required factor.
            AXDIST = AXDIST*PIXSC( I )/NEWSC( I )

*  Modify the second position by this amount.
            OUT( 2, I ) = AST_AXOFFSET( FWCS, I, OUT( 1, I ), AXDIST,
     :                                  STATUS )
         END DO

*  Transform the modified positions back into GRID coords.
         CALL AST_TRANN( MAP, 2, NWCS, 2, OUT, .FALSE., NPIX, 2, IN,
     :                   STATUS )

*  Find the expansion factors on each grid axis.
         DO I = 1, NPIX
            FACTS( I ) = ABS( IN( 2, I ) - IN( 1, I ) )
         END DO

*  If the Mapping is missing an inverse transformation, we may still be
*  able to deal with WCS axes that correspond with a single pixel axis.
      ELSE

*  Invert the Mapping so that we can split it by WCS axis.
         CALL AST_INVERT( MAP, STATUS )

*  Do each WCS axis.
         DO I = 1, NWCS
            OK = .FALSE.

*  Attempt to split off the current WCS axis from the Mapping
            CALL AST_MAPSPLIT( MAP, 1, I, OUTAX, OMAP, STATUS )
            IF( OMAP .NE. AST__NULL ) then
               IF( AST_GETI( OMAP, 'Nout', STATUS ) .EQ. 1 ) THEN

*  Set the factor for the corresponding grid axis.
                  FACTS( OUTAX( 1 ) ) = PIXSC( I )/NEWSC( I )
                  OK = .TRUE.

              END IF
           END IF

           IF( .NOT. OK .AND. STATUS .EQ. SAI__OK ) THEN
              STATUS = SAI__ERROR
              CALL MSG_SETI( 'I', I )
              CALL ERR_REP( ' ','Cannot determine new pixel scale for'//
     :                      ' WCS axis ^I.', STATUS )
              CALL ERR_REP( ' ','Try using a different value for '//
     :                      'parameter MODE.', STATUS )
              GO TO 999
           END IF

        END DO

      END IF

*  Arrive here if an error occurs.
 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

      END
