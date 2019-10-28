      SUBROUTINE KPG1_ASTRM8( IWCS, DEFAX, LBND, UBND, WORK, STATUS )
*+
*  Name:
*     KPG1_ASTRM8

*  Purpose:
*     Trims axes from the current Frame of a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASTRM8( IWCS, DEFAX, LBND, UBND, WORK, STATUS )

*  Description:
*     This routine is equivalent to KPG1_ASTRM except that arguments
*     LBND and UBND are INTEGER*8 instead of INTEGER. See KPG1_ASTRM
*     for more information.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The FrameSet to use. A new current Frame may be added to the
*        FrameSet by this routine.
*     DEFAX( * ) = INTEGER (Given)
*        This array should have one element for each axis in the base
*        Frame of the supplied FrameSet. The i'th value is the index
*        within the original current Frame of the axis which is to be
*        associated with the i'th base Frame axis by default. Only used
*        if no better defaults can be found by splitting the FrameSet
*        Mapping.
*     LBND( * ) = INTEGER*8 (Given)
*        The lower pixel bound on each pixel axis. Array length should be
*        at least equal to the number of base Frame axes in IWCS.
*     UBND( * ) = INTEGER*8 (Given)
*        The upper pixel bound on each pixel axis. Array length should be
*        at least equal to the number of base Frame axes in IWCS.
*     WORK( * ) = INTEGER (Given)
*        Work space. It's length should be at least twice as large as the
*        largest pixel dimension implied by LBND and UBND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2019 East Asian Observatory
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
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2019 (DSB):
*        Original version, copied from KPG1_ASTRM and changed to use
*        INTEGER*8 bounds and dimensions.
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

*  Arguments Given:
      INTEGER IWCS
      INTEGER DEFAX( * )
      INTEGER*8 LBND( * )
      INTEGER*8 UBND( * )
      DOUBLE PRECISION WORK( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BCMAP
      INTEGER CURAXES( NDF__MXDIM )
      INTEGER I
      INTEGER MAP1
      INTEGER NDIM
      INTEGER NFC
      INTEGER PIXAXES( NDF__MXDIM )
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the base->current Mapping.
      BCMAP = AST_GETMAPPING( IWCS, AST__BASE, AST__CURRENT, STATUS )

*  Get the number of axes in the original current Frame.
      NFC = MIN( NDF__MXDIM, AST_GETI( BCMAP, 'NOUT', STATUS ) )

*  Get the number of base Frame (i.e. pixel) axes.
      NDIM = MIN( NDF__MXDIM, AST_GETI( BCMAP, 'NIN', STATUS ) )

*  If there are too many axes, we need to decide which axes to keep.
      IF( NFC .GT. NDIM ) THEN

*  Our first choice for default axes are those which are fed by the base
*  Frame axes. Find these axes now.
         DO I = 1, NDIM
            PIXAXES( I ) = I
         END DO
         CALL AST_MAPSPLIT( BCMAP, NDIM, PIXAXES, CURAXES, MAP1,
     :                      STATUS )

*  If this could not be done, use the defaults supplied in DEFAX.
         IF( MAP1 .NE. AST__NULL ) THEN
            IF( AST_GETI( MAP1, 'Nout', STATUS ) .NE. NDIM ) THEN
               CALL AST_ANNUL( MAP1, STATUS )
            END IF
         END IF

         IF( MAP1 .EQ. AST__NULL ) THEN
            DO I = 1, NDIM
               CURAXES( I ) = DEFAX( I )
            END DO
         END IF

*  Allow the user to select the current Frame axes to use, using the above
*  defaults.
         CALL KPG1_GTAXI( 'USEAXIS', IWCS, NDIM, CURAXES, STATUS )
      END IF

*  Call ATL_AXTRM to trim or ad axes as required.
      CALL ATL_AXTRM8( IWCS, CURAXES, LBND, UBND, WORK, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
