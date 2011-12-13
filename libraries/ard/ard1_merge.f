      SUBROUTINE ARD1_MERGE( UWCS, AWCS, DLBND, DUBND, MAP, IWCS,
     :                       WCSDAT, STATUS )
*+
*  Name:
*     ARD1_STAT

*  Purpose:

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_MERGE( UWCS, AWCS, DLBND, DUBND, MAP, IWCS, WCSDAT, STATUS )

*  Description:

*  Arguments:
*     UWCS = INTEGER (Given)
*        A pointer to an AST FrameSet supplied by the user. This should at
*        least have a Frame with Domain ARDAPP referring to "Application
*        co-ordinates". The current Frame in this FrameSet should refer to
*        "User co-ordinates" (i.e. the coord system in which positions are
*        supplied in the ARD description).
*     AWCS = INTEGER (Given)
*        A pointer to an AST FrameSet supplied by the application. This
*        should have a Base Frame referring to pixel coords within the
*        pixel mask and another Frame with Domain ARDAPP referring to
*        "Application co-ordinates".
*     DLBND( * ) = DOUBLE PRECISION (Given)
*        The lower bounds of pixel coordinates.
*     DUBND( * ) = DOUBLE PRECISION (Given)
*        The upper bounds of pixel coordinates.
*     MAP = INTEGER (Returned)
*        A pointer to an AST Mapping from the pixel coords in the mask,
*        to the coordinate system in which positions are specified in the
*        ARD expression. This is obtained by merging UWCS and AWCS,
*        aligning them in a suitable common Frame.
*     IWCS = INTEGER (Returned)
*        Returned equal to AST__NULL if the pixel->user mapping is linear.
*        Otherwise, it is returned holding a pointer to an AST FrameSet
*        containing just two Frames, the Base frame is pixel coords, the
*        current Frame is user coords.
*     WCSDAT( * ) = DOUBLE PRECISION (Returned)
*        Returned holding information which qualifies IWCS. If IWCS is
*        AST__NULL, then WCSDAT holds the coefficiets of the linear mapping
*        from pixel to user coords. Otherwise, wcsdat(1) holds a lower
*        limit on the distance (within the user coords) per pixel, and
*        the other elements in WCSDAT are not used. The supplied array
*        should have at least NDIM*(NDIM+1) elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-FEB-1994 (DSB):
*        Original version.
*     5-JUN-2001 (DSB):
*        Modified to use AST instead of linear coeffs.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST_ constants and functions

*  Arguments Given:
      INTEGER UWCS
      INTEGER AWCS
      DOUBLE PRECISION DLBND( * )
      DOUBLE PRECISION DUBND( * )

*  Arguments Returned:
      INTEGER MAP
      INTEGER IWCS
      DOUBLE PRECISION WCSDAT( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL LINEAR             ! Is MAP linear?

*.

      MAP = AST__NULL
      IWCS = AST__NULL

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialize the returned FrameSet to be a copy of the application
*  FrameSet.
      IWCS = AST_COPY( AWCS, STATUS )

*  Merge in the Frames form the user FrameSet, aligning them in some
*  suitable Frame.
      CALL ARD1_ASMRG( IWCS, UWCS, STATUS )

*  Get the Simplified Mapping from pixel to user coords.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IWCS, AST__BASE,
     :                                    AST__CURRENT, STATUS ),
     :                    STATUS )

*  See if it is linear.
      CALL ARD1_LINMP( MAP, AST_GETFRAME( UWCS, AST__CURRENT, STATUS ),
     :                 DLBND, DUBND, LINEAR, WCSDAT, STATUS )

*  If so, annul the FrameSet.
      IF( LINEAR ) CALL AST_ANNUL( IWCS, STATUS )


      END
