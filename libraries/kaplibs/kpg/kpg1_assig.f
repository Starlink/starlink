      SUBROUTINE KPG1_ASSIG( IWCS, NDIM, LBND, UBND, STATUS )
*+
*  Name:
*     KPG1_ASSIG

*  Purpose:
*     Ensures that the Current Frame from an NDF WCS FrameSet has no
*     insignificant axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASSIG( IWCS, NDIM, LBND, UBND, STATUS )

*  Description:
*     This routine looks for insignificant axes in the Current Frame of
*     the supplied WCS FrameSet (an axis is insignificant if all pixels
*     within the NDF pixel array has the same position on the axis). If
*     any insignificant axes are found, a new Frame is added to the
*     FrameSet containing only the significant axes from the Current Frame.
*     This new Frame is added into the FrameSet and becomes the new
*     Current Frame. The PermMap which connects it to the original
*     Current Frame assigns the correct constant values to the
*     insignificant axes whn used in the inverse direction.

*  Arguments:
*     IWCS = INTEGER (Given)
*        The WCS FrameSet from the NDF, as returned by KPG1_GTWCS.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the NDF, as returned by NDF_BOUND.
*     LBND( NDIM ) = INTEGER (Returned)
*        The lower pixel index bounds of the NDF, as returned by NDF_BOUND.
*     UBND( NDIM ) = INTEGER (Returned)
*        The upper pixel index bounds of the NDF, as returned by NDF_BOUND.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The routine KPG1_ASSIG8 is equivalent to this function but uses
*     INTEGER*8 for the LBND and UBND arguments.

*  Copyright:
*     Copyright (C) 1999, 2000, 2001 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUL-1999 (DSB):
*        Original version.
*     8-SEP-2000 (DSB):
*        Check for errors in AST_MAPBOX.
*     27-FEB-2001 (DSB):
*        Report an error if there are no significant axes.
*     4-OCT-2019 (DSB):
*        Changed to be a thin wrapper round KPG1_ASSIG.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants

*  Arguments Given:
      INTEGER IWCS
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER*8 LBND8( NDF__MXDIM )
      INTEGER*8 UBND8( NDF__MXDIM )
*.

*  Copy the supplied INTEGER values into local INTEGER*8 arrays.
      DO I = 1, NDIM
         LBND8( I ) = LBND( I )
         UBND8( I ) = UBND( I )
      END DO

*  Call the 8-byte version of this routine.
      CALL KPG1_ASSIG8( IWCS, NDIM, LBND8, UBND8, STATUS )

      END
