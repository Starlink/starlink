      SUBROUTINE KPG1_PX2AX( NDIM, PX, INDF, AX, STATUS )
*+
*  Name:
*     KPG1_PX2AX

*  Purpose:
*     Converts a pixel's indices into WCS or axis co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PX2AX( NDIM, PX, INDF, AX, STATUS )

*  Description:
*     This routine converts the pixel indices of an NDF pixel into the
*     WCS co-ordinate values of the pixel's centre. If a WCS FrameSet is
*     not available, then the pixel indices are converted into the axis
*     co-ordinate system. If an axis co-ordinate system is not defined for
*     the NDF, then the pixel co-ordinate system will be used instead.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     PX( NDIM ) = INTEGER (Given)
*        Indices of the NDF's pixel.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     AX( * ) = DOUBLE PRECISION (Returned)
*        WCS or axis co-ordinate values for the pixel's centre.
*        There should be enough elements in this array for all the
*        current frame WCS axes (which need not be the same as NDIM).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is simplified by handling only a single pixel. It
*     will not be efficient enough to handle arrays of pixels.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: Davd Berry (JAC, UCLan)
*     {enter_new_authors_here}

*  History:
*     28-MAR-1991 (RFWS):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
*     18-MAY-2007 (RFWS):
*        Return WCS coords in preference to AXIS coords.
*     4-DEC-2019 (DSB):
*        Call KPG1_PX2AX8 to do the work.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'AST_PAR'          ! AST functions and constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER PX( NDIM )
      INTEGER INDF

*  Arguments Returned:
      DOUBLE PRECISION AX( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER*8 PX8( NDF__MXDIM )
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert the supplied 32 bit integers to 64 bit.
      DO I = 1, NDIM
         PX8( I ) = PX( i )
      END DO

*  Call the 64-bit version of this routine.
      CALL KPG1_PX2AX8( NDIM, PX8, INDF, AX, STATUS )

      END

