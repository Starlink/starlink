      SUBROUTINE KPG1_PX2AX8( NDIM, PX, INDF, AX, STATUS )
*+
*  Name:
*     KPG1_PX2AX8

*  Purpose:
*     Converts a pixel's indices into WCS or axis co-ordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PX2AX8( NDIM, PX, INDF, AX, STATUS )

*  Description:
*     This routine converts the pixel indices of an NDF pixel into the
*     WCS co-ordinate values of the pixel's centre. If a WCS FrameSet is
*     not available, then the pixel indices are converted into the axis
*     co-ordinate system. If an axis co-ordinate system is not defined for
*     the NDF, then the pixel co-ordinate system will be used instead.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     PX( NDIM ) = INTEGER*8 (Given)
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
*     5-DEC-2019 (DSB):
*        Support huge files.
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
      INTEGER*8 PX( NDIM )
      INTEGER INDF

*  Arguments Returned:
      DOUBLE PRECISION AX( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION IN( NDF__MXDIM )! Input PIXEL coords
      INTEGER*8 EL               ! Number of array elements mapped
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IERR               ! Error location (dummy)
      INTEGER INDFS              ! Identifier for NDF section
      INTEGER IWCS               ! Identifier for WCS FrameSet
      INTEGER*8 LBND( NDF__MXDIM ) ! Lower bounds of NDF section
      INTEGER NERR               ! Error count (dummy)
      INTEGER PIXFRM             ! Index of PIXEL Frame
      INTEGER PNTR( 1 )          ! Pointer to mapped array
      INTEGER*8 UBND( NDF__MXDIM ) ! Upper bounds of NDF section
      LOGICAL GOTWCS             ! Does the NDF have a WCS FrameSet?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the NDF has a defined WCS component, use it in preference to the
*  AXIS component.
      CALL NDF_STATE( INDF, 'WCS', GOTWCS, STATUS )
      IF( GOTWCS ) THEN

*  Get the WCS FrameSet.
         CALL NDF_GTWCS( INDF, IWCS, STATUS )

*  Find the PIXEL Frame, and make it the base Frame.
         CALL KPG1_ASFFR( IWCS, 'PIXEL', PIXFRM, STATUS )
         CALL AST_SETI( IWCS, 'Base', PIXFRM, STATUS )

*  Use the FrameSet to transform the supplied pixel position into the
*  current WCS Frame.
         DO I = 1, NDIM
            IN( I ) = DBLE( PX( I ) ) - 0.5D0
         END DO
         CALL AST_TRANN( IWCS, 1, NDIM, 1, IN, .TRUE.,
     :                   AST_GETI( IWCS, 'Nout', STATUS ), 1, AX,
     :                   STATUS )

*  Free resources
         CALL AST_ANNUL( IWCS, STATUS )

*  If no WCS FrameSet was available, use AXIS components.
      ELSE

*  Set up the lower and upper bounds of an NDF section containing the
*  pixel to be converted.
         DO 1 I = 1, NDIM
            LBND( I ) = PX( I )
            UBND( I ) = PX( I )
 1       CONTINUE

*  Create the NDF section.
         CALL NDF_SECT8( INDF, NDIM, LBND, UBND, INDFS, STATUS )

*  Loop to convert each pixel index.
         DO 2 I = 1, NDIM

*  Map the appropriate NDF section axis centre array, giving one mapped
*  element.
            CALL NDF_AMAP8( INDFS, 'Centre', I, '_DOUBLE', 'READ', PNTR,
     :                      EL, STATUS )

*  Extract the element's value and unmap the array.
            CALL VEC_DTOD( .FALSE., 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ),
     :                     AX( I ), IERR,
     :                     NERR, STATUS )
            CALL NDF_AUNMP( INDFS, 'Centre', I, STATUS )
 2       CONTINUE

*  Annul the NDF section identifier.
         CALL NDF_ANNUL( INDFS, STATUS )
      END IF

      END
