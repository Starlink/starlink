      SUBROUTINE SETORIGIN( STATUS )
*+
*  Name:
*     SETORIGIN

*  Purpose:
*     Sets a new pixel origin for an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETORIGIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application sets a new pixel origin value for an NDF data
*     structure.  The NDF is accessed in update mode and the indices of
*     the first pixel (the NDF's lower pixel-index bounds) are set to
*     specified integer values, which may be positive or negative.  No
*     other properties of the NDF are altered.  If required, a template
*     NDF may be supplied and the new origin values will be derived
*     from it.

*  Usage:
*     setorigin ndf origin

*  ADAM Parameters:
*     LIKE = NDF (Read)
*        This parameter may be used to supply an NDF which is to be
*        used as a template.  If such a template is supplied, then its
*        origin (its lower pixel-index bounds) will be used as the new
*        origin value for the NDF supplied via the NDF parameter.  By
*        default, no template will be used and the new origin will be
*        specified via the ORIGIN parameter. [!]
*     NDF = NDF (Read and Write)
*        The NDF data structure whose pixel origin is to be modified.
*     ORIGIN() = _INTEGER (Read)
*        A 1-dimensional array specifying the new pixel origin values,
*        one for each NDF dimension.

*  Examples:
*     setorigin image_2d [1,1]
*        Sets the indices of the first pixel in the 2-dimensional image
*        image_2d to be (1,1).  The image pixel values are unaltered.
*     setorigin ndf=starfield
*        A new pixel origin is set for the NDF structure called
*        starfield.  SETORIGIN will prompt for the new origin values,
*        supplying the existing values as defaults.
*     setorigin ndf=cube origin=[-128,-128]
*        Sets the pixel origin values for the first two dimensions of
*        the 3-dimensional NDF called cube to be (-128,-128).  A value
*        for the third dimension is not specified, so the origin of
*        this dimension will remain unchanged.
*     setorigin betapic like=alphapic
*        Sets the pixel origin of the NDF called betapic to be equal to
*        that of the NDF called alphapic.

*  Notes:
*     If the number of new pixel origin values is less than the number
*     of NDF dimensions, then the pixel origin of the extra dimensions
*     will remain unchanged.  If the number of values exceeds the number
*     of NDF dimensions, then the excess values will be ignored.

*  Related Applications:
*     KAPPA: SETBOUND.

*  Timing:
*     Setting a new pixel origin is a quick operation whose timing does
*     not depend on the size of the NDF.

*  Copyright:
*     Copyright (C) 1990-1991 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JUL-1990 (RFWS):
*        Original version.
*     4-JUL-1990 (RFWS):
*        Changed to allow dimensions whose new origin is not specified
*        to retain their original lower pixel-index bounds.
*     3-AUG-1990 (RFWS):
*        Corrected error in routine name in error message.
*     13-MAR-1991 (RFWS):
*        Added the LIKE parameter to permit the use of a template NDF.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Added Related Applications.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'PAR_ERR'          ! PAR_ error codes

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER LBND( NDF__MXDIM ) ! Lower pixel-index bounds
      INTEGER N                  ! Number of new origin values
      INTEGER NDF1               ! Identifier for NDF being modified
      INTEGER NDF2               ! Identifier for template NDF
      INTEGER NDIM               ! Number of NDF dimensions
      INTEGER ORIG( NDF__MXDIM ) ! New origin values
      INTEGER SHIFT( NDF__MXDIM ) ! Pixel-index shifts to apply
      INTEGER UBND( NDF__MXDIM ) ! Upper pixel-index bounds
      INTEGER UBNDL( NDF__MXDIM ) ! Upper bounds of template NDF
      LOGICAL LIKE               ! Template NDF provided?

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF to be modified and determine its pixel-index bounds.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF1, STATUS )
      CALL NDF_BOUND( NDF1, NDF__MXDIM, LBND, UBND, NDIM, STATUS )

*  Defer error reporting and attempt to obtain a second NDF to act as a
*  template.
      LIKE = .TRUE.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL LPG_ASSOC( 'LIKE', 'READ', NDF2, STATUS )

*  If a null NDF was specified, then annul the error and note that a
*  template was not provided.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            LIKE = .FALSE.
         END IF
         CALL ERR_RLSE
      END IF

*  If a template was provided, then obtain its bounds.  The lower
*  bounds will act as the new origin values.
      IF ( LIKE ) THEN
         CALL NDF_BOUND( NDF2, NDF__MXDIM, ORIG, UBNDL, N, STATUS )

*  Otherwise, set a default and obtain new origin values directly.
      ELSE
         CALL PAR_DEF1I( 'ORIGIN', NDIM, LBND, STATUS )
         CALL PAR_GET1I( 'ORIGIN', NDIM, ORIG, N, STATUS )
      END IF

*  Determine the pixel-index shifts required, defaulting any which were
*  not supplied to be zero.
      DO 1 I = 1, NDIM
         IF ( I .LE. N ) THEN
            SHIFT( I ) = ORIG( I ) - LBND( I )
         ELSE
            SHIFT( I ) = 0
         END IF
    1 CONTINUE

*  Apply the shifts.
      CALL NDF_SHIFT( NDIM, SHIFT, NDF1, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report contextual information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETORIGIN_ERR',
     :     'SETORIGIN: Error setting a new pixel origin for an NDF.',
     :     STATUS )
      END IF

      END
