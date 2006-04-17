      SUBROUTINE KPG1_PX2AX( NDIM, PX, INDF, AX, STATUS )
*+
*  Name:
*     KPG1_PX2AX

*  Purpose:
*     Convert a pixel's indices into axis coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PX2AX( NDIM, PX, INDF, AX, STATUS )

*  Description:
*     This routine converts the pixel indices of an NDF pixel into the
*     axis coordinate values of the pixel's centre. If an axis
*     coordinate system is not defined for the NDF, then the pixel
*     coordinate system will be used instead.

*  Arguments:
*     NDIM = INTEGER (Given)
*        Number of NDF dimensions.
*     PX( NDIM ) = INTEGER (Given)
*        Indices of the NDF's pixel.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     AX( NDIM ) = DOUBLE PRECISION (Returned)
*        Axis coordinate values for the pixel's centre.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine is simplified by handling only a single pixel. It
*     will not be efficient enough to handle arrays of pixels.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     28-MAR-1991 (RFWS):
*        Original version.
*     2004 September 1 (TIMJ):
*        Use CNF_PVAL
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

*  Arguments Given:
      INTEGER NDIM
      INTEGER PX( NDIM )
      INTEGER INDF

*  Arguments Returned:
      DOUBLE PRECISION AX( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER EL                 ! Number of array elements mapped
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IERR               ! Error location (dummy)
      INTEGER INDFS              ! Identifier for NDF section
      INTEGER LBND( NDF__MXDIM ) ! Lower bounds of NDF section
      INTEGER NERR               ! Error count (dummy)
      INTEGER PNTR( 1 )          ! Pointer to mapped array
      INTEGER UBND( NDF__MXDIM ) ! Upper bounds of NDF section

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the lower and upper bounds of an NDF section containing the
*  pixel to be converted.
      DO 1 I = 1, NDIM
         LBND( I ) = PX( I )
         UBND( I ) = PX( I )
 1    CONTINUE

*  Create the NDF section.
      CALL NDF_SECT( INDF, NDIM, LBND, UBND, INDFS, STATUS )

*  Loop to convert each pixel index.
      DO 2 I = 1, NDIM

*  Map the appropriate NDF section axis centre array, giving one mapped
*  element.
         CALL NDF_AMAP( INDFS, 'Centre', I, '_DOUBLE', 'READ', PNTR,
     :                  EL, STATUS )

*  Extract the element's value and unmap the array.
         CALL VEC_DTOD( .FALSE., 1, %VAL( CNF_PVAL( PNTR( 1 ) ) ), 
     :                  AX( I ), IERR,
     :                  NERR, STATUS )
         CALL NDF_AUNMP( INDFS, 'Centre', I, STATUS )
 2    CONTINUE

*  Annul the NDF section identifier.
      CALL NDF_ANNUL( INDFS, STATUS )

      END
