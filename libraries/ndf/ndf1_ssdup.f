      SUBROUTINE NDF1_SSDUP( IARY1, IARY2, IARY3, STATUS )
*+
*  Name:
*     NDF1_SSDUP

*  Purpose:
*     Duplicate an array section.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_SSDUP( IARY1, IARY2, IARY3, STATUS )

*  Description:
*     The routine creates a "similar section" from an array (whose ARY_
*     system identifier is supplied) using an existing array section as
*     a template.  The new array section will bear the same
*     relationship to its base array as the template does to its own
*     base array.
*
*     Note that this routine is the same as ARY_SSECT (q.v.), except
*     that the number of output section dimensions matches the template
*     array, rather than the original input array.

*  Arguments:
*     IARY1 = INTEGER (Given)
*        ARY_ system identifier for the array, or array section, from
*        which the new section is to be drawn.
*     IARY2= INTEGER (Given)
*        ARY_ system identifier for the template array section (may also
*        be a base array).
*     IARY3 = INTEGER (Returned)
*        ARY_ system identifier for the new array section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If this routine is called with STATUS set, then a value of
*     ARY__NOID will be returned for the IARY3 argument, although no
*     further processing will occur. The same value will also be
*     returned if the routine should fail for any reason.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-JAN-1999 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Arguments Given:
      INTEGER IARY1
      INTEGER IARY2
      INTEGER IARY3

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! Dimension sizes (junk array)
      INTEGER IDIM               ! Loop counter for dimensions
      INTEGER ITMP               ! Temporary ARY_ identifier
      INTEGER LBND1( NDF__MXDIM ) ! Lower bounds of array
      INTEGER NDIM1              ! Number of input array dimensions
      INTEGER NDIM2              ! Number of template array dimensions
      INTEGER UBND1( NDF__MXDIM ) ! Upper bounds of array
*.

*  Initialise the returned ARY_ identifier.
      IARY3 = ARY__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the bounds and number of dimensions of the first array.
      CALL ARY_BOUND( IARY1, NDF__MXDIM, LBND1, UBND1, NDIM1, STATUS )

*  Determine the number of dimensions in the second array.
      CALL ARY_DIM( IARY2, NDF__MXDIM, DIM, NDIM2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the numbers of dimensions match, we can simply select the
*  required section.
         IF ( NDIM1 .EQ. NDIM2 ) THEN
            CALL ARY_SSECT( IARY1, IARY2, IARY3, STATUS )

*  Otherwise, pad the bounds of the first array with ones to match the
*  number of dimensions in the second array.
         ELSE
            DO 1 IDIM = NDIM1 + 1, NDIM2
               LBND1( IDIM ) = 1
               UBND1( IDIM ) = 1
 1          CONTINUE

*  Create a temporary section from the first array with the original
*  bounds but the new number of dimensions.
            CALL ARY_SECT( IARY1, NDIM2, LBND1, UBND1, ITMP, STATUS )

*  From this, obtain the required section.
            CALL ARY_SSECT( ITMP, IARY2, IARY3, STATUS )

*  Annul the temporary section.
            CALL ARY_ANNUL( ITMP, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_SSDUP', STATUS )

      END
