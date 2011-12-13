      SUBROUTINE NDF1_GADEX( LBND, UBND, IARY, UPPER, SCALE, ZERO,
     :                       STATUS )
*+
*  Name:
*     NDF1_GADEX

*  Purpose:
*     Get extrapolation parameters for an NDF axis data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GADEX( LBND, UBND, IARY, UPPER, SCALE, ZERO, STATUS )

*  Description:
*     The routine obtains the parameters required to extrapolate an
*     NDF's axis data array beyond the lower or upper pixel-index
*     limits of the NDF. A linear extrapolation is used, taking the
*     difference between the nearest two existing pixels to define the
*     gradient of the extrapolated line.

*  Arguments:
*     LBND = INTEGER (Given)
*        Lower pixel-index bound of the axis data array.
*     UBND = INTEGER (Given)
*        Upper pixel-index bound of the axis data array.
*     IARY = INTEGER (Given)
*        ARY_ system identifier for the (1-dimensional) axis data
*        array.
*     UPPER = LOGICAL (Given)
*        Whether extrapolation to higher pixel indices is required. If
*        not, then extrapolation to lower pixel indices is assumed.
*     SCALE = DOUBLE PRECISION (Returned)
*        Extrapolation scale factor. The extrapolated axis value AVAL
*        is related to the axis array's pixel index I by AVAL = I *
*        SCALE + ZERO.
*     ZERO = DOUBLE PRECISION (Returned)
*        Extrapolation zero point (see formula above).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the existing axis data array contains only a single pixel,
*     then a SCALE value of 1.0D0 will be returned.

*  Algorithm:
*     -  Determine the indices of the two nearest pixels to be used for
*     extrapolation, ensuring that they lie within the axis array.
*     -  Create a section containing the pixels of interest and map it
*     for reading with a numeric type of _DOUBLE.
*     -  Copy the mapped pixel values to a double precision array so
*     their values can be accessed.
*     -  Annul the array section (which also unmaps it).
*     -  Derive the extrapolation scale factor and zero point.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     10-JUL-1990 (RFWS):
*        Original version.
*     9-NOV-1990 (RFWS):
*        Changed the routine name.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      INTEGER IARY
      LOGICAL UPPER

*  Arguments Returned:
      DOUBLE PRECISION SCALE
      DOUBLE PRECISION ZERO

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL( 2 ) ! Array pixel values
      INTEGER EL                 ! Number of mapped pixel values
      INTEGER IARYS              ! ARY_ identifier for section
      INTEGER IERR               ! Error position (dummy)
      INTEGER L( 1 )             ! Index of first extrapolation pixel
      INTEGER NERR               ! Error count (dummy)
      INTEGER PNTR               ! Pointer to mapped pixel values
      INTEGER U( 1 )             ! Index of second extrapolation pixel

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the indices of the two nearest pixels to be used for
*  extrapolation, ensuring that they lie within the axis array.
      IF ( UPPER ) THEN
         L( 1 ) = MAX( LBND, UBND - 1 )
         U( 1 ) = UBND
      ELSE
         L( 1 ) = LBND
         U( 1 ) = MIN( UBND, LBND + 1 )
      END IF

*  Create a section containing the pixels of interest and map it for
*  reading with a numeric type of _DOUBLE.
      CALL ARY_SECT( IARY, 1, L, U, IARYS, STATUS )
      CALL ARY_MAP( IARYS, '_DOUBLE', 'READ', PNTR, EL, STATUS )

*  Copy the mapped pixel values to a double precision array so their
*  values can be accessed.
      CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( PNTR ) ), AVAL, IERR,
     :               NERR, STATUS )

*  Annul the array section (which also unmaps it).
      CALL ARY_ANNUL( IARYS, STATUS )

*  Derive the extrapolation scale factor and zero point.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( EL .LE. 1 ) THEN
            SCALE = 1.0D0
         ELSE
            SCALE = AVAL( 2 ) - AVAL( 1 )
         END IF
         ZERO = AVAL( 1 ) - SCALE * DBLE( L( 1 ) )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_GADEX', STATUS )

      END
