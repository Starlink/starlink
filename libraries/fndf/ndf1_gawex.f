      SUBROUTINE NDF1_GAWEX( LBND, UBND, IARY, UPPER, WIDTH, STATUS )
*+
*  Name:
*     NDF1_GAWEX

*  Purpose:
*     Get an extrapolation value for an NDF axis width array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GAWEX( LBND, UBND, IARY, UPPER, WIDTH, STATUS )

*  Description:
*     The routine obtains a value required to extrapolate an NDF's axis
*     width array beyond the lower or upper pixel-index limits of the
*     NDF. The width of the nearest available axis element is used.

*  Arguments:
*     LBND = INTEGER (Given)
*        Lower pixel-index bound of the axis width array.
*     UBND = INTEGER (Given)
*        Upper pixel-index bound of the axis width array.
*     IARY = INTEGER (Given)
*        ARY_ system identifier for the (1-dimensional) axis width
*        array.
*     UPPER = LOGICAL (Given)
*        Whether extrapolation to higher pixel indices is required. If
*        not, then extrapolation to lower pixel indices is assumed.
*     WIDTH = DOUBLE PRECISION (Returned)
*        Width value to use for extrapolation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Determine the pixel-index bounds of a section containing the
*     nearest pixel to be used for extrapolation.
*     -  Create a section containing the pixel of interest and map it
*     for reading with a numeric type of _DOUBLE.
*     -  Copy the mapped pixel value to a double precision array so
*     that its value can be accessed.
*     -  Annul the array section (which also unmaps it).
*     -  Return the extracted width value.

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
*     9-NOV-1990 (RFWS):
*        Original version, derived from the equivalent axis data array
*        routine.
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
      DOUBLE PRECISION WIDTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION AVAL( 1 ) ! Array pixel value
      INTEGER EL                 ! Number of mapped pixel values
      INTEGER IARYS              ! ARY_ identifier for section
      INTEGER IERR               ! Error position (dummy)
      INTEGER L( 1 )             ! Lower section bound
      INTEGER NERR               ! Error count (dummy)
      INTEGER PNTR               ! Pointer to mapped pixel value
      INTEGER U( 1 )             ! Upper section bound

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the pixel-index bounds of a section containing the nearest
*  pixel to be used for extrapolation.
      IF ( UPPER ) THEN
         L( 1 ) = UBND
         U( 1 ) = UBND
      ELSE
         L( 1 ) = LBND
         U( 1 ) = LBND
      END IF

*  Create a section containing the pixel of interest and map it for
*  reading with a numeric type of _DOUBLE.
      CALL ARY_SECT( IARY, 1, L, U, IARYS, STATUS )
      CALL ARY_MAP( IARYS, '_DOUBLE', 'READ', PNTR, EL, STATUS )

*  Copy the mapped pixel value to a double precision array so that its
*  value can be accessed.
      CALL VEC_DTOD( .FALSE., EL, %VAL( CNF_PVAL( PNTR ) ), AVAL, IERR,
     :               NERR, STATUS )

*  Annul the array section (which also unmaps it).
      CALL ARY_ANNUL( IARYS, STATUS )

*  Return the extracted width value.
      IF ( STATUS .EQ. SAI__OK ) THEN
         WIDTH = AVAL( 1 )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_GAWEX', STATUS )

      END
