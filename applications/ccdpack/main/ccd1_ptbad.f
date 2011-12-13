      SUBROUTINE CCD1_PTBAD( TYPE, LBND, UBND, NY, I1, I2, DATA,
     :                       STATUS )
*+
*  Name:
*     CCD1_PTBAD

*  Purpose:
*     Fill selected columns of an image with bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_PTBAD( TYPE, LBND, UBND, NY, I1, I2, DATA, STATUS )

*  Description:
*     The routine fills a specified range of columns in an array
*     (regarded as a 2-dimensional image) with the "bad" value.  Other
*     elements of the array are left unchanged.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        Data type of the array to be filled: '_REAL' or '_DOUBLE' (in
*        upper case). The routine will return without action if one of
*        these two strings is not given.
*     LBND = INTEGER (Given)
*        Lower bound of the first dimension of the array.
*     UBND = INTEGER (Given)
*        Upper bound of the first dimension of the array.
*     NY = INTEGER (Given)
*        Second dimension size of the array.
*     I1 = INTEGER (Given)
*        Lower bound on the range of columns to be filled (must lie in
*        the range LBND to UBND inclusive).
*     I2 = INTEGER (Given)
*        Upper bound on the range of columns to be filled (must lie in
*        the range LBND to UBND inclusive and be at least equal to I1).
*     DATA = INTEGER (Given)
*        Pointer to the array to be filled. The region of the array
*        affected is (I1:I2,1:NY) - other elements are returned
*        unchanged. The dimensions of the array should be
*        (LBND:UBND,NY) and its type must match that specified via the
*        TYPE argument.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     8-JUN-1992 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER LBND
      INTEGER UBND
      INTEGER NY
      INTEGER I1
      INTEGER I2

*  Arguments Given and Returned:
      INTEGER DATA

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test for each recognised data type and call the appropriate routine
*  to do the work.
      IF ( TYPE .EQ. '_REAL' ) THEN
         CALL CCD1_PTBDR( LBND, UBND, NY, I1, I2,
     :                    %VAL( CNF_PVAL( DATA ) ), STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
         CALL CCD1_PTBDD( LBND, UBND, NY, I1, I2,
     :                    %VAL( CNF_PVAL( DATA ) ), STATUS )
      END IF

      END
* $Id$
