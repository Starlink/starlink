      SUBROUTINE ARY_STYPE( FTYPE, IARY, STATUS )
*+
*  Name:
*     ARY_STYPE

*  Purpose:
*     Set a new type for an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_STYPE( FTYPE, IARY, STATUS )

*  Description:
*     The routine sets a new full type for an array, causing its data
*     storage type to be changed. If the array's pixel values are
*     defined, then they will be converted from the old type to the new
*     one.  If they are undefined, then no conversion will be
*     necessary.  Subsequent enquiries will reflect the new type.
*     Conversion may be performed between any types supported by the
*     ARY_ routines, including from a non-complex type to a complex
*     type (and vice versa).

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The new full type specification for the array (e.g.  '_REAL'
*        or 'COMPLEX_INTEGER').
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine may only be used to change the type of a base
*     array. If it is called with an array which is not a base array,
*     then it will return without action. No error will result.
*     -  An error will result if the array, or any part of it, is
*     currently mapped for access (e.g. through another identifier).
*     -  If the type of an array is to be changed without its pixel
*     values being retained, then a call to ARY_RESET should be made
*     beforehand. This will avoid the cost of converting all the
*     values.

*  Algorithm:
*     -  Check the full type specification for validity.
*     -  Import the array identifier.
*     -  Set the new type for the array.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1989 (RFWS):
*        Original version.
*     15-SEP-1989 (RFWS):
*        Added check that TYPE access is available.
*     24-JAN-1990 (RFWS):
*        Renamed from ARY_SFTYP tp ARY_STYPE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Arguments Given:
      CHARACTER * ( * ) FTYPE
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( ARY__SZTYP ) TYPE ! Array numeric type
      INTEGER IACB               ! Index to array entry in the ACB
      LOGICAL CMPLX              ! Whether the array is to be complex

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check the full type for validity.
      CALL ARY1_VFTP( FTYPE, TYPE, CMPLX, STATUS )

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )

*  Check that TYPE access to the array is available.
      CALL ARY1_CHACC( IACB, 'TYPE', STATUS )

*  Set the new array type.
      CALL ARY1_STP( TYPE, CMPLX, IACB, STATUS )

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_STYPE_ERR',
     :   'ARY_STYPE: Error setting new (full) type for an array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_STYPE', STATUS )
      END IF

      END
