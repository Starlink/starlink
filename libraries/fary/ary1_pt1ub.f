      SUBROUTINE ARY1_PT1UB( BAD, EL, ARRAY, TYPE, LOC, DCE, STATUS )
*+
*  Name:
*     ARY1_PT1UB

*  Purpose:
*     Write a 1-dimensional array of UNSIGNED BYTE values to an HDS object.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_PT1UB( BAD, EL, ARRAY, TYPE, LOC, DCE, STATUS )

*  Description:
*     The routine writes a 1-dimensional array of UNSIGNED BYTE values to an
*     HDS object. The object must be 1-dimensional and must contain
*     exactly the number of elements to be written. Data type
*     conversion to any numeric data type is performed if necessary,
*     with optional testing for bad pixel values.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Whether to test for bad pixel values.
*     EL = INTEGER (Given)
*        Number of array elements to be written.
*     ARRAY( N ) = BYTE (Given)
*        The array of UNSIGNED BYTE values.
*     TYPE = CHARACTER * ( * ) (Given)
*        HDS data type of the object to be written. This must be in
*        upper case.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to 1-dimensional HDS object to receive the data.
*     DCE = LOGICAL (Returned)
*        Whether any data type conversion errors occurred (the affected
*        elements of the data object are set to bad values if this
*        happens).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Initialise.
*     -  If no data type conversion is required, then write the data
*     values directly.
*     -  If type conversion is required, then map the data object
*     without type conversion, perform the conversion explicitly and
*     unmap the data.

*  Copyright:
*     Copyright (C) 1989 Science & Engineering Research Council.
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
*     12-JUL-1989 (RFWS):
*        Original version.
*     18-SEP-1989 (RFWS):
*        Changed DAT_UNMAP call to ARY1_HUNMP to ensure unmapping under
*        error conditions.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants

*  Arguments Given:
      LOGICAL BAD
      INTEGER EL
      BYTE ARRAY
      CHARACTER * ( * ) TYPE
      CHARACTER * ( * ) LOC

*  Arguments Returned:
      LOGICAL DCE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( 1 )           ! Object dimension array
      INTEGER PNTR               ! Pointer to mapped data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      DIM( 1 ) = EL

*  If no data type conversion is required, then write the data values
*  directly.
      IF ( TYPE .EQ. '_UBYTE' ) THEN
         DCE = .FALSE.
         CALL DAT_PUT( LOC, '_UBYTE', 1, DIM, ARRAY, STATUS )

*  If type conversion is required, then map the data without type
*  conversion, perform the type conversion explicitly and unmap the
*  data.
      ELSE
         CALL DAT_MAP( LOC, TYPE, 'WRITE', 1, DIM, PNTR, STATUS )
         CALL ARY1_CVFUB( BAD, EL, ARRAY, TYPE, PNTR, DCE, STATUS )
         CALL ARY1_HUNMP( LOC, STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_PT1UB',
     :STATUS )

      END
