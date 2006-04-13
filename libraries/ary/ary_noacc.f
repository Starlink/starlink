      SUBROUTINE ARY_NOACC( ACCESS, IARY, STATUS )
*+
*  Name:
*     ARY_NOACC

*  Purpose:
*     Disable a specified type of access to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_NOACC( ACCESS, IARY, STATUS )

*  Description:
*     The routine disables the specified type of access to an array, so
*     that any subsequent attempt to access it in that way will fail.
*     Access restrictions imposed on an array identifier by this
*     routine will be propagated to any new identifiers derived from
*     it, and cannot be revoked.

*  Arguments:
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of access to be disabled: 'BOUNDS', 'DELETE',
*        'MODIFY', 'SHIFT', 'TYPE' or 'WRITE'.
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Disabling each type of access imposes the following restrictions
*     on an array:
*     -  'BOUNDS' prevents the pixel-index bounds of a base array from
*     being altered.
*     -  'DELETE' prevents the array being deleted.
*     -  'MODIFY' prevents any form of modification to the array (i.e.
*     it disables all the other access types).
*     -  'SHIFT' prevents pixel-index shifts from being applied to a
*     base array.
*     -  'TYPE' prevents the data type of the array from being altered.
*     -  'WRITE' prevents new values from being written to the array,
*     or the array's state from being reset.

*  Algorithm:
*     -  Import the array identifier.
*     -  Test the access type specified against each valid value in turn
*     and reset the appropriate access control flag(s) in the ACB.
*     -  If the access type is not recognised, then report an error.

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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-SEP-1989 (RFWS):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_ACC( ARY__MXACC, ARY_MXACB ) = LOGICAL (Write)
*           Access control flags.

*  Arguments Given:
      CHARACTER * ( * ) ACCESS
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check for each type of access in turn and reset the appropriate
*  access control flag.

*  ...BOUNDS access; prevents the array's bounds from being altered.
         IF ( CHR_SIMLR( ACCESS, 'BOUNDS' ) ) THEN
            ACB_ACC( ARY__BOUND, IACB ) = .FALSE.
      
*  ...DELETE access; prevents the array being deleted.
         ELSE IF ( CHR_SIMLR( ACCESS, 'DELETE' ) ) THEN
            ACB_ACC( ARY__DELET, IACB ) = .FALSE.

*  ...MODIFY access; prevents any form of modification to the array.
         ELSE IF ( CHR_SIMLR( ACCESS, 'MODIFY' ) ) THEN
            ACB_ACC( ARY__BOUND, IACB ) = .FALSE.
            ACB_ACC( ARY__DELET, IACB ) = .FALSE.
            ACB_ACC( ARY__SHIFT, IACB ) = .FALSE.
            ACB_ACC( ARY__TYPE, IACB ) = .FALSE.
            ACB_ACC( ARY__WRITE, IACB ) = .FALSE.
            
*  ...SHIFT access; prevents pixel index shifts from being applied to
*  the array.
         ELSE IF ( CHR_SIMLR( ACCESS, 'SHIFT' ) ) THEN
            ACB_ACC( ARY__SHIFT, IACB ) = .FALSE.

*  ...TYPE access; prevents the array's data type being altered.
         ELSE IF ( CHR_SIMLR( ACCESS, 'TYPE' ) ) THEN
            ACB_ACC( ARY__TYPE, IACB ) = .FALSE.

*  ...WRITE access; inhibits the writing of new data values or the
*  resetting of the array's state.
         ELSE IF ( CHR_SIMLR( ACCESS, 'WRITE' ) ) THEN
            ACB_ACC( ARY__WRITE, IACB ) = .FALSE.

*  If the access type was not recognised, then report an error.
         ELSE
            STATUS = ARY__ACCIN
            CALL MSG_SETC( 'BADACC', ACCESS )
            CALL ERR_REP( 'ARY_NOACC_BAD',
     :      'Invalid access type ''^BADACC'' specified (possible ' //
     :      'programming error).', STATUS )
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_NOACC_ERR',
     :   'ARY_NOACC: Error disabling access to an array.', STATUS )
         CALL ARY1_TRACE( 'ARY_NOACC', STATUS )
      END IF

      END
