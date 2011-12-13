      SUBROUTINE ARY1_VBAD( TYPE, N, PNTR, STATUS )
*+
*  Name:
*     ARY1_VBAD

*  Purpose:
*     Set all elements of a vectorised array to the "bad" value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_VBAD( TYPE, N, PNTR, STATUS )

*  Description:
*     The routine sets all elements of a vectorised array, of any
*     numeric data type, to the appropriate "bad" value. The array is
*     passed by pointer.

*  Arguments:
*     TYPE = CHARACTER * ( * ) (Given)
*        An HDS primitive numeric type string, specifying the data type
*        of the vectorised array (case insensitive).
*     N = INTEGER (Given)
*        Number of array elements.
*     PNTR = INTEGER (Given)
*        Pointer to the array whose elements are to be set (the pointer
*        value itself is not altered, although the array elements are).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Test the data type specified against each of the permitted
*     values in turn, calling an appropriate routine to set the array
*     elements to a "bad" value.
*     -  If the data type specified is not valid, then report an error.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JUN-1989  (RFWS):
*        Original version.
*     7-SEP-1989 (RFWS):
*        Added message token for routine name, to prevent '$' from
*        affecting error messages.
*     13-MAR-1990 (RFWS):
*        Changed to call ARY1_BADx instead of VEC_BADx.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNV_PVAL function
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) TYPE
      INTEGER N
      INTEGER PNTR

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test the data type string supplied against each permitted value in
*  turn, calling the appropriate routine to set the array elements to a
*  "bad" value.
      IF ( CHR_SIMLR( TYPE, '_BYTE' ) ) THEN
         CALL ARY1_BADB( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN
         CALL ARY1_BADUB( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         CALL ARY1_BADD( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         CALL ARY1_BADI( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         CALL ARY1_BADR( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_WORD' ) ) THEN
         CALL ARY1_BADW( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_UWORD' ) ) THEN
         CALL ARY1_BADUW( N, %VAL( CNF_PVAL( PNTR ) ), STATUS )

*  If the data type string was not valid, then report an error.
      ELSE
         STATUS = ARY__FATIN
         CALL MSG_SETC( 'ROUTINE', 'ARY1_VBAD' )
         CALL MSG_SETC( 'BADTYPE', TYPE )
         CALL ERR_REP( 'ARY1_VBAD_TYPE',
     :   'Routine ^ROUTINE called with an invalid TYPE argument of ' //
     :   '''^BADTYPE'' (internal programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_VBAD', STATUS )

      END
