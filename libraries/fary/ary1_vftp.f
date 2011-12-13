      SUBROUTINE ARY1_VFTP( FTYPE, TYPE, CMPLX, STATUS )
*+
*  Name:
*     ARY1_VFTP

*  Purpose:
*     Check a full type specification for validity.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_VFTP( FTYPE, TYPE, CMPLX, STATUS )

*  Description:
*     The routine checks that a full type specification is valid and
*     decomposes it into a primitive numeric type string and a logical
*     flag indicating if the full type is complex or not. An error is
*     reported if the full type specification is not valid.

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The full type specification to be validated (case
*        insensitive).
*     TYPE = CHARACTER * ( * ) (Returned)
*        The primitive numeric type implied by FTYPE.
*     CMPLX = LOGICAL (Returned)
*        Whether FTYPE specifies complex data or not.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  To be valid, a full type specification must either be a
*     primitive numeric HDS type string, or one of these strings
*     prefixed with the string 'COMPLEX'.

*  Algorithm:
*     -  Test the full type specification against each of the permitted
*     values in turn, returning appropriate TYPE and CMPLX values.
*     -  If the specification does not match any of the permitted forms,
*     then report an error.

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
*     8-JUN-1989  (RFWS):
*        Original version.
*     22-AUG-1989 (RFWS):
*        Corrected error in ARY1_CCPY argument lists. Also added more
*        comments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Arguments Given:
      CHARACTER * ( * ) FTYPE

*  Arguments Returned:
      CHARACTER * ( * ) TYPE
      LOGICAL CMPLX

*  Status:
      INTEGER STATUS             ! Global status

*  External references:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test the full type specification against all the permitted values,
*  assigning the results accordingly.

*  ...byte data types.
      IF ( CHR_SIMLR( FTYPE, '_BYTE' ) ) THEN
         CALL ARY1_CCPY( '_BYTE', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_BYTE' ) ) THEN
         CALL ARY1_CCPY( '_BYTE', TYPE, STATUS )
         CMPLX = .TRUE.

*  ...unsigned byte data types.
      ELSE IF ( CHR_SIMLR( FTYPE, '_UBYTE' ) ) THEN
         CALL ARY1_CCPY( '_UBYTE', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_UBYTE' ) ) THEN
         CALL ARY1_CCPY( '_UBYTE', TYPE, STATUS )
         CMPLX = .TRUE.

*  ...double precision data types.
      ELSE IF ( CHR_SIMLR( FTYPE, '_DOUBLE' ) ) THEN
         CALL ARY1_CCPY( '_DOUBLE', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_DOUBLE' ) ) THEN
         CALL ARY1_CCPY( '_DOUBLE', TYPE, STATUS )
         CMPLX = .TRUE.

*  ...integer data types.
      ELSE IF ( CHR_SIMLR( FTYPE, '_INTEGER' ) ) THEN
         CALL ARY1_CCPY( '_INTEGER', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_INTEGER' ) ) THEN
         CALL ARY1_CCPY( '_INTEGER', TYPE, STATUS )
         CMPLX = .TRUE.

*  ...real data types.
      ELSE IF ( CHR_SIMLR( FTYPE, '_REAL' ) ) THEN
         CALL ARY1_CCPY( '_REAL', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_REAL' ) ) THEN
         CALL ARY1_CCPY( '_REAL', TYPE, STATUS )
         CMPLX = .TRUE.

*  ...word data types.
      ELSE IF ( CHR_SIMLR( FTYPE, '_WORD' ) ) THEN
         CALL ARY1_CCPY( '_WORD', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_WORD' ) ) THEN
         CALL ARY1_CCPY( '_WORD', TYPE, STATUS )
         CMPLX = .TRUE.

*  ...unsigned word data types.
      ELSE IF ( CHR_SIMLR( FTYPE, '_UWORD' ) ) THEN
         CALL ARY1_CCPY( '_UWORD', TYPE, STATUS )
         CMPLX = .FALSE.
      ELSE IF ( CHR_SIMLR( FTYPE, 'COMPLEX_UWORD' ) ) THEN
         CALL ARY1_CCPY( '_UWORD', TYPE, STATUS )
         CMPLX = .TRUE.

*  If the full type specification was not recognised, then report an
*  error.
      ELSE
         STATUS = ARY__FTPIN
         CALL MSG_SETC( 'BADFTYPE', FTYPE )
         CALL ERR_REP( 'ARY1_VFTP_BAD',
     :   'Invalid full array type ''^BADFTYPE'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL ARY1_TRACE( 'ARY1_VFTP', STATUS )

      END
