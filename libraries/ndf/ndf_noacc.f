      SUBROUTINE NDF_NOACC( ACCESS, INDF, STATUS )
*+
*  Name:
*     NDF_NOACC

*  Purpose:
*     Disable a specified type of access to an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_NOACC( ACCESS, INDF, STATUS )

*  Description:
*     The routine disables the specified type of access to an NDF, so
*     that any subsequent attempt to access it in that way will fail.
*     Access restrictions imposed on an NDF identifier by this routine
*     will be propagated to any new identifiers derived from it, and
*     cannot be revoked.

*  Arguments:
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of access to be disabled: 'BOUNDS', 'DELETE',
*        'MODIFY', 'SHIFT', 'TYPE' or 'WRITE'.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     Disabling each type of access imposes the following restrictions
*     on an NDF:
*     -  'BOUNDS' prevents the pixel-index bounds of a base NDF from
*     being altered.
*     -  'DELETE' prevents an NDF from being deleted.
*     -  'MODIFY' prevents any form of modification to the NDF (i.e. it
*     disables all the other access types).
*     -  'SHIFT' prevents pixel-index shifts from being applied to a
*     base NDF.
*     -  'TYPE' prevents the data type of any NDF components from being
*     altered.
*     -  'WRITE' prevents new values from being written to the NDF, or
*     the state of any of its components from being reset.

*  Algorithm:
*     -  Import the NDF identifier.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ACC( NDF__MXACC, NDF_MXACB ) = LOGICAL (Write)
*           Access control flags.

*  Arguments Given:
      CHARACTER * ( * ) ACCESS
      INTEGER INDF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check for each type of access in turn and reset the appropriate
*  access control flag.

*  ...BOUNDS access; prevents the NDF's bounds from being altered.
         IF ( CHR_SIMLR( ACCESS, 'BOUNDS' ) ) THEN
            ACB_ACC( NDF__BOUND, IACB ) = .FALSE.

*  ...DELETE access; prevents the NDF being deleted.
         ELSE IF ( CHR_SIMLR( ACCESS, 'DELETE' ) ) THEN
            ACB_ACC( NDF__DELET, IACB ) = .FALSE.

*  ...MODIFY access; prevents any form of modification to the NDF.
         ELSE IF ( CHR_SIMLR( ACCESS, 'MODIFY' ) ) THEN
            ACB_ACC( NDF__BOUND, IACB ) = .FALSE.
            ACB_ACC( NDF__DELET, IACB ) = .FALSE.
            ACB_ACC( NDF__SHIFT, IACB ) = .FALSE.
            ACB_ACC( NDF__TYPE, IACB ) = .FALSE.
            ACB_ACC( NDF__WRITE, IACB ) = .FALSE.

*  ...SHIFT access; prevents pixel index shifts from being applied to
*  the NDF.
         ELSE IF ( CHR_SIMLR( ACCESS, 'SHIFT' ) ) THEN
            ACB_ACC( NDF__SHIFT, IACB ) = .FALSE.

*  ...TYPE access; prevents the NDF's data type being altered.
         ELSE IF ( CHR_SIMLR( ACCESS, 'TYPE' ) ) THEN
            ACB_ACC( NDF__TYPE, IACB ) = .FALSE.

*  ...WRITE access; inhibits the writing of new data values or the
*  resetting of the NDF components' state.
         ELSE IF ( CHR_SIMLR( ACCESS, 'WRITE' ) ) THEN
            ACB_ACC( NDF__WRITE, IACB ) = .FALSE.

*  If the access type was not recognised, then report an error.
         ELSE
            STATUS = NDF__ACCIN
            CALL MSG_SETC( 'BADACC', ACCESS )
            CALL ERR_REP( 'NDF_NOACC_BAD',
     :      'Invalid access type ''^BADACC'' specified (possible ' //
     :      'programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_NOACC_ERR',
     :   'NDF_NOACC: Error disabling a specified type of access to ' //
     :   'an NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_NOACC', STATUS )
      END IF

      END
