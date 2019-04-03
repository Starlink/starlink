      SUBROUTINE NDF1_ACCOK( IACB, ACCESS, OK, STATUS )
*+
*  Name:
*     NDF1_ACCOK

*  Purpose:
*     Determine whether a specified type of ACB access is available.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ACCOK( IACB, ACCESS, OK, STATUS )

*  Description:
*     The routine returns a logical value indicating whether the
*     specified mode of access to an NDF entry in the ACB is permitted
*     by the current setting of the ACB access control flags.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     ACCESS = CHARACTER * ( * ) (Given)
*        The type of access required (case insensitive).
*     OK = LOGICAL (Returned)
*        Whether the specified type of access is available.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     BOUNDS and SHIFT access is always permitted if the NDF is not a
*     base NDF, regardless of the state of the corresponding access
*     control flags.

*  Algorithm:
*     -  Test the specified access type against each permitted value in
*     turn and obtain the value of the associated access control flag
*     (taking account of whether the NDF is a base NDF, if
*     appropriate).
*     -  If the access type was not recognised, then report an error.

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
*     26-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     29-JAN-1990 (RFWS):
*        Changed to allow BOUNDS and SHIFT access regardless of the
*        access control flag settings if the object being accessed is
*        not a base NDF.
*     {enter_further_changes_here}

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
*        ACB_ACC( NDF__MXACC, NDF_MXACB ) = LOGICAL (Read)
*           Access control flags.
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether the NDF is a cut (i.e. NDF section).

*  Arguments Given:
      INTEGER IACB
      CHARACTER * ( * ) ACCESS

*  Arguments Returned:
      LOGICAL OK

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! Case insensitive string comparison

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Test the requested access type against each permitted value in turn
*  and obtain the value of the associated access control flag.

*  ...BOUNDS access.
      IF ( CHR_SIMLR( ACCESS, 'BOUNDS' ) ) THEN
         OK = ACB_ACC( NDF__BOUND, IACB ) .OR. ACB_CUT( IACB )

*  ...DELETE access.
      ELSE IF ( CHR_SIMLR( ACCESS, 'DELETE' ) ) THEN
         OK = ACB_ACC( NDF__DELET, IACB )

*  ...SHIFT access.
      ELSE IF ( CHR_SIMLR( ACCESS, 'SHIFT' ) ) THEN
         OK = ACB_ACC( NDF__SHIFT, IACB ) .OR. ACB_CUT( IACB )

*  ...TYPE access.
      ELSE IF ( CHR_SIMLR( ACCESS, 'TYPE' ) ) THEN
         OK = ACB_ACC( NDF__TYPE, IACB )

*  ...WRITE access.
      ELSE IF ( CHR_SIMLR( ACCESS, 'WRITE' ) ) THEN
         OK = ACB_ACC( NDF__WRITE, IACB )

*  If the access type was not recognised, then report an error.
      ELSE
         STATUS = NDF__ACCIN
         CALL MSG_SETC( 'BADACC', ACCESS )
         CALL ERR_REP( 'NDF1_ACCOK_BAD',
     :   'Invalid access type ''^BADACC'' specified (possible ' //
     :   'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ACCOK', STATUS )

      END
