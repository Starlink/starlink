      SUBROUTINE NDF1_GTBB( IACB, BADBIT, STATUS )
*+
*  Name:
*     NDF1_GTBB

*  Purpose:
*     Get the effective bad-bits mask value for an ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_GTBB( IACB, BADBIT, STATUS )

*  Description:
*     The routine returns the effective bad-bits mask value to be
*     applied to the quality component of an ACB entry. It takes
*     account of any override value which may have been applied.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     BADBIT = BYTE (Returned)
*        The unsigned byte bad-bits value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  If an override bad-bits value has been set for the ACB entry,
*     then return that value.
*     -  Otherwise, ensure that quality information is available in the
*     DCB.
*     -  Return the DCB bad-bits value.

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
*     1-FEB-1990 (RFWS):
*        Original version.
*     5-APR-1990 (RFWS):
*        Fixed bug in call to NDF1_QIMP; the DCB index was being passed
*        instead of the ACB index.
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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_QBB( NDF__MXDCB ) = BYTE (Read)
*           Data object bad-bits value.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_ISQBB( NDF__MXACB ) = LOGICAL (Read)
*           Whether an override bad-bits value has been set.
*        ACB_QBB( NDF__MXACB ) = BYTE (Read)
*           Override bad-bits value.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      BYTE BADBIT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If an override bad-bits value has been set for the ACB entry, then
*  return that value.
      IF ( ACB_ISQBB( IACB ) ) THEN
         BADBIT = ACB_QBB( IACB )
      ELSE

*  Otherwise, ensure that quality information is available in the DCB
*  and ACB.
         CALL NDF1_QIMP( IACB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Return the DCB bad-bits value.
            IDCB = ACB_IDCB( IACB )
            BADBIT = DCB_QBB( IDCB )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_GTBB', STATUS )

      END
