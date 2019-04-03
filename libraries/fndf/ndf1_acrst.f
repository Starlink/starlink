      SUBROUTINE NDF1_ACRST( IAX, ICCOMP, IACB, STATUS )
*+
*  Name:
*     NDF1_ACRST

*  Purpose:
*     Reset an axis character component to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_ACRST( IAX, ICCOMP, IACB, STATUS )

*  Description:
*     The routine resets an axis character component of an NDF to an
*     undefined state by erasing the associated data object.

*  Arguments:
*     IAX = INTEGER (Given)
*        Axis number.
*     ICCOMP = INTEGER (Given)
*        Axis character component identifier: NDF_ALAB or NDF_AUNI (as
*        defined in the include file NDF_CONST).
*     IACB = INTEGER (Given)
*        Index to the NDF entry in the ACB.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine can only be used to reset an axis character
*     component via a base NDF. If an NDF section is supplied, then it
*     will return without action. No error will result.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     {enter_new_authors_here}

*  History:
*     16-OCT-1990 (RFWS):
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
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACCN( NDF__MXACN ) = CHARACTER * ( DAT__SZNAM ) (Read)
*           Axis character component names.
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Read and Write)
*           Locators to axis character components.
*        DCB_ALOC( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
*        (Read)
*           Locators to axis structure elements.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_CUT( NDF__MXACB ) = LOGICAL (Read)
*           Whether an NDF is a cut (i.e. section).
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IAX
      INTEGER ICCOMP
      INTEGER IACB

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check if the NDF is a section. There is nothing to do if it is.
      IF ( .NOT. ACB_CUT( IACB ) ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that axis character component information is available in the
*  DCB.
         CALL NDF1_DAC( IAX, ICCOMP, IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the character component exists, then annul its locator and erase
*  it.
            IF ( DCB_ACLOC( IAX, ICCOMP, IDCB ) .NE. DAT__NOLOC ) THEN
               CALL DAT_ANNUL( DCB_ACLOC( IAX, ICCOMP, IDCB ), STATUS )
               CALL DAT_ERASE( DCB_ALOC( IAX, IDCB ),
     :                         DCB_ACCN( ICCOMP ), STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_ACRST', STATUS )

      END
