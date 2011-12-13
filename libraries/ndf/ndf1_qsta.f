      SUBROUTINE NDF1_QSTA( IACB, STATE, STATUS )
*+
*  Name:
*     NDF1_QSTA

*  Purpose:
*     Determine the state of the quality component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_QSTA( IACB, STATE, STATUS )

*  Description:
*     The routine returns a logical value indicating if the quality
*     component of an NDF is defined. The NDF is identified by its ACB
*     entry.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's ACB entry.
*     STATE = LOGICAL (Returned)
*        Whether the quality component's values are defined (.TRUE. for
*        defined, .FALSE. for undefined).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Ensure that quality information is available in the DCB and
*     ACB.
*     -  Obtain an index to the data object entry in the DCB.
*     -  See if the quality array identifier is valid. The component is
*     undefined if it is not.
*     -  If the identifier is valid, then see if the array has defined
*     values.

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
*     29-JAN-1990 (RFWS):
*        Original, derived from the NDF1_VSTA routine.
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
*        DCB_QID( NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's quality array.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER IACB

*  Arguments Returned:
      LOGICAL STATE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that quality information is available in the DCB and ACB.
      CALL NDF1_QIMP( IACB, STATUS )

*  Obtain an index to the data object entry in the DCB.
      IDCB = ACB_IDCB( IACB )

*  See if the quality array identifier is valid. If not, then the
*  component is undefined.
      CALL ARY_VALID( DCB_QID( IDCB ), STATE, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  If the identifier is valid, then see if the array has defined
*  values.
         IF ( STATE ) THEN
            CALL ARY_STATE( DCB_QID( IDCB ), STATE, STATUS )
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_QSTA', STATUS )

      END
