      SUBROUTINE NDF_ISTMP( INDF, ISTMP, STATUS )
*+
*  Name:
*     NDF_ISTMP

*  Purpose:
*     Enquire if an NDF is temporary.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ISTMP( INDF, ISTMP, STATUS )

*  Description:
*     The routine returns a logical value indicating whether the
*     specified NDF is temporary. Temporary NDFs are deleted once the
*     last identifier which refers to them is annulled.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     ISTMP = LOGICAL (Returned)
*        Whether the NDF is temporary.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Obtain an index to the data object entry in the DCB.
*     -  Examine the object's disposal mode to determine if it is
*     temporary.

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

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_DSP( NDF__MXDCB ) = CHARACTER * ( NDF__SZDSP ) (Read)
*           Data object disposal mode.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      LOGICAL ISTMP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain the data object index in the DCB.
         IDCB = ACB_IDCB( IACB )

*  The object's disposal mode determines whether it is temporary.
         ISTMP = DCB_DSP( IDCB ) .EQ. 'TEMP'
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ISTMP_ERR',
     :   'NDF_ISTMP: Error enquiring if an NDF is temporary.',
     :   STATUS )
         CALL NDF1_TRACE( 'NDF_ISTMP', STATUS )
      END IF

      END
