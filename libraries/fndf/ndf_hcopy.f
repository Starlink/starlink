      SUBROUTINE NDF_HCOPY( INDF1, INDF2, STATUS )
*+
*  Name:
*     NDF_HCOPY

*  Purpose:
*     Copy history information from one NDF to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HCOPY( INDF1, INDF2, STATUS )

*  Description:
*     The routine copies history information from one NDF to another,
*     replacing any that already exists in the destination NDF.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        Identifier for the NDF (or NDF section) containing the history
*        information to be copied.
*     INDF2 = INTEGER (Given)
*        Identifier for the NDF to receive the copied history information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - This routine returns without action leaving the destination NDF
*     unchanged if no History component exists in the input NDF.
*     - If the input NDF contains a History component, then a History
*     component is added to the destination NDF automatically, if one
*     does not already exist.

*  Copyright:
*     Copyright (C) 2014 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2014 (DSB):
*        Original version.
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
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB1              ! Index to the 1st NDF entry in the ACB
      INTEGER IACB2              ! Index to the 2nd NDF entry in the ACB
      INTEGER IDCB1              ! DCB entry index for 1st data object
      INTEGER IDCB2              ! DCB entry index for 2nd data object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the two identifiers.
      CALL NDF1_IMPID( INDF1, IACB1, STATUS )
      CALL NDF1_IMPID( INDF2, IACB2, STATUS )

*  Obtain indices to the DCB entries of the two data object.
      IDCB1 = ACB_IDCB( IACB1 )
      IDCB2 = ACB_IDCB( IACB2 )

*  Ensure history structure information is available for the input
*  NDF.
      CALL NDF1_DH( IDCB1, STATUS )

*  Use the component locator to determine the state of the History
*  component in the input NDF. Only proceed if it is defined.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF( DCB_HLOC( IDCB1 ) .NE. DAT__NOLOC ) THEN

*  Reset any pre-existing History component in the destination NDF.
            CALL NDF1_RST( IACB2, 'HISTORY', STATUS )

*  Copy the History component from input to output.
            CALL NDF1_HPRP( IDCB1, .TRUE., IDCB2, STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HCOPY_ERR',
     :   'NDF_HCOPY: Error copying history information from one ' //
     :   'NDF to another.', STATUS )
         CALL NDF1_TRACE( 'NDF_HCOPY', STATUS )
      END IF

      END
