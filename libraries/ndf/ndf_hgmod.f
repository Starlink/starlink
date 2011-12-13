      SUBROUTINE NDF_HGMOD( INDF, HMODE, STATUS )
*+
*  Name:
*     NDF_HGMOD

*  Purpose:
*     Get the history update mode for an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HGMOD( INDF, HMODE, STATUS )

*  Description:
*     The routine returns the current history component update mode of
*     an NDF. See NDF_HSMOD.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     HMODE = CHARACTER * ( * ) (Returned)
*        The history update mode: 'DISABLED', 'QUIET', 'NORMAL' or 'VERBOSE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - An error is reported if the NDF has no HISTORY component.

*  Copyright:
*     Copyright (C) 2008, 2009 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, UCLan)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     4-JUL-2008 (DSB):
*        Original version.
*     5-AUG-2009 (TIMJ):
*        Strip leading spaces from UPDATE_MODE. At some point two
*        spaces were being inserted.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      CHARACTER * ( * ) HMODE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index of NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB
      LOGICAL THERE              ! Does HISTORY component exist?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB and ensure that
*  DCB history information is available.
         IDCB = ACB_IDCB( IACB )
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that a history component is present and report an error if it
*  is not.
            IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOHIS
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_HGMOD_NOHIS',
     :                       'There is no history component present ' //
     :                       'in the NDF structure ^NDF (possible ' //
     :                       'programming error).', STATUS )

*  Otherwise, if the UPDATE_MODE component exists, get its value. If not,
*  use a default value of NORMAL.
            ELSE
               CALL DAT_THERE( DCB_HLOC( IDCB ), 'UPDATE_MODE', THERE,
     :                         STATUS )
               IF( THERE ) THEN
                  CALL CMP_GET0C( DCB_HLOC( IDCB ), 'UPDATE_MODE',
     :                            HMODE, STATUS )
                  CALL CHR_RMBLK( HMODE )
               ELSE
                  HMODE = 'NORMAL'
               END IF
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HGMOD_ERR',
     :   'NDF_HGMOD: Error getting the history update mode for an ' //
     :   'NDF.', STATUS )
         CALL NDF1_TRACE( 'NDF_HGMOD', STATUS )
      END IF

      END
