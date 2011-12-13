      SUBROUTINE NDF_HNREC( INDF, NREC, STATUS )
*+
*  Name:
*     NDF_HNREC

*  Purpose:
*     Determine the number of NDF history records present.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_HNREC( INDF, NREC, STATUS )

*  Description:
*     The routine returns a count of the number of history records
*     present in an NDF.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     NREC = INTEGER (Returned)
*        Number of history records.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The number of records returned may be zero if a history
*     component exists but no history information has yet been entered.
*     -  An error will result if there is no history component present
*     in the NDF.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

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
*     7-MAY-1993 (RFWS):
*        Original version.
*     2-JUN-1993 (RFWS):
*        Report an error if there is no history component present
*     {enter_further_changes_here}

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
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read)
*           Number of valid history records present.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER NREC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to the NDF entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  If OK, then obtain an index to the data object entry in the DCB.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IDCB = ACB_IDCB( IACB )

*  Ensure that history information is available in the DCB.
         CALL NDF1_DH( IDCB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If there is no history component present, then report an error.
            IF ( DCB_HLOC( IDCB ) .EQ. DAT__NOLOC ) THEN
               STATUS = NDF__NOHIS
               CALL NDF1_DMSG( 'NDF', IDCB )
               CALL ERR_REP( 'NDF_HNREC_NOHIS',
     :                       'There is no history component present ' //
     :                       'in the NDF structure ^NDF (possible ' //
     :                       'programming error).', STATUS )

*  Otherwise, return the record count from the DCB.
            ELSE
               NREC = DCB_HNREC( IDCB )
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_HNREC_ERR',
     :   'NDF_HNREC: Error determining the number of NDF history ' //
     :   'records present.', STATUS )
         CALL NDF1_TRACE( 'NDF_HNREC', STATUS )
      END IF

      END
