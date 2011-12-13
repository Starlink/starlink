      SUBROUTINE NDF1_HTLEN( IDCB, HTLEN, STATUS )
*+
*  Name:
*     NDF1_HTLEN

*  Purpose:
*     Return the length of the text in the current history record.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_HTLEN( IDCB, HTLEN, STATUS )

*  Description:
*     The routine returns the length of the text in the current history
*     record, as determined from the HDS locators stored in the DCB.

*  Arguments:
*     IDCB = INTEGER (Given)
*        DCB index identifying the NDF whose history is to be modified.
*     HTLEN = INTEGER (Returned)
*        The length of the text in the current history record.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     16-OCT-2009 (DSB):
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
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_HLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for NDF history component.
*        DCB_HNREC( NDF__MXDCB ) = INTEGER (Read)
*           Number of valid history records present.
*        DCB_HRLOC( NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Locator for array of history records.

*  Arguments Given:
      INTEGER IDCB

*  Arguments Returned:
      INTEGER HTLEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CELL! Locator for array sell
      CHARACTER * ( DAT__SZLOC ) LOC ! Temporary locator
      INTEGER SUB( 1 )           ! Array subscript

*.

*  Initialise
      HTLEN = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that history information is available in the DCB.
      CALL NDF1_DH( IDCB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Check if a history component is present, otherwise there is nothing
*  more to do.
         IF ( DCB_HLOC( IDCB ) .NE. DAT__NOLOC ) THEN

*  Get a locator to the current record.
            SUB( 1 ) = DCB_HNREC( IDCB )
            IF( SUB( 1 ) .GE. 1 ) THEN
               CALL DAT_CELL( DCB_HRLOC( IDCB ), 1, SUB, CELL, STATUS )

*  Obtain a locator to the TEXT component, and get its HDS character
*  length.
               CALL DAT_FIND( CELL, 'TEXT', LOC, STATUS )
               CALL DAT_CLEN( LOC, HTLEN, STATUS )

*  Annul the locators.
               CALL DAT_ANNUL( LOC, STATUS )
               CALL DAT_ANNUL( CELL, STATUS )
            END IF
         END IF
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_HTLEN', STATUS )

      END
