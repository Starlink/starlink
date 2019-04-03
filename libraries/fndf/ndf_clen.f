      SUBROUTINE NDF_CLEN( INDF, COMP, LENGTH, STATUS )
*+
*  Name:
*     NDF_CLEN

*  Purpose:
*     Determine the length of an NDF character component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CLEN( INDF, COMP, LENGTH, STATUS )

*  Description:
*     The routine returns the length of the specified character
*     component of an NDF (i.e. the number of characters in the LABEL,
*     TITLE or UNITS component).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the character component whose length is required:
*        'LABEL', 'TITLE' or 'UNITS'.
*     LENGTH = INTEGER (Returned)
*        Length of the component in characters.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The length of an NDF character component is determined by the
*     length of the VALUE string assigned to it by a previous call to
*     NDF_CPUT (note that this could include trailing blanks).
*     -  If the specified component is in an undefined state, then a
*     length of zero will be returned.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the component name.
*     -  Obtain an index to the data object entry in the DCB and ensure
*     that information about the required character component is
*     available in the DCB.
*     -  If the component is not present in the NDF, then return a
*     length of zero.
*     -  If it is present, then enquire its length.

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
*     21-SEP-1989 (RFWS):
*        Original version.
*     24-JAN-1990 (RFWS):
*        Renamed CCOMP to COMP.
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
*        DCB_CLOC( NDF__MXCCN, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC
*        ) (Read)
*           Locators to NDF character components.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      INTEGER LENGTH

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the DCB
      INTEGER ICCOMP             ! Identifier for character component
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the component name.
      CALL NDF1_VCCN( COMP, ICCOMP, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that information about the required character component is
*  available in the DCB.
         CALL NDF1_DC( IDCB, ICCOMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component is not present in the NDF, then return a length of
*  zero.
            IF ( DCB_CLOC( ICCOMP, IDCB ) .EQ. DAT__NOLOC ) THEN
               LENGTH = 0

*  If it is present, then determine its length.
            ELSE
               CALL DAT_LEN( DCB_CLOC( ICCOMP, IDCB ), LENGTH, STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_CLEN_ERR',
     :   'NDF_CLEN: Error determining the length of an NDF ' //
     :   'character component.', STATUS )
         CALL NDF1_TRACE( 'NDF_CLEN', STATUS )
      END IF

      END
