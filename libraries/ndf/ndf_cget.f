      SUBROUTINE NDF_CGET( INDF, COMP, VALUE, STATUS )
*+
*  Name:
*     NDF_CGET

*  Purpose:
*     Obtain the value of an NDF character component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CGET( INDF, COMP, VALUE, STATUS )

*  Description:
*     The routine obtains the value of the specified character
*     component of an NDF (i.e. the value of the LABEL, TITLE or UNITS
*     component).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the character component whose value is required:
*        'LABEL', 'TITLE' or 'UNITS'.
*     VALUE = CHARACTER * ( * ) (Given and Returned)
*        The component's value.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the requested component is in an undefined state, then the
*     VALUE argument will be returned unchanged. A suitable default
*     should therefore be established before calling this routine.
*     -  If the length of the VALUE argument is too short to
*     accommodate the returned result without losing significant
*     (non-blank) trailing characters, then this will be indicated by
*     an appended ellipsis, i.e. '...'. No error will result.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Validate the character component name.
*     -  Obtain an index to the data object entry in the DCB and ensure
*     that information about the required character component is
*     available in the DCB.
*     -  If the required component is present, then mark the error stack
*     and read its value.
*     -  If character string truncation occurred, then annul the error
*     and append ellipses to the returned value.
*     -  Release the error stack.

*  Copyright:
*     Copyright (C) 1989, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     26-SEP-1989 (RFWS):
*        Minor addition to prologue.
*     2-OCT-1989 (RFWS):
*        Corrected errors in subroutine name and positioning of
*        ellipses.
*     24-JAN-1990 (RFWS):
*        Renamed CCOMP to COMP.
*     4-DEC-1991 (RFWS):
*        Changed DAT__TRUNC to DAT__CONER to reflect changes in HDS
*        behaviour.
*     14-OCT-1992 (RFWS):
*        Re-instated test for DAT__TRUNC as well, since it may still
*        occur.
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
      INCLUDE 'DAT_ERR'          ! HDS error codes

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

*  Arguments Given and Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER ICCOMP             ! Identifier for character component
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER N                  ! Character position to start ellipses

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Validate the character component name.
      CALL NDF1_VCCN( COMP, ICCOMP, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

*  Ensure that information for the required character component is
*  available in the DCB.
         CALL NDF1_DC( IDCB, ICCOMP, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If the component is present, then mark the error stack and read its
*  value.
            IF ( DCB_CLOC( ICCOMP, IDCB ) .NE. DAT__NOLOC ) THEN
               CALL ERR_MARK
               CALL DAT_GET0C( DCB_CLOC( ICCOMP, IDCB ), VALUE, STATUS )

*  If character string truncation occurred, then annul the error and
*  append ellipses to the returned value.
               IF ( ( STATUS .EQ. DAT__CONER ) .OR.
     :              ( STATUS .EQ. DAT__TRUNC ) ) THEN
                  CALL ERR_ANNUL( STATUS )
                  N = MAX( 1, LEN( VALUE ) - 2 )
                  VALUE( N : ) = '...'
               END IF

*  Release the error stack.
               CALL ERR_RLSE
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_CGET_ERR',
     :   'NDF_CGET: Error obtaining the value of an NDF character ' //
     :   'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_CGET', STATUS )
      END IF

      END
